module Compiler where

import AST
import Validations
import Generator

import Data.Maybe
import Data.List
-- import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Control.Monad.Writer.Lazy

type Output = Writer [String] ()

type BlockInfo = [Exp]
type ParamInfo = [(String, String)]
type MethodInfo = (String, ParamInfo, BlockInfo)
type ClassInfo = (String, [String], [MethodInfo])
type ClassesInfo = [(String, ClassInfo)]
type SelectorsTable = [(String, String)]

out :: String -> Output
out s = tell [s]

nonPrimitiveClasses :: ClassesInfo -> ClassesInfo
nonPrimitiveClasses = filter (\(c, _) -> not $ elem c primitiveClasses)

primitiveClasses :: [String]
primitiveClasses = map (\(c, _, _) -> c) primitiveClassesInfo

compileWith :: ClassesInfo -> SelectorsTable -> Output
compileWith classesInfo sTable = do
    compilePrelude
    compileClassesVars classesInfo
    compilePrimitivesClasses
    compileUtils
    compileClasses classesInfo
    compileMethods classesInfo sTable
    compileMain classesInfo sTable

compilePrelude :: Output
compilePrelude = mapM_ out [
        "#include <iostream>",
        "using namespace std;",
        "",
        "typedef unsigned long long int Num;",
        "typedef const char* String;",
        "typedef void* PTR;",
        "",
        "struct Clase {",
        "  PTR* metodos ;",
        "};",
        "",
        "struct Objeto {",
        "  Clase* clase;",
        "  PTR* varsInstancia;",
        "};",
        "",
        "typedef Objeto* (*Metodo)(...);",
        "",
        "#define NUM_TO_PTR(N) ((PTR)(N)) /* Convierte Num -> PTR */",
        "#define PTR_TO_NUM(P) ((Num)(P)) /* Convierte PTR -> Num */",
        "#define STRING_TO_PTR(S) ((PTR)(S)) /* Convierte String -> PTR */",
        "#define PTR_TO_STRING(P) ((String)(P)) /* Convierte PTR -> String */",
        "#define METHOD_TO_PTR(M) ((PTR)(M)) /* Convierte Metodo -> PTR */",
        "#define PTR_TO_METHOD(P) ((Metodo)(P)) /* Convierte PTR -> Metodo */",
        "#define OBJECT_TO_PTR(O) ((PTR)(O)) /* Convierte Objeto* -> PTR */",
        "#define PTR_TO_OBJECT(P) ((Objeto*)(P)) /* Convierte PTR -> Objeto* */",
        ""
    ]

compileUtils :: Output
compileUtils = mapM_ out [
        "#define DEFAULT_VALUE constructor_cls0(0)",
        "",
        "#define GET_LOCAL(i) (PTR_TO_OBJECT(o0->varsInstancia[i]))",
        "",
        "Objeto* ASSIGN_LOCAL(Objeto* o0, int i, Objeto* x) {",
        "  o0->varsInstancia[i] = OBJECT_TO_PTR(x);",
        "  return DEFAULT_VALUE;",
        "}",
        "",
        "Objeto* ASSIGN_PARAM(Objeto*& param, Objeto* x) {",
        "  param = x;",
        "  return DEFAULT_VALUE;",
        "}",
        "",
        "Metodo CALL(Objeto* r, string m, int i) {",
        "  if(r->clase->metodos[i] == NULL) {",
        "    cout << \"El objeto no acepta el mensaje \" << m << endl;",
        "  }",
        "  return PTR_TO_METHOD(r->clase->metodos[i]);",
        "}",
        ""
    ]

primitiveClassesInfo :: [ClassInfo]
primitiveClassesInfo = [
        -- cls0
        ("Int", ["valor"], [primInfo "print/0", primInfo "add/1"]), 
        -- cls1
        ("String", ["valor"], [primInfo "print/0", primInfo "add/1"])
    ]
    where primInfo n = (n, [], [])

primitiveSelectors :: [String]
primitiveSelectors = [
        -- sel0
        "print/0", 
        -- sel1
        "add/1"
    ]

compilePrimitivesClasses :: Output
compilePrimitivesClasses = mapM_ out [
        "/* Construye un objeto de clase Int */",
        "Objeto* constructor_cls0(Num valor) {",
        "  Objeto* obj = new Objeto;",
        "  obj->clase = cls0; /* Int */",
        "  obj->varsInstancia = new PTR[1];",
        "  obj->varsInstancia[0] = NUM_TO_PTR(valor);",
        "  return obj;",
        "}",
        "",
        "/* Construye un objeto de clase String */",
        "Objeto* constructor_cls1(String valor) {",
        "  Objeto* obj = new Objeto;",
        "  obj->clase = cls1; /* String */",
        "  obj->varsInstancia = new PTR[1];",
        "  obj->varsInstancia[0] = STRING_TO_PTR(valor);",
        "  return obj;",
        "}",
        ""
    ]

compilePrimitiveMethods :: Output
compilePrimitiveMethods = mapM_ out [
        "/* cls0 => Int , sel0 => print/0 */",
        "Objeto* met_cls0_sel0(Objeto* o0) {",
        "  cout << PTR_TO_NUM(o0->varsInstancia[0]) << endl;",
        "  return o0;",
        "}",
        "",
        "/* cls0 => Int , sel1 => add/1 */",
        "Objeto* met_cls0_sel1(Objeto* o0, Objeto* o1) {",
        "  Num n1 = PTR_TO_NUM(o0->varsInstancia[0]);",
        "  Num n2 = PTR_TO_NUM(o1->varsInstancia[0]);",
        "  return constructor_cls0(n1 + n2);",
        "}",
        "",
        "/* cls0 => String , sel0 => print/0 */",
        "Objeto* met_cls1_sel0(Objeto* o0) {",
        "  cout << PTR_TO_STRING(o0->varsInstancia[0]) << endl;",
        "  return o0;",
        "}",
        "",
        "/* cls1 => String , sel1 => add/1 */",
        "Objeto* met_cls1_sel1(Objeto* o0, Objeto* o1) {",
        "  String s1 = PTR_TO_STRING(o0->varsInstancia[0]);",
        "  String s2 = PTR_TO_STRING(o1->varsInstancia[0]);",
        "  string str1(s1);",
        "  string str2(s2);",
        "  const char* result = (str1 + str2).c_str();",
        "  return constructor_cls1(result);",
        "}",
        ""
    ]

compileClassesVars :: ClassesInfo -> Output
compileClassesVars cTable =
    foldr
        (\(c1, (c2, _, _)) r -> 
            do
                out $ "Clase* " ++ c2 ++ "; /* " ++ c1 ++ " */"
                r
        )
        (out "") 
        cTable

compileClasses :: ClassesInfo -> Output
compileClasses = compileClasses' . nonPrimitiveClasses
    where compileClasses' = 
                foldr (\(c1, (c2, locals, _)) r -> 
                    do
                        compileClass c1 c2 (length locals)
                        r
                ) (out "")

compileClass :: String -> String -> Int -> Output
compileClass origCls cls n = 
    do
        out $ "/* Construye un objeto de la clase " ++ origCls ++ " */"
        out $ "Objeto* constructor_" ++ cls ++ "() {"
        out $ "  Objeto* obj = new Objeto;"
        out $ "  obj->clase = " ++ cls ++ "; /* " ++ origCls ++ " */"
        compileLocals n
        out $ "  return obj;"
        out $ "}"
        out ""

compileLocals :: Int -> Output
compileLocals n =
    do
        out $ "  obj->varsInstancia = new PTR[" ++ show n ++ "];"
        mapM_ (\i -> out $ "  obj->varsInstancia[" ++ show i ++ "] = OBJECT_TO_PTR(DEFAULT_VALUE);") [0..n-1]

compileMethods :: ClassesInfo -> SelectorsTable -> Output
compileMethods cTable sTable = do
    compilePrimitiveMethods
    foldr (\(cName, (cls, _, methods)) r -> 
            do
                compileClassMethods cTable sTable cName cls methods
                r
        ) 
        (out "") 
        (nonPrimitiveClasses cTable)

compileClassMethods :: ClassesInfo -> SelectorsTable -> String -> String -> [MethodInfo] -> Output
compileClassMethods cTable sTable cName cls methods = 
    mapM_ (\m -> compileClassMethod cTable sTable cName cls m) methods

compileClassMethod :: ClassesInfo -> SelectorsTable -> String -> String -> MethodInfo -> Output
compileClassMethod cTable sTable cName cls (m, params, block) =
    do
        out $ "/* " ++ cls ++ " => " ++ cName ++ " , " ++ sel ++ " => " ++ m ++ " */"
        out $ "Objeto* met_" ++ cls ++ "_" ++ sel ++ "(" ++ mParams ++ ") {"
        compileBlock cTable sTable params locals block
        out $ "}"
        out $ ""
    where sel = fromJust $ lookup m sTable
          (_, locals, _) = fromJust $ lookup cName cTable
          mParams = concat $ intersperse ", " $ "Objeto* o0" : map (\(_, p) -> "Objeto* " ++ p) params

compileBlock :: ClassesInfo -> SelectorsTable -> ParamInfo -> [String] -> [Exp] -> Output
compileBlock cTable sTable params locals block = 
    if null block
       then out "  return DEFAULT_VALUE;"
       else mapM_ out $ 
                map (\e -> "  " ++ e ++ ";") compiledBlock'
    where compiledBlock = map (compileExp cTable sTable params locals) block
          (first, last) = splitAt (length compiledBlock - 1) compiledBlock
          rLast = map (\e -> "return " ++ e) last
          compiledBlock' = first ++ rLast

compileExp :: ClassesInfo -> SelectorsTable -> ParamInfo -> [String] -> Exp -> String
compileExp cTable sTable params locals (ValueNum n)    = "constructor_cls0(" ++ show n ++ ")"
compileExp cTable sTable params locals (ValueString s) = "constructor_cls1(\"" ++ s ++ "\")"
compileExp cTable sTable params locals Self            = "o0"

compileExp cTable sTable params locals (New c)         = "constructor_" ++ cls ++ "()"
    where (cls, _, _) = fromJust $ lookup c cTable

compileExp cTable sTable params locals (Var x)         = 
    case lookup x params of
        (Just s) -> s
        Nothing  -> "GET_LOCAL(" ++ show i ++ ")"
        where i = fromJust $ elemIndex x locals

compileExp cTable sTable params locals (Assign x e)    =
    case lookup x params of
        (Just s) -> "ASSIGN_PARAM(" ++ s ++ ", " ++ compileExp cTable sTable params locals e ++ ")"
        Nothing  -> "ASSIGN_LOCAL(o0, " ++ show i ++ ", " ++ compileExp cTable sTable params locals e ++ ")"
        where i = fromJust $ elemIndex x locals

compileExp cTable sTable params locals (Send s ps e) = "CALL(" ++ e' ++ ", " ++ "\"" ++ m ++ "\"" ++ ", " ++ show i ++ ")(" ++ ps' ++ ")"
    where m   = s ++ "/" ++ show (length ps)
          i   = fromJust $ elemIndex m (map fst sTable)
          e'  = compileExp cTable sTable params locals e
          ps' = concat $ intersperse ", " $ map (compileExp cTable sTable params locals) ps

compileMain :: ClassesInfo -> SelectorsTable -> Output
compileMain cTable sTable = do
    out $ "int main() {"
    mapM_ (`compileClassInitialization` sTable) cTable
    out $ ""
    out $ "  /* Ejecución del programa principal */"
    compileMainCall cTable sTable
    out $ ""
    out $ "  return 0;"
    out $ "}"

compileClassInitialization :: (String, ClassInfo) -> SelectorsTable -> Output
compileClassInitialization (c, (cls, _, methods)) sTable = 
    do
        out $ ""
        out $ "  /* Inicialización de la clase " ++ cls ++ " ( " ++ c ++ " ) */"
        out $ "  " ++ cls ++ " = new Clase;"
        out $ "  " ++ cls ++ "->metodos = new PTR[" ++ show (length sTable) ++ "];"
        mapM_ compileMethodsForInit (zip [0..] sTable)

    where compileMethodsForInit (i, (m, sel)) =
            do  
                out $ "  " ++ cls ++ "->metodos[" ++ show i ++ "] = " ++ (compileMethodForInit m sel) ++ ";"
          compileMethodForInit m sel =
            if elem m classMethods
               then "METHOD_TO_PTR(met_" ++ cls ++ "_" ++ sel ++ ")"
               else "NULL"
          classMethods = map (\(m, ps, _) -> m) methods

compileMainCall :: ClassesInfo -> SelectorsTable -> Output
compileMainCall cTable sTable = out $ "  CALL(constructor_" ++ cls ++ "(), " ++ "\"main/0\"" ++ ", " ++ show i ++ ")();"
    where (cls, _, _) = fromJust $ lookup "Main" cTable
          i = fromJust $ elemIndex "main/0" (map fst sTable)
 
-------------------------------------------------------------------------------------------------------------

collectClasses :: Program -> [ClassInfo]
collectClasses classes = map (\(Class n locals methods) -> (n, locals, collectMethods methods)) classes

classesTable :: Program -> ClassesInfo
classesTable classes = 
    foldr (\((s, nlocals, methods), i) m -> (s, ("cls" ++ show i, nlocals, methods)) : m)
        [] 
        (zip (primitiveClassesInfo ++ (collectClasses classes)) [0..])

collectMethods :: [Method] -> [MethodInfo]
collectMethods methods = map collectMethod methods

collectMethod :: Method -> MethodInfo
collectMethod (Method n params block) = (
        n ++ "/" ++ show (length params), 
        zip params (map (\i -> "o" ++ show i) [1..]),
        block
    )

-------------------------------------------------------------------------------------------------------------

selectorsTable :: Program -> SelectorsTable
selectorsTable classes = 
    foldr (\(s, i) m -> (s, ("sel" ++ show i)) : m)
          []
          (zip (primitiveSelectors ++ selectors) [0..])
    where selectors = filter (\x -> not $ elem x primitiveSelectors) (S.elems (collectSelectorsSet classes))

collectSelectorsSet :: Program -> S.Set String
collectSelectorsSet classes  = foldr (\x s -> S.insert x s) S.empty $ (concat $ map collectOnClasses classes)
    where collectOnClasses (Class _ _ methods) = concat $ map collectOnMethods methods
          collectOnMethods (Method n params block) = (n ++ "/" ++ show (length params)) : collectOnBlock block
          collectOnBlock block = concat $ map collectOnExp block
          collectOnExp (Assign _ e) = collectOnExp e
          collectOnExp (Send n params e) = (n ++ "/" ++ show (length params)) : collectOnExp e
          collectOnExp _ = []

-------------------------------------------------------------------------------------------------------------

compile :: IO ()
compile = do
    grammar <- readFile "remolacha.ll"
    input   <- readFile "example.rm"
    let program = toProgram $ parseTermino grammar input
    let sTable = selectorsTable program 
    let cTable = classesTable program
    -- mapM_ print $ sTable
    -- mapM_ print $ cTable
    -- putStrLn ""
    mapM_ putStrLn $ execWriter $ compileWith cTable sTable