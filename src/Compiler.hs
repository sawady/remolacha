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
type ClassInfo = (String, Int, [MethodInfo])
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
    compileClasses classesInfo
    compileMethods classesInfo sTable

compilePrelude :: Output
compilePrelude = mapM_ out [
        "#include <iostream>",
        "using namespace std;",
        "",
        "typedef unsigned long long int Num;",
        "typedef string String;",
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

primitiveClassesInfo :: [ClassInfo]
primitiveClassesInfo = [
        -- cls0
        ("Int", 1, [primInfo "print/0", primInfo "add/1"]), 
        -- cls1
        ("String", 1, [primInfo "print/0", primInfo "add/1"])
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
        "Objeto* met_cls0_sel1(Objeto* o0, Objecto* o1) {",
        "  NUM n1 = PTR_TO_NUM(o0->varsInstancia[0]);",
        "  NUM n2 = PTR_TO_NUM(o1->varsInstancia[0]);",
        "  return constructor_cls1(n1 + n2);",
        "}",
        "",
        "/* cls0 => String , sel0 => print/0 */",
        "Objeto* met_cls1_sel0(Objeto* o0) {",
        "  cout << PTR_TO_STRING(o0->varsInstancia[0]) << endl;",
        "  return o0;",
        "}",
        "",
        "/* cls1 => String , sel1 => add/1 */",
        "Objeto* met_cls1_sel1(Objeto* o0, Objecto* o1) {",
        "  String s1 = PTR_TO_STRING(o0->varsInstancia[0]);",
        "  String s2 = PTR_TO_STRING(o1->varsInstancia[0]);",
        "  return constructor_cls1(s1 + s2);",
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
                foldr (\(c1, (c2, n, _)) r -> 
                    do
                        compileClass c1 c2 n
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
        out $ "  return obj"
        out $ "}"
        out ""

compileLocals :: Int -> Output
compileLocals n =
    do
        out $ "  obj->varsInstancia = new PTR[" ++ show n ++ "];"
        mapM_ (\i -> out $ "  obj->varsInstancia[" ++ show i ++ "] = constructor_cls0(0);") [0..n-1]

compileMethods :: ClassesInfo -> SelectorsTable -> Output
compileMethods cTable sTable = do
    compilePrimitiveMethods
    foldr (\(cName, (cls, _, methods)) r -> 
            do
                compileClassMethods sTable cName cls methods
                r
        ) 
        (out "") 
        (nonPrimitiveClasses cTable)

compileClassMethods :: SelectorsTable -> String -> String -> [MethodInfo] -> Output
compileClassMethods sTable cName cls methods = 
    mapM_ (\m -> compileClassMethod sTable cName cls m) methods

compileClassMethod :: SelectorsTable -> String -> String -> MethodInfo -> Output
compileClassMethod sTable cName cls (m, params, block) =
    do
        out $ "/* " ++ cls ++ " => " ++ cName ++ " , " ++ sel ++ " => " ++ m ++ " */"
        out $ "Objeto* met_" ++ cls ++ "_" ++ sel ++ "(" ++ mParams ++ ") {"
        compileBlock params block
        out $ "}"
        out $ ""
    where sel = fromJust $ lookup m sTable
          mParams = concat $ intersperse ", " $ "Objeto* o0" : map (\(_, p) -> "Objeto* " ++ p) params

compileBlock :: ParamInfo -> [Exp] -> Output
compileBlock params block = mapM_ (compileExp params) block

compileExp :: ParamInfo -> Exp -> Output
compileExp params (ValueNum n)    = out $ "constructor_cls0(" ++ show n ++ ")"
compileExp params (ValueString s) = out $ "constructor_cls1(\"" ++ s ++ "\")"
compileExp params Self            = out "o0"
compileExp params (New c)         = out $ "new " ++ c
compileExp params (Var x)         = out x -- reemplaza x por parametro o variable de instancia
compileExp params (Assign s e)    = out $ s ++ " = 0" -- usar una funcion especial para asignar
compileExp params (Send s p e)    = out $ "send" 

collectClasses :: Program -> [ClassInfo]
collectClasses classes = map (\(Class n locals methods) -> (n, length locals, collectMethods methods)) classes

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

selectorsTable :: Program -> SelectorsTable
selectorsTable classes = 
    foldr (\(s, i) m -> (s, ("sel" ++ show i)) : m)
          []
          (zip (primitiveSelectors ++ S.elems (collectSelectorsSet classes)) [0..])

collectSelectorsSet :: Program -> S.Set String
collectSelectorsSet classes  = foldr (\x s -> S.insert x s) S.empty $ concat $ map collectOnClasses classes
    where collectOnClasses (Class _ _ methods) = concat $ map collectOnMethods methods
          collectOnMethods (Method n params block) = (n ++ "/" ++ show (length params)) : collectOnBlock block
          collectOnBlock block = concat $ map collectOnExp block
          collectOnExp (Assign _ e) = collectOnExp e
          collectOnExp (Send n params e) = (n ++ "/" ++ show (length params)) : collectOnExp e
          collectOnExp _ = []

compile :: IO ()
compile = do
    grammar <- readFile "remolacha.ll"
    input   <- readFile "example.rm"
    let program = toProgram $ parseTermino grammar input
    let sTable = selectorsTable program 
    let cTable = classesTable program
    mapM_ print $ sTable
    mapM_ print $ cTable
    putStrLn ""
    mapM_ putStrLn $ execWriter $ compileWith cTable sTable