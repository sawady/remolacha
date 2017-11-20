module Compiler where

import AST
import Generator
import qualified Data.Map.Strict as M
import qualified Data.Set as S

type MethodInfo = String
type ClassInfo = (String, Int, [MethodInfo])
type ClassesInfo = M.Map String ClassInfo
type SelectorsTable = M.Map String String

primitiveClasses :: [ClassInfo]
--                  cls0                               cls1
primitiveClasses = [("Int", 1, ["print/0", "add/1"]), ("String", 1, ["print/0", "add/1"])]

primitiveSelectors :: [String]
--                    sel0       sel1
primitiveSelectors = ["print/0", "add/1"]

compileWith :: ClassesInfo -> String
compileWith classesInfo = unlines $ 
    compilePrelude ++ compileClasses classesInfo ++ compileMethods classesInfo

compilePrelude :: [String]
compilePrelude = [
        "typedef unsigned long long int Num;",
        "typedef char* String;",
        "typedef void* PTR;",
        "",
        "struct Clase {",
        "  PTR * metodos ;",
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
        "#define OBJECT_TO_PTR(O) ((PTR)(O)) /* Convierte Objeto * -> PTR */",
        "#define PTR_TO_OBJECT(P) ((Objeto*)(P)) /* Convierte PTR -> Objeto * */",
        ""
    ] ++ compilePrimitivesClasses

compilePrimitivesClasses :: [String]
compilePrimitivesClasses = [
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

compileClasses :: ClassesInfo -> [String]
compileClasses m = compileClasses' $ filter (\(c,_) -> not $ elem c primitives) (M.assocs m)
    where compileClasses' = foldr (\(c1, (c2, n, _)) r -> compileClass c1 c2 n ++ r) []
          primitives = map (\(c, _, _) -> c) primitiveClasses

compileClass :: String -> String -> Int -> [String]
compileClass origCls cls n = [
        "/* Construye un objeto de la clase " ++ origCls ++ " */",
        "Objeto* constructor_" ++ cls ++ "() {",
        "  Objeto* obj = new Objeto;",
        "  obj->clase = " ++ cls ++ "; /* " ++ origCls ++ " */"
    ] ++ compileLocals n ++ ["  return obj", "}", ""] 

compileLocals :: Int -> [String]
compileLocals n = 
    ("  obj->varsInstancia = new PTR[" ++ show n ++ "];") : 
        map (\i -> "  obj->varsInstancia[" ++ show i ++ "] = constructor_cls0(0);") [0..n-1]

compileMethods :: ClassesInfo -> [String]
compileMethods m = undefined

collectClasses :: Program -> [ClassInfo]
collectClasses classes = map (\(Class n locals methods) -> (n, length locals, collectMethods methods)) classes

classesTable :: Program -> ClassesInfo
classesTable classes = 
    foldr (\((s, nlocals, methods), i) m -> M.insert s ("cls" ++ show i, nlocals, methods) m)
        M.empty 
        (zip (primitiveClasses ++ (collectClasses classes)) [0..])

collectSelectorsSet :: Program -> S.Set String
collectSelectorsSet classes  
    | checkForError classes = error "Some error occurs"
    | otherwise = foldr (\x s -> S.insert x s) S.empty $ concat $ map collectOnClasses classes
      where collectOnClasses (Class _ _ methods) = collectMethods methods

checkForError classes 
    | (hasDupl $ getClass classes)                   = error "duplicate class"
    | (any hasDupl $ getLocals classes)              = error "duplicate variable name"
    | (any hasDupl $ getMethods classes)             = error "duplicate method"
    | otherwise                                      = error "undefined error"


hasDupl ls   = not $ allDifferent ls
allDifferent :: Eq a => [a] -> Bool
allDifferent []       = True
allDifferent (x:xs)   = x `notElem` xs && allDifferent xs
getClass   = map (\(Class name locals methods) -> name)
getLocals  = map (\(Class name locals methods) -> locals)
getMethods = map (\(Class name locals methods) -> map getName methods)
getName (Method name params _) = (name, length params)

collectMethods :: [Method] -> [String]
collectMethods methods = map collectMethod methods

collectMethod :: Method -> String
collectMethod (Method n params block) = n ++ "/" ++ show (length params)              

selectorsTable :: Program -> SelectorsTable
selectorsTable classes = 
    foldr (\(s, i) m -> M.insert s ("sel" ++ show i) m)
          M.empty
          (zip (primitiveSelectors ++ S.elems (collectSelectorsSet classes)) [0..])

fst (a,_,_) = a
snd (_,b,_) = b
thd (_,_,c) = c

type File = String 

compile :: IO ()
compile = do
    grammar <- readFile "remolacha.ll"
    input   <- readFile "example.rm"
    let program = toProgram $ parseTermino grammar input
    let sTable = selectorsTable program 
    mapM_ print $ M.assocs sTable
    mapM_ print $ M.assocs (classesTable program)
    putStrLn ""
    putStrLn $ compileWith (classesTable program)

compile2 :: IO ()
compile2 = do
    grammar <- readFile "remolacha.ll"
    input   <- readFile "test1.rm"
    let program = toProgram $ parseTermino grammar input
    let sTable = selectorsTable program 
    mapM_ print $ M.assocs sTable
    mapM_ print $ M.assocs (classesTable program)
    putStrLn ""
    putStrLn $ compileWith (classesTable program)