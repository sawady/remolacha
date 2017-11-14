module Compiler where

import AST
import Generator
import qualified Data.Map.Strict as M

printPrelude :: String
printPrelude = unlines [
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
    ]

collectClasses :: Program -> [String]
collectClasses classes = map (\(Class n _ _) -> n) classes

collectSelectors :: Program -> [String]
collectSelectors classes = concat $ map collectOnClasses classes
    where collectOnClasses (Class _ _ methods) = map collectOnMethods methods
          collectOnMethods (Method n params block) = (n ++ "/" ++ show (length params))


classesTable :: Program -> M.Map String String
classesTable classes = 
    foldr (\(s, i) m -> M.insert s ("cls" ++ show i) m) 
        M.empty 
        (zip (collectClasses classes) [0..])
                    
selectorsTable :: Program -> M.Map String String
selectorsTable classes = 
    foldr (\(s, i) m -> M.insert s ("sel" ++ show i) m)
          M.empty
          (zip (collectSelectors classes) [0..])

compile :: IO ()
compile = do
    grammar <- readFile "remolacha.ll"
    input   <- readFile "example.rm"
    let program = toProgram $ parseTermino grammar input
    -- mapM_ print $ collectClasses program
    -- mapM_ print $ collectSelectors program
    mapM_ print $ M.assocs (classesTable program)
    mapM_ print $ M.assocs (selectorsTable program)