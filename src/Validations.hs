module Validations where

import Data.List (intersect)
import Generator
import AST

checkForError classes 
    | (hasDupl $ getClass classes)                                              = Left "duplicate class"
    | (any hasDupl $ getLocals classes)                                         = Left "duplicate variable name"
    | (any hasDupl $ getMethodsNames classes)                                   = Left "duplicate method"
    | any id (map (any hasDupl) $ getMethodsParams classes)                     = Left "duplicate method param name"
    | any id (compareOnebyOne (getLocals classes) (getMethodsParams classes))   = Left "instance variable and param name cannot be equals"
    | not $ allClassesInScope classes                                           = Left "variable not initialized"
    | not $ allHasExistingClass (getClass classes) (collectAllNew classes)      = Left "not existing class"
    | otherwise                                                                 = Right "All Validations pass"


hasDupl ls   = not $ allDifferent ls
allDifferent :: Eq a => [a] -> Bool
allDifferent []       = True
allDifferent (x:xs)   = x `notElem` xs && allDifferent xs

compareOnebyOne :: [[String]] -> [[[String]]] -> [Bool]
compareOnebyOne [] _          = []
compareOnebyOne (x:xs) (y:ys) = (any (compareList x) y) : compareOnebyOne xs ys

allClassesInScope :: [Class] -> Bool
allClassesInScope []                              = True
allClassesInScope ((Class _ locals methods) : xs) = (allInScope locals methods) && (allClassesInScope xs)

getClass                            = map (\(Class name locals methods) -> name)
getLocals                           = map (\(Class name locals methods) -> locals)
getExps                             = concat . map (\(Class name locals methods) -> getExps' methods)
getMethodsNames                     = map (\(Class name locals methods) -> map getName methods)
getMethodsParams                    = map (\(Class name locals methods) -> map getMethodParams methods)
getName (Method name params _)      = (name, length params)
getMethodParams (Method _ params _) = params 
compareList ls                      = not . null . intersect ls
getExps' methods                    = concat $ map (\(Method _ _ exps) -> exps) methods

allInScope locals methods                  = all (isInScope locals) methods
isInScope locals (Method _ variables exps) = all (`elem` (locals ++ variables)) (concat $ map collectAssigns exps)  

allHasExistingClass classesNames allNewNames = all (`elem` classesNames) allNewNames

collectAllNew classes = concat $ map getNews (getExps classes)


getNews (New name)         = [name]
getNews (Assign var exp)   = getNews exp
getNews (Send _ exprs exp) = getNews exp ++ concat (map getNews exprs)
getNews _                  = []

collectAssigns (Assign var exp)   = var : collectAssigns exp
collectAssigns (Send _ exprs exp) = collectAssigns exp ++ concat (map collectAssigns exprs)
collectAssigns _                  = []

getMsg (Left m)  = m
getMsg (Right m) = m

testValidations :: IO ()
testValidations = do
    grammar <- readFile "remolacha.ll"
    input   <- readFile "test2.rm"
    let program = toProgram $ parseTermino grammar input
    putStrLn (getMsg $ checkForError program)