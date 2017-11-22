module Validations where

import Data.List (intersect)
import Generator
import AST

checkForError classes 
    | (hasDupl $ getClass classes)                                              = error "duplicate class"
    | (any hasDupl $ getLocals classes)                                         = error "duplicate variable name"
    | (any hasDupl $ getMethods classes)                                        = error "duplicate method"
    | any id (map (any hasDupl) $ getMethodsParams classes)                     = error "duplicate method param name"
    | any id (compareOnebyOne (getLocals classes) (getMethodsParams classes))   = error "instance variable and param name cannot be equals"
    | not $ allClassesInScope classes                                           = error "variable not initialized"
    | otherwise                                                                 = "All Validations pass"


hasDupl ls   = not $ allDifferent ls
allDifferent :: Eq a => [a] -> Bool
allDifferent []       = True
allDifferent (x:xs)   = x `notElem` xs && allDifferent xs

compareOnebyOne :: [[String]] -> [[[String]]] -> [Bool]
compareOnebyOne [] _          = []
compareOnebyOne (x:xs) (y:ys) = (any (compareList x) y) : compareOnebyOne xs ys

allClassesInScope :: [Class] -> Bool
allClassesInScope []          = True
allClassesInScope ((Class _ locals methods) : xs) = (allInScope locals methods) && (allClassesInScope xs)

getClass         = map (\(Class name locals methods) -> name)
getLocals        = map (\(Class name locals methods) -> locals)
getMethods       = map (\(Class name locals methods) -> map getName methods)
getMethodsParams = map (\(Class name locals methods) -> map getMethodParams methods)
getName (Method name params _) = (name, length params)
getMethodParams (Method _ params _) = params 
compareList ls = not . null . intersect ls

allInScope locals methods = all (isInScope locals) methods
isInScope locals (Method _ variables exps) = all (`elem` (locals ++ variables)) (concat $ map collectAssigns exps)  

collectAssigns (Assign var exp)   = var : collectAssigns exp
collectAssigns (Send _ exprs exp) = collectAssigns exp ++ concat (map collectAssigns exprs)
collectAssigns _                  = []

--[varName | x@(Assign varName _) <- exps]



testValidations :: IO ()
testValidations = do
    grammar <- readFile "remolacha.ll"
    input   <- readFile "test2.rm"
    let program = toProgram $ parseTermino grammar input
    putStrLn (checkForError program)