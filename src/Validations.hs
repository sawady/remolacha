module Validations where

import AST

fst (a,_,_) = a
snd (_,b,_) = b
thd (_,_,c) = c

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