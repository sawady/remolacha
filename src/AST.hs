module AST(toProgram) where

import Terms

type Program = [Class]

data Class = Class String [Local] [Method] deriving Show

type Local = String

data Method = Method String [String] [Exp] deriving Show

data Exp = 
      ValueNum Int
    | ValueString String
    | Var String
    | Assign String Exp
    | Self
    | New String
    | Send String [Exp] Exp deriving Show

toProgram :: Termino -> Program
toProgram = collectClasses

collectClasses :: Termino -> [Class]
collectClasses (Estructura "Nil" _) = []
collectClasses (Estructura "Cons" (Estructura "DefClass" (Estructura n _ : locals : methods) : xs)) = 
    Class n (collectLocals locals) (concat $ map collectMethods methods) : (concat $ map collectClasses xs)
collectClasses _ = error "la lista de clases no esta bien formada"

collectLocals :: Termino -> [Local]
collectLocals (Estructura "Nil" _) = []
collectLocals (Estructura "Cons" (Estructura v [] : xs)) = 
    v : (concat $ map collectLocals xs)
collectLocals _ = error "la lista de variables de instancia no esta bien formada"

collectMethods :: Termino -> [Method]
collectMethods (Estructura "Nil" _) = []
collectMethods (Estructura "Cons" (Estructura "DefMethod" (Estructura n _ : params : body) : xs)) = 
    Method n (collectParams params) (concat $ map collectExpList body) : (concat $ map collectMethods xs)
collectMethods _ = error "la lista de metodos no esta bien formada"

collectParams :: Termino -> [String]
collectParams (Estructura "Nil" _) = []
collectParams (Estructura "Cons" (Estructura n [] : xs)) = 
    n : (concat $ map collectParams xs)
collectParams _ = error "la lista de parametros no esta bien formada"

collectExpList :: Termino -> [Exp]
collectExpList (Estructura "Nil" _) = []
collectExpList (Estructura "Cons" (exp : xs)) = 
    collectExp exp : (concat $ map collectExpList xs)
collectExpList _ = error "el bloque no esta bien formado"

collectExp :: Termino -> Exp
collectExp  (Estructura "Set" [Estructura n [], exp]) = Assign n (collectExp exp)
collectExp  (Estructura "Self" []) = Self
collectExp  (Estructura "New"  [Estructura n _]) = New n
collectExp  (Estructura "ConstantString" [Cadena n]) = ValueString n
collectExp  (Estructura "ConstantNumber" [Numero n]) = ValueNum n
collectExp  (Estructura "Variable" [Estructura n _]) = Var n
collectExp  (Estructura "Send" [exp, (Estructura n _), params]) = Send n (collectExpList params) (collectExp exp)
collectExp _ = error "la expresion no esta bien formada"