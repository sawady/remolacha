module Compiler where

import AST
import Generator

parseRemolacha :: IO ()
parseRemolacha = do
    grammar <- readFile "remolacha.ll"
    input   <- readFile "example.rm"
    print $ toProgram $ parseTermino grammar input