{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Monad(when)
import System.Console.CmdArgs
import System.Directory
import System.Environment
import System.Exit
import System.IO
import System.Process

import Compiler

data RemolachaArgs = RemolachaArgs {
    file :: String,
    output :: String
 } deriving (Show, Data, Typeable)

llecaConsole = RemolachaArgs {
    file        = def &= help "Input file" &= typ "[input]" &= name "f",
    output      = def &= help "Output file" &= typ "[input]" &= name "o"
 }

main :: IO ()
main = do
    args <- (cmdArgs $ llecaConsole
        &= help "The Remolacha compiler"
        &= program "Remolacha"
        &= summary "Remolacha v1.0.0.0"
     )

    dir <- getCurrentDirectory

    inputFile <- (canonicalizePath (file args))
    existsGrammar <- doesFileExist (inputFile)
    when (not existsGrammar) (die ("The given input file " ++ (file args) ++ " does not exist"))

    outputFile <- (canonicalizePath (output args))

    fileContents <- readFile inputFile

    putStrLn ""

    -- compile