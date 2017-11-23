import Validations

import Test.Hspec
import Generator
import AST

main :: IO ()
main = test

test :: IO ()
test = hspec $ do
    describe "validations" $ do
        it "duplicate class" $ do
            grammar <- readFile "remolacha.ll"
            input   <- readFile "test/input/duplicate_class.rm"
            let program = toProgram $ parseTermino grammar input
            checkForError program `shouldBe` (Left "duplicate class")


        it "duplicate variable name" $ do
            grammar <- readFile "remolacha.ll"
            input   <- readFile "test/input/duplicate_variable_name.rm"
            let program = toProgram $ parseTermino grammar input
            checkForError program `shouldBe` (Left "duplicate variable name")

        it "duplicate method" $ do
            grammar <- readFile "remolacha.ll"
            input   <- readFile "test/input/duplicate_method.rm"
            let program = toProgram $ parseTermino grammar input
            checkForError program `shouldBe` (Left "duplicate method")

        it "duplicate method param name" $ do
            grammar <- readFile "remolacha.ll"
            input   <- readFile "test/input/duplicate_method_param_name.rm"
            let program = toProgram $ parseTermino grammar input
            checkForError program `shouldBe` (Left "duplicate method param name")            