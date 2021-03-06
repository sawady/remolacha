import Validations

import Test.Hspec
import Generator
import AST
import Compiler

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

        it "instance variable and param name cannot be equals" $ do
            grammar <- readFile "remolacha.ll"
            input   <- readFile "test/input/instance_variable_and_param_name.rm"
            let program = toProgram $ parseTermino grammar input
            checkForError program `shouldBe` (Left "instance variable and param name cannot be equals")

        it "variable not initialized" $ do
            grammar <- readFile "remolacha.ll"
            input   <- readFile "test/input/variable_not_initialized.rm"
            let program = toProgram $ parseTermino grammar input
            checkForError program `shouldBe` (Left "variable not initialized")

        it "all validations pass" $ do
            grammar <- readFile "remolacha.ll"
            input   <- readFile "test/input/all_validation_pass.rm"
            let program = toProgram $ parseTermino grammar input
            checkForError program `shouldBe` (Right "All Validations pass")

        it "One Punch" $ do
            "Saitama sensei" `shouldBe` "Saitama sensei"

    describe "compilation test" $ do
        it "basic compilation" $ do
            grammar   <- readFile "remolacha.ll"
            input     <- readFile "example.rm"
            expected  <- readFile "test/input/test_compile.cpp"
            let program = toProgram $ parseTermino grammar input
            let (Right r) = compile grammar input
            r `shouldBe` expected

            