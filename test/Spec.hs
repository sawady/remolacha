import Validations

import Test.Hspec
import Generator
import AST

main :: IO ()
main = do
	test "test2.rm" (Left "variable not initialized")
	test "test1.rm" (Left "instance variable and param name cannot be equals")
	test "test3.rm" (Left "not existing class")

test :: String -> Either String String -> IO ()
test path result = hspec $ do
	describe "basic" $ do
		it "all ok" $ do
			grammar <- readFile "remolacha.ll"
			input   <- readFile path
			let program = toProgram $ parseTermino grammar input
			checkForError program `shouldBe` result