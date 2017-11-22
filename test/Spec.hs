import Validations

import Test.Hspec
import Generator
import AST

main :: IO ()
main = test1

test1 :: IO ()
test1 = hspec $ do
	describe "basic" $ do
		it "all ok" $ do
			grammar <- readFile "remolacha.ll"
			input   <- readFile "test2.rm"
			let program = toProgram $ parseTermino grammar input
			checkForError program `shouldBe` Left "variable not initialized"