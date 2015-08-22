module Main where
import System.Environment
import Simple


main :: IO ()
main = do
{
	putStrLn "Input first number";
    number1  <-getLine;
	putStrLn(readExpr number1);
	
	
}