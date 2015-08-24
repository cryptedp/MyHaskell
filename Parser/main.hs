module Main where
import System.Environment
import Simple


main :: IO ()
main = getArgs >>= print . eval . readExpr . head