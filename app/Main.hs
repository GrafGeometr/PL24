module Main where
import Data.Aeson
import Lang
import Interpret


main :: IO ()
main = do
    -- filename <- getLine
    let filename = "input.json"
    Right program <- eitherDecodeFileStrict filename :: IO (Either String Expr)

    runProg program


