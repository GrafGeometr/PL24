module Main where
import Data.Aeson
import Lang
import Compile
    

main :: IO ()
main = do
    filename <- getLine
    Right program <- eitherDecodeFileStrict filename :: IO (Either String Program)

    let compiled = compile program

    outputFilename <- getLine

    encodeFile outputFilename compiled


