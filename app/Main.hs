module Main where
import Data.Aeson
import Lang
import Compile
import Control.Monad.State (evalState)


main :: IO ()
main = do
    filename <- getLine
    Right program <- eitherDecodeFileStrict filename :: IO (Either String Program)

    print program

    let compiled = evalState (compile program) 0

    outputFilename <- getLine

    encodeFile outputFilename compiled


