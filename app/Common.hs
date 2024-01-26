module Common where
import Data.Text
import qualified Data.Map as Map
import Data.Bifunctor


type Name = Text
type Value = Int
type BinOp = Text

toInt :: (Value -> Value -> Bool) -> Value -> Value -> Value
toInt f x y = if f x y then 1 else 0

binOps :: Map.Map BinOp (Value -> Value -> Value)
binOps = Map.fromList $ first pack <$>
    [ ("+" , (+))
    , ("-" , (-))
    , ("*" , (*))
    , ("/" , div)
    , ("%" , mod)
    , ("<=", toInt (<=))
    , ("<" , toInt (<))
    , (">=", toInt (>=))
    , (">" , toInt (>))
    , ("==" , toInt (==))
    , ("<>" , toInt (/=))
    , ("&" , (*))
    , ("|" , max)
    ]

runOp :: BinOp -> Value -> Value -> Value
runOp op = binOps Map.! op
