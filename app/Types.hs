module Types where
import Common
import qualified Data.Map as Map

data Any = Unit | IntVal Int | Fun ([Any] -> IO Any)
type Variables = Map.Map Name (IO Any)


instance Show Any where
    show (IntVal v) = show v
    show Unit = "()"
    show (Fun _) = "<function>???"
