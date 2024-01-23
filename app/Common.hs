module Common where
import Data.Text


type Name = Text
type Value = Int
type BinOp = Text

type Label = Text


(<.>):: Label -> Int -> Label
l <.> i = l <> (pack . show $ i)
