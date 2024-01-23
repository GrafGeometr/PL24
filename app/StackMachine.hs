{-# LANGUAGE OverloadedStrings #-}
module StackMachine where
import Common
import Data.Aeson hiding (Value)
import Data.Text


data StackCommand
    = StackBinOp BinOp
    | StackConst Value
    | StackVar Name
    | Read
    | Write
    | Set Name
    | Jump Label
    | JumpZero Label
    | JumpNotZero Label
    | Label Label


instance ToJSON StackCommand where
    toJSON (StackBinOp op) = object ["kind" .= ("Binop" :: Text), "value" .= op]
    toJSON (StackConst v) = toJSON v
    toJSON (StackVar v) = toJSON v
    toJSON Read = toJSON ("READ" :: Text)
    toJSON Write = toJSON ("WRITE" :: Text)
    toJSON (Set v) = object ["kind" .= ("ST" :: Text), "value" .= v]
    toJSON (Jump label) = object ["kind" .= ("JMP" :: Text), "value" .= label]
    toJSON (JumpZero label) = object ["kind" .= ("JZ" :: Text), "value" .= label]
    toJSON (JumpNotZero label) = object ["kind" .= ("JNZ" :: Text), "value" .= label]
    toJSON (Label label) = object ["kind" .= ("LABEL" :: Text), "value" .= label]

type StackMachine = [StackCommand]