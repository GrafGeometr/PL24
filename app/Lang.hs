{-# LANGUAGE OverloadedStrings #-}
module Lang where


import Common
import Data.Aeson hiding (Value)
import GHC.Generics
import Data.Text
import Control.Applicative


data Expr
    = BinOp { name :: BinOp, left :: Expr, right :: Expr }
    | Const { value :: Value }
    | Var { name :: Name }
    deriving (Eq, Show, Generic)

data Stmt
    = Skip
    | Assn { lvalue :: Name, rvalue :: Expr }
    | If { cond :: Expr, thenBranch :: Program, elseBranch :: Program }
    | While { cond :: Expr, body :: Program }
    | Read { nameToRead :: Name }
    | Write { valueToWrite :: Expr }
    deriving (Eq, Show, Generic)


newtype Program = Program [Stmt] deriving (Eq, Show, Semigroup)


exprOptions :: Options
exprOptions = defaultOptions
    { sumEncoding = TaggedObject
        { tagFieldName = "kind"
        , contentsFieldName = undefined
        }
    , constructorTagModifier =
        let f "BinOp" = "op"
            f s = s
        in  f
    }


instance FromJSON Expr where
    parseJSON = genericParseJSON exprOptions


stmtOptions :: Options
stmtOptions = defaultOptions
    { sumEncoding = TaggedObject
        { tagFieldName = "kind"
        , contentsFieldName = undefined
        }
    , constructorTagModifier =
        let f "If" = "if"
            f s = s
        in  f
    , fieldLabelModifier =
        let f "thenBranch" = "then"
            f "elseBranch" = "else"
            f "nameToRead" = "name"
            f "valueToWrite" = "value"
            f s = s
        in  f
    }


instance FromJSON Stmt where
    parseJSON = genericParseJSON stmtOptions


instance FromJSON Program where
    parseJSON v =
        withObject "Program" (\o -> do
            "Seq" :: Text <- o .: "kind"
            left <- o .: "left"
            right <- o .: "right"
            return $ left <> right) v <|> Program . pure <$> parseJSON v
