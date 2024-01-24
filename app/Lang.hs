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
    | If { cond :: Expr, thenBranch :: ProgramBody, elseBranch :: ProgramBody }
    | While { cond :: Expr, body :: ProgramBody }
    | Read { nameToRead :: Name }
    | Write { valueToWrite :: Expr }
    | Call { func :: Name, args :: [Expr] }
    deriving (Eq, Show, Generic)

data Function
    = Fun { funName :: Name, params :: [Name], funBody :: ProgramBody }
    deriving (Eq, Show, Generic)

newtype ProgramBody = ProgramBody [Stmt] deriving (Eq, Show, Semigroup)


data Program = Program [Function] ProgramBody deriving (Eq, Show)


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


funOptions :: Options
funOptions = defaultOptions
    { sumEncoding = TaggedObject
        { tagFieldName = "kind"
        , contentsFieldName = undefined
        }
    , fieldLabelModifier =
        let f "funBody" = "body"
            f "funName" = "name"
            f s = s
        in  f
    }


instance FromJSON Function where
    parseJSON = genericParseJSON funOptions


instance FromJSON Program where
    parseJSON=
        withObject "Program" (\o -> do
            fs :: [Function] <- o .: "funs"
            p <- o .: "prog"
            return $ Program fs p)


instance FromJSON ProgramBody where
    parseJSON v =
        withObject "Program" (\o -> do
            "Seq" :: Text <- o .: "kind"
            left <- o .: "left"
            right <- o .: "right"
            return $ left <> right) v <|> ProgramBody . pure <$> parseJSON v
