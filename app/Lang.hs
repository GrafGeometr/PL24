module Lang where
import Common
import GHC.Generics
import Data.Aeson hiding (Value)


data Expr
    = BinOp { name :: BinOp, left :: Expr, right :: Expr }
    | Const { value :: Value }
    | Var { name :: Name }
    | Lambda { params :: [Name], body :: Expr }
    | If { cond :: Expr, then' :: Expr, else' :: Expr }
    | Call { func :: Expr, args :: [Expr] }
    | Write { valueToWrite :: Expr }
    | Read
    | Let { name :: Name, rhs :: Expr, body :: Expr }
    | Seq { left :: Expr, right :: Expr }
    deriving (Eq, Show, Generic)


instance FromJSON Expr where
    parseJSON = genericParseJSON defaultOptions
        { sumEncoding = TaggedObject
            { tagFieldName = "kind"
            , contentsFieldName = undefined
            }
        , constructorTagModifier =
            let f "BinOp" = "Op"
                f "Lambda" = "Fun"
                f s = s
            in  f
        , fieldLabelModifier =
            let f "valueToWrite" = "value"
                f "else'" = "else"
                f "then'" = "then"
                f s = s
            in  f
        }
