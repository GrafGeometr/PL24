module Interpret where
import Types
import Lang
import qualified Data.Map as Map
import Common


interpret :: Variables -> Expr -> IO Any
interpret vars (BinOp op l r) = do
    IntVal x <- interpret vars l
    IntVal y <- interpret vars r
    return $ IntVal $ runOp op x y
interpret _ (Const v) = return $ IntVal v
interpret vars (Var v) = vars Map.! v
interpret vars (Lambda params body) = return $ Fun (\args -> do
    let vars' = Map.fromList $ zip params (pure <$> args)
    interpret (Map.union vars vars') body
    )
interpret vars (If c t f) = do
    IntVal c' <- interpret vars c
    if c' == 1
    then interpret vars t
    else interpret vars f
interpret vars (Call f args) = do
    Fun f' <- interpret vars f
    args' <- mapM (interpret vars) args
    f' args'
interpret _ Read = IntVal <$> readLn
interpret vars (Write v) = do
    val <- interpret vars v
    print val
    return Unit
interpret vars (Let v rhs body) = do
    let rhs' = interpret vars rhs
    interpret (Map.insert v rhs' vars) body
interpret vars (Seq l r) = do
    _ <- interpret vars l
    interpret vars r


runProg :: Expr -> IO ()
runProg expr = do
    res <- interpret Map.empty expr
    print res

