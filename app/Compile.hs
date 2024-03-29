{-# LANGUAGE OverloadedStrings #-}
module Compile where
import StackMachine
import Lang
import Control.Monad.State
import Common


type CompileState = State Int StackMachine


class Compile a where
    compile :: a -> CompileState


instance Compile Expr where
    compile :: Expr -> CompileState
    compile (Const v) = return . pure $ StackConst v
    compile (Var v) = return . pure $ StackVar v
    compile (BinOp op l r) = do
        l' <- compile l
        r' <- compile r
        return $ l' <> r' <> pure (StackBinOp op)


instance Compile Stmt where
    compile :: Stmt -> CompileState
    compile Skip = return []
    compile (Assn v e) = do
        e' <- compile e
        return $ e' <> pure (Set v)
    compile (If c t e) = do
        cnt <- get
        modify (+ 1)
        c' <- compile c
        t' <- compile t
        e' <- compile e
        return $
            c' <>
            [JumpZero $ "else" <.> cnt] <>
            t' <>
            [Jump $ "end" <.> cnt] <>
            [Label $ "else" <.> cnt] <>
            e' <>
            [Label $ "end" <.> cnt]
    
    compile (While c b) = do
        cnt <- get
        modify (+ 1)
        c' <- compile c
        b' <- compile b
        return $
            [Jump $ "while" <.> cnt] <>
            [Label $ "body" <.> cnt] <>
            b' <>
            [Jump $ "while" <.> cnt] <>
            c' <>
            [JumpNotZero $ "while" <.> cnt]
    
    compile (Lang.Read v) = return $ pure StackMachine.Read <> pure (Set v)
    compile (Lang.Write v) = do
        v' <- compile v
        return $ v' <> pure StackMachine.Write
    compile (Lang.Call f args) = do
        args' <- reverse <$> mapM compile args
        return $ mconcat args' <> pure (StackMachine.Call f)


instance Compile Function where
    compile :: Function -> CompileState
    compile (Fun name params body) = do
        b' <- compile body
        return $
            [Label name] <>
            [Begin params] <>
            b' <>
            [End]


instance Compile ProgramBody where
    compile :: ProgramBody-> CompileState
    compile (ProgramBody s) = mconcat <$> mapM compile s


instance Compile Program where
    compile :: Program -> CompileState
    compile (Program fs b) = do
        b' <- compile b
        fs' <- mapM compile fs
        return $ b' <>
            [End] <>
            mconcat fs'

