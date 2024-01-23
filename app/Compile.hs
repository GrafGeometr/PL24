{-# LANGUAGE OverloadedStrings #-}
module Compile where
import StackMachine
import Lang


class Compile a where
    compile :: a -> StackMachine


instance Compile Expr where
    compile (Const v) = pure $ StackConst v
    compile (Var v) = pure $ StackVar v
    compile (BinOp op l r) = compile l <> compile r <> pure (StackBinOp op)


instance Compile Stmt where
    compile Skip = []
    compile (Assn v e) = compile e <> pure (Set v)
    compile (If c t e) = 
        compile c <> 
        pure (JumpZero "else") <> 
        compile t <>
        pure (Jump "end") <>
        pure (Label "else") <>
        compile e <>
        pure (Label "end")
    
    compile (While c b) =
        pure (Jump "while") <>
        pure (Label "body") <>
        compile b <>
        pure (Label "while") <>
        compile c <>
        pure (JumpNotZero "body")

    compile (Lang.Read v) = pure StackMachine.Read <> pure (Set v)
    compile (Lang.Write e) = compile e <> pure StackMachine.Write


instance Compile Program where
    compile (Program p) = foldMap compile p
