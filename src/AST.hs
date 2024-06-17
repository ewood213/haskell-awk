module AST where

import qualified Value
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS

data AwkStatement = AwkStatement Expression [Action]
data Action = AssignAction ByteString Expression
            | AssignUpdateAction ByteString BinaryOp Expression
            | PrintAction [Expression]
data Expression = BinaryExpression Expression BinaryOp Expression
                | UnaryExpression Expression UnaryOp
                | FloatExpression Float
                | VarNameExpression ByteString
                | StringExpression ByteString
                | ColvarExpression Expression
                | NRExpression
                | NFExpression
                | BeginExpression
                | EndExpression

type BinaryOp = Value.Value -> Value.Value -> Value.Value
type UnaryOp = Value.Value -> Value.Value

