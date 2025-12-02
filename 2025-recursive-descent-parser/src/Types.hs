-- DO NOT EDIT: ANY CHANGES WILL BE REVERTED
module Types where

type Parser a = [Token] -> Either Error ([Token], a)

type Program = Block

type Block = [Stmt]

data Stmt = Asgn String Expr
          | While Expr Block
          deriving (Eq, Show)

data Expr = Add Expr Expr
          | Mul Expr Expr
          | Val Int
          | Var String
          deriving (Eq, Show)

data Token = Eq | Plus | Minus | Times | LParen | RParen | LBrace | RBrace
           | Semi | Nat Int | Ident String | WhileTok
           deriving (Eq, Show)

data Error = BadChar Char
           | StmtNotFound (Maybe Token)
           | ExprNotFound (Maybe Token)
           | IntNotFound (Maybe Token)
           | UnparsedInput [Token]
           | Unexpected (Maybe Token) Token
           deriving (Eq, Show)
