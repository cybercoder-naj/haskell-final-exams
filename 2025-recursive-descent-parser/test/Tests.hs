import IC.TestSuite

-- You MAY edit this file, but the changes to it WILL be undone after
-- we run "your" tests, if this file doesn't compile, you cannot fix that
-- by editing this file: fix your code in Parser.hs!

import Parser
import Lexer
import Examples
import Types

main :: IO ()
main = runTests tests

tests :: [TestGroup]
tests =
  [ testGroup "checkTok" checkTokTests
  , testGroup "parseAtom" parseAtomTests
  , testGroup "parseStmt" parseStmtTests
  , testGroup "parseTerm" parseTermTests
  , testGroup "parseBlock" parseBlockTests
  , testGroup "parse" parseTests
  , testGroup "parentheses" parenTests
  , testGroup "while" whileTests
  , testGroup "extraTests" extraTests
  ]

checkTokTests :: [TestCase]
checkTokTests =
  [ checkTok Plus [Plus,Nat 4,RBrace] --> Right [Nat 4,RBrace]
  , checkTok Plus [Minus,Nat 4,RBrace] --> Left (Unexpected (Just Minus) Plus)
  , checkTok Plus [] --> Left (Unexpected Nothing Plus)
  ]

parseAtomTests :: [TestCase]
parseAtomTests =
  [ parseAtom atomToks1 --> Right ([Plus,Ident "y",RBrace],Var "x")
  , parseAtom atomToks2 --> Right ([Plus,Ident "y",RBrace],Val 2)
  , parseAtom atomToks3 --> Right ([Plus,Ident "y",RBrace],Val (-2))
  , parseAtom atomToks4 --> Left (ExprNotFound (Just Plus))
  , parseAtom [Minus, Ident "y"] --> Left (IntNotFound (Just (Ident "y")))
  , parseAtom [] --> Left (ExprNotFound Nothing)
  ]

parseStmtTests :: [TestCase]
parseStmtTests =
  [ parseStmt asgnToks1 --> Right ([RBrace],Asgn "x" (Val 1))
  , parseStmt asgnToks2 --> Right ([],Asgn "x" (Var "y"))
  , parseStmt asgnToks3 --> Left (StmtNotFound (Just (Nat 4)))
  , parseStmt asgnToks4 --> Left (ExprNotFound (Just RBrace))
  -- you're allowed either of these two errors
  , parseStmt asgnToks5 == Left (StmtNotFound (Just (Ident "x"))) ||
    parseStmt asgnToks5 == Left (Unexpected (Just Times) Eq) --> True
  ]

parseTermTests :: [TestCase]
parseTermTests =
  [ parseTerm termToks1 --> Right ([RBrace],Mul (Var "x") (Val 3))
  , parseTerm termToks2 --> Right ([],Mul (Mul (Var "x") (Var "y")) (Val 2))
  , parseTerm termToks3 --> Left (ExprNotFound (Just Semi))
  , parseTerm termToks4 --> Left (ExprNotFound Nothing)
  ]

parseBlockTests :: [TestCase]
parseBlockTests =
  [ parseBlock blockToks1 --> Right ([],[Asgn "y" (Var "x")])
  , parseBlock blockToks2 --> Right ([],[Asgn "x" (Val 4),Asgn "y" (Var "x")])
  , parseBlock blockToks3 --> Left (ExprNotFound (Just RParen))
  -- you're allowed either of these two errors
  , parseBlock blockToks4 == Left (StmtNotFound (Just (Ident "x"))) ||
    parseBlock blockToks4 == Left (Unexpected (Just Times) Eq) --> True
  , parseBlock blockToks5 --> Left (StmtNotFound Nothing)
  ]

parseTests :: [TestCase]
parseTests =
  [ parse progStr1 --> Right [Asgn "res1" (Val 0)]
  , parse progStr2 --> Right [Asgn "v1" (Val 1),Asgn "v2" (Val 2),Asgn "res1" (Add (Var "v1") (Mul (Var "v2") (Val 3)))]
  , parse progStr3 --> Left (BadChar '?')
  , parse progStr4 --> Left (StmtNotFound (Just (Nat 4)))
  , parse progStr5 --> Left (ExprNotFound (Just LBrace))
  , parse progStr6 --> Left (UnparsedInput [Ident "res1",Eq,Nat 2])
  ]

parenTests :: [TestCase]
parenTests =
  [ parseAtom parenToks1 --> Right ([],Var "x")
  , parseAtom parenToks2 --> Right ([],Mul (Var "x") (Var "y"))
  , parseExpr parenToks3 --> Right ([],Mul (Var "x") (Add (Val 2) (Var "y")))
  , parseExpr parenToks4 --> Left (ExprNotFound (Just RParen))
  , parseExpr parenToks5 --> Left (Unexpected Nothing RParen)
  ]

whileTests :: [TestCase]
whileTests =
  [ parse fact --> factParse
  , parse badFact1 --> Left (Unexpected (Just LParen) LBrace)
  , parse badFact2 --> Left (StmtNotFound (Just RBrace))
  , parse badFact3 --> Left (Unexpected (Just (Ident "acc")) RBrace)
  ]

nest = "x=0; while x {y=x+2*x; while (y) {y=0}}; z=6"

nestParse = Right [Asgn "x" (Val 0), While (Var "x") [Asgn "y" (Add (Var "x") (Mul (Val 2) (Var "x"))), While (Var "y") [Asgn "y" (Val 0)]], Asgn "z" (Val 6)]

extraTests :: [TestCase]
extraTests =
  [ parse "" --> Left (StmtNotFound Nothing)
  , parse "x=((6))" --> Right [Asgn "x" (Val 6)]
  , parse "while 0 {}" --> Left (StmtNotFound (Just RBrace))
  , parse nest --> nestParse
  ]
