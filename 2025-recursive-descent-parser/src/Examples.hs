-- DO NOT EDIT: ANY CHANGES WILL BE REVERTED
module Examples where

import Types

fact :: String
fact = "acc = 1; n = in1; while n {acc = acc * n; n = n + -1}; res1 = acc"

factParse :: Either Error Block
factParse = Right [Asgn "acc" (Val 1),
                   Asgn "n" (Var "in1"),
                   While (Var "n") [Asgn "acc" (Mul (Var "acc") (Var "n")),
                                    Asgn "n" (Add (Var "n") (Val (-1)))],
                   Asgn "res1" (Var "acc")]

atomToks1, atomToks2, atomToks3, atomToks4 :: [Token]
atomToks1 = [Ident "x",Plus,Ident "y", RBrace]
atomToks2 = [Nat 2,Plus,Ident "y", RBrace]
atomToks3 = [Minus, Nat 2,Plus,Ident "y", RBrace]
atomToks4 = [Plus,Ident "y", RBrace]

asgnToks1, asgnToks2, asgnToks3, asgnToks4 :: [Token]
asgnToks1 = [Ident "x",Eq,Nat 1,RBrace]
asgnToks2 = [Ident "x",Eq,Ident "y"]
asgnToks3 = [Nat 4,Ident "y",Eq,Nat 2]
asgnToks4 = [Ident "x",Eq,RBrace]
asgnToks5 = [Ident "x",Times,Ident "y"]

termToks1, termToks2, termToks3, termToks4 :: [Token]
termToks1 = [Ident "x",Times,Nat 3,RBrace]
termToks2 = [Ident "x",Times,Ident "y",Times,Nat 2]
termToks3 = [Semi,Times,Nat 2]
termToks4 = [Nat 2,Times]

blockToks1, blockToks2, blockToks3, blockToks4, blockToks5 :: [Token]
blockToks1 = [Ident "y",Eq,Ident "x"]
blockToks2 = blockToks5 ++ blockToks1
blockToks3 = [Ident "x",Eq,RParen] ++ blockToks1
blockToks4 = [Ident "x",Times,Ident "y"]
blockToks5 = [Ident "x",Eq,Nat 4,Semi]

progStr1, progStr2, progStr3, progStr4, progStr5, progStr6  :: String
progStr1 = "res1=0"
progStr2 = "v1=1; v2=2; res1=v1+v2*3"
progStr3 = "x=?; res1=2"
progStr4 = "x=1; 4res1=2"
progStr5 = "x={1; res1=2"
progStr6 = "x=1 res1=2"

parenToks1, parenToks2, parenToks3, parenToks4, parenToks5 :: [Token]
parenToks1 = [LParen,Ident "x",RParen]
parenToks2 = [LParen,Ident "x",Times,Ident "y",RParen]
parenToks3 = [Ident "x",Times,LParen,Nat 2,Plus,Ident "y",RParen]
parenToks4 = [LParen,Ident "x",Times,RParen]
parenToks5 = [Ident "x",Times,LParen,Nat 2]

badFact1 :: String
badFact1 = "acc = 1; n = 5; while n (acc = acc * n; n = n + -1}; res = acc"

badFact2 :: String
badFact2 = "acc = 1; n = 5; while n {}; res = acc"

badFact3 :: String
badFact3 = "acc = 1; n = 5; while n {acc = 5acc * n; n = n + -1}; res = acc"
