-- DO NOT EDIT: ANY CHANGES WILL BE REVERTED
module Lexer where

import Data.Char (isSpace, isDigit, isLetter, isAlphaNum)

import Types

tokenTable :: [(Char, Token)]
tokenTable = [ ('+', Plus)
             , ('-', Minus)
             , ('*', Times)
             , ('(', LParen)
             , (')', RParen)
             , ('{', LBrace)
             , ('}', RBrace)
             , ('=', Eq)
             , (';', Semi)
             ]

tokenise :: String -> Either Error [Token]
tokenise inp = tokenise' inp []
  where -- making this tail-recursive avoids the need to process the Either
        -- every time it returns, which ends up being much cleaner at the cost
        -- of a `reverse`...
        tokenise' :: String -> [Token] -> Either Error [Token]
        tokenise' [] toks = Right (reverse toks)
        tokenise' inp@(c : cs) toks
          | isSpace c  = tokenise' cs toks
          | isDigit c  = let (t, cs') = span isDigit inp
                         in tokenise' cs' (Nat (read t) : toks)
          | isLetter c = let (t, cs') = span isAlphaNum inp
                             tok = if t == "while" then WhileTok else Ident t
                         in tokenise' cs' (tok : toks)
          | otherwise  = maybe (Left (BadChar c))
                               (\op -> tokenise' cs (op : toks))
                               (lookup c tokenTable)
