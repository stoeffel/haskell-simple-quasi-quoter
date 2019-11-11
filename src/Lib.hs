{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib where

import Control.Monad (fail)
import qualified Data.Attoparsec.Text as P
import qualified Data.Text as T
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as QQ
import Language.Haskell.TH.Syntax (Lift)
import Protolude

qq :: QQ.QuasiQuoter
qq =
  QQ.QuasiQuoter
    { QQ.quoteExp = run . T.pack,
      QQ.quoteType = fail "not supported",
      QQ.quotePat = fail "not supported",
      QQ.quoteDec = fail "not supported"
    }

data Expr
  = BinOp Op Expr Expr
  | Lit Int
  deriving (Lift, Show)

data Op
  = Add
  | Minus
  | Mul
  deriving (Lift, Show)

run :: Text -> TH.Q TH.Exp
run x =
  case P.parse parser (T.strip x <> "\n") of
    P.Done _ exprs -> [e|exprs|]
    err -> fail ("Parsing failed: " <> show err)

interpret :: Expr -> Int
interpret (BinOp o x y) = opFun o (interpret x) (interpret y)
interpret (Lit x) = x

opFun :: Op -> (Int -> Int -> Int)
opFun Add = (+)
opFun Minus = (-)
opFun Mul = (*)

parser :: P.Parser Expr
parser = expr <* P.endOfLine

expr :: P.Parser Expr
expr = P.choice [lit, binOp]

lit :: P.Parser Expr
lit = Lit <$> P.decimal

binOp :: P.Parser Expr
binOp =
  P.try . inParens $
    BinOp <$> spaced op <*> spaced expr <*> spaced expr

inParens :: P.Parser a -> P.Parser a
inParens p = P.char '(' *> spaced p <* P.char ')'

spaced :: P.Parser a -> P.Parser a
spaced p = P.skipMany P.space *> p <* P.skipMany P.space

op :: P.Parser Op
op =
  P.choice
    [ const Add <$> P.char '+',
      const Minus <$> P.char '-',
      const Mul <$> P.char '*'
    ]
