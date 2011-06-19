-- Parse S expressions and lambda expressions
module Parser (parse) where

import Language.Haskell.Parser
import Language.Haskell.Syntax
import Value
import Card

parse s = transform meat
    where ParseOk (HsModule _ _ _ _ [decl]) = parseModule ("the_value = " ++ s)
          HsPatBind _ _ (HsUnGuardedRhs meat) _ = decl
          transform (HsLambda _ [HsPVar (HsIdent varName)] value) = ValueLambda varName (transform value)
          transform (HsParen value) = transform value
          transform (HsApp lhs rhs) = ValueApplication (transform lhs) (transform rhs)
          transform (HsVar (UnQual (HsIdent varName))) = transformIdentifier varName
          transform (HsCon (UnQual (HsIdent varName))) = transformIdentifier varName
          transform (HsLit (HsInt n)) = ValueNum (fromInteger n)
          transform x = error $ show x
          transformIdentifier "I"      = valueI
          transformIdentifier "zero"   = valueZero
          transformIdentifier "succ"   = valueSucc
          transformIdentifier "dbl"    = valueDbl
          transformIdentifier "get"    = valueGet
          transformIdentifier "put"    = valuePut
          transformIdentifier "S"      = valueS
          transformIdentifier "K"      = valueK
          transformIdentifier "inc"    = valueInc
	  transformIdentifier "dec"    = valueDec
	  transformIdentifier "attack" = valueAttack
	  transformIdentifier "help"   = valueHelp
	  transformIdentifier "copy"   = valueCopy
	  transformIdentifier "revive" = valueRevive
	  transformIdentifier "zombie" = valueZombie
          transformIdentifier varName  = ValueVariable varName
