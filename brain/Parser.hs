-- Parse S expressions and lambda expressions
module Parser (parse) where

import Language.Haskell.Parser
import Language.Haskell.Syntax
import Value
import Card

parse s = show (transform meat)
    where ParseOk (HsModule _ _ _ _ [decl]) = parseModule ("the_value = " ++ s)
          HsPatBind _ _ (HsUnGuardedRhs meat) _ = decl
          transform (HsLambda _ [HsPVar (HsIdent varName)] value) = ValueLambda varName (transform value)
          transform (HsParen value) = transform value
          transform (HsApp lhs rhs) = ValueApplication (transform lhs) (transform rhs)
          transform (HsVar (UnQual (HsIdent varName))) = transformIdentifier varName
          transform (HsCon (UnQual (HsIdent varName))) = transformIdentifier varName
          transform (HsLit (HsInt n)) = ValueNum (fromInteger n)
          transform x = error $ show x
          transformIdentifier "I" = ValueCard IdentityCard
          transformIdentifier "zero" = ValueCard ZeroCard
          transformIdentifier "succ" = ValueCard SuccCard
          transformIdentifier "dbl" = ValueCard DoubleCard
          transformIdentifier "get" = ValueCard GetCard
          transformIdentifier "put" = ValueCard PutCard
          transformIdentifier "S" = ValueCard SCard
          transformIdentifier "K" = ValueCard KCard
          transformIdentifier "inc" = ValueCard IncCard
	  transformIdentifier "dec" = ValueCard DecCard
	  transformIdentifier "attack" = ValueCard AttackCard
	  transformIdentifier "help" = ValueCard HelpCard
	  transformIdentifier "copy" = ValueCard CopyCard
	  transformIdentifier "revive" = ValueCard ReviveCard
	  transformIdentifier "zombie" = ValueCard ZombieCard
          transformIdentifier varName = ValueVariable varName
