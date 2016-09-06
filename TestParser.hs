{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module TestParser where
import Parser
import U.DynFlags
import Lexer
import SrcLoc
import U.FastString
import U.StringBuffer
import ShowInstances ()
import RdrName
import HsExpr

pExp :: String -> LHsExpr RdrName
pExp = runParser defaultDynFlag


runParser :: DynFlags -> String -> LHsExpr RdrName
-- LHsExpr RdrName
runParser flags str = let POk _ r = unP parseExpression parseState
                      in  r
  where
    filename = "<interactive>"
    location = mkRealSrcLoc (mkFastString filename) 1 1
    buff = stringToStringBuffer str
    parseState = mkPState flags buff location
