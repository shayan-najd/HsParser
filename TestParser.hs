{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module TestParser where
import Parser
import U.DynFlags
import Lexer
import SrcLoc
import RdrName
import HsExpr
import U.FastString
import U.StringBuffer
import U.Outputable
import U.Pretty
import Platform

pExp :: String -> String
pExp = runParser defaultDynFlag


runParser :: DynFlags -> String -> String
-- LHsExpr RdrName
runParser flags str = let POk _ r = unP parseExpression parseState
                in  showPpr flags r
  where
    filename = "<interactive>"
    location = mkRealSrcLoc (mkFastString filename) 1 1
    buffer = stringToStringBuffer str
    parseState = mkPState flags buffer location
