{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module TestParser where
import Parser
import DynFlags
import Lexer
import SrcLoc
import RdrName
import HsExpr
import FastString
import StringBuffer
import Outputable
import Pretty
import Platform

pExp :: String -> String
pExp = runParser (defaultDynFlag
                  (Settings
                   {sTargetPlatform = }))

runParser :: DynFlags -> String -> String
-- LHsExpr RdrName
runParser flags str = let POk _ r = unP parseExpression parseState
                in  showPpr flags r
  where
    filename = "<interactive>"
    location = mkRealSrcLoc (mkFastString filename) 1 1
    buffer = stringToStringBuffer str
    parseState = mkPState flags buffer location
