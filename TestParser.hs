{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module TestParser where
import Parser
import U.DynFlags
import Lexer
import Language.Haskell.Syntax.SrcLoc
import Language.Haskell.Utility.FastString
import U.StringBuffer
import ShowInstances ()
import OutputableInstances ()
import U.Outputable

pExp :: String -> String
pExp = runParser defaultDynFlag

runParser :: DynFlags -> String -> String
-- LHsExpr RdrName
runParser flags str = case unP parseExpression parseState of
                        POk     _ r -> show (unLoc r)
                        PFailed s e -> "Error at " ++ show s ++ ":" ++
                                       showSDoc flags e
  where
    filename = "<interactive>"
    location = mkRealSrcLoc (mkFastString filename) 1 1
    buff = stringToStringBuffer str
    parseState = mkPState flags buff location
