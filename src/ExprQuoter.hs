
-----------------------------------------------------------------------------------------------------------------------

module ExprQuoter(expr) where

import Language.Haskell.TH.Quote
import qualified Language.Haskell.TH as TH
import qualified Data.ByteString.Char8 as C

import Parser(parse)

-----------------------------------------------------------------------------------------------------------------------

expr :: QuasiQuoter
expr = QuasiQuoter
   {
      quoteExp = quoteExprExp,
      quotePat = quoteExprPat,
      quoteType = undefined,
      quoteDec = undefined
   }


quoteExprExp :: String -> TH.Q TH.Exp
quoteExprExp str =  do
   loc <- TH.location
   let e = parse (TH.loc_filename loc) $ C.pack str
   dataToExpQ (const Nothing) e


quoteExprPat :: String -> TH.Q TH.Pat
quoteExprPat str =  do
   loc <- TH.location
   let e = parse (TH.loc_filename loc) $ C.pack str
   dataToPatQ (const Nothing) e

-----------------------------------------------------------------------------------------------------------------------


