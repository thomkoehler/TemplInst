
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
      quotePat = undefined,
      quoteType = undefined,
      quoteDec = undefined
   }


quoteExprExp str =  do
   loc <- TH.location
   let
      pos =  (TH.loc_filename loc,fst (TH.loc_start loc), snd (TH.loc_start loc))
      expr = parse (TH.loc_filename loc) $ C.pack str

   dataToExpQ (const Nothing) expr

-----------------------------------------------------------------------------------------------------------------------


