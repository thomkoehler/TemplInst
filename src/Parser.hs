-----------------------------------------------------------------------------------------------------------------------

module Parser where

import Text.Parsec hiding (State)
import Text.Parsec.Indent
import Control.Monad.State
import Text.Printf(printf)

import Language

-----------------------------------------------------------------------------------------------------------------------

type IParser a = ParsecT String () (State SourcePos) a

iParse :: IParser a -> SourceName -> String -> Either ParseError a
iParse aParser srcName input =
   runIndent srcName $ runParserT aParser () srcName input


reserved :: [Name]
reserved =
   [
      "let",
      "in"
   ]


var :: IParser Name
var = do
   n <- (letter <|> char '_' <?> "variable")
   ns <- many (alphaNum <|> char '_' <|> char '\'' <?> "" )
   let name = n:ns
   if elem name reserved
      then fail $ printf "keyword %s is unexpected" name
      else return name



scDefn :: IParser (ScDefn Name)
scDefn = do
   name <- var
   spaces
   argNames <- many var
   bl <- expr
   return $ ScDefn name argNames bl

expr :: IParser (Expr  Name)
expr = undefined

-----------------------------------------------------------------------------------------------------------------------

