-----------------------------------------------------------------------------------------------------------------------

module Parser(parse) where

import Text.Parsec hiding(State, parse)
import Text.Parsec.Char
import Text.Parsec.Indent
import Control.Monad.State
import Text.Printf(printf)

import Language

-----------------------------------------------------------------------------------------------------------------------

type IParser a = ParsecT String () (State SourcePos) a


reserved :: [Name]
reserved =
   [
      "let",
      "in",
      "="
   ]


parse :: SourceName -> String -> [ScDefn Name]
parse srcName input = case iParse program srcName input of
   Right res -> res 
   Left err -> error $ show err 


iParse :: IParser a -> SourceName -> String -> Either ParseError a
iParse aParser srcName input =
   runIndent srcName $ runParserT aParser () srcName input


var :: IParser Name
var = do
   n <- (letter <|> char '_' <?> "variable")
   ns <- many (alphaNum <|> char '_' <|> char '\'' <?> "" )
   let name = n:ns
   if elem name reserved
      then fail $ printf "keyword %s is unexpected" name
      else return name


program :: IParser [ScDefn Name]
program = do 
   defn <- scDefn
   return [defn] 


scDefn :: IParser (ScDefn Name)
scDefn = do
   name <- skipSpaces var
   spaces
   argNames <- many $ skipSpaces var
   _ <- skipSpaces $ string "="
   bl <- expr
   return $ ScDefn name argNames bl


expr :: IParser (Expr  Name)
expr = do
   spaces
   number



skipSpaces :: IParser a -> IParser a
skipSpaces p = do
   res <- p
   spaces
   return res


  

number :: IParser (Expr  Name)
number = do
   n <- many1 digit
   return $ ENum $ read n 


-----------------------------------------------------------------------------------------------------------------------

