-----------------------------------------------------------------------------------------------------------------------

module Parser(parse) where

import Text.Parsec hiding(State, parse)
import qualified Text.Parsec.Token as P
import Text.Parsec.Language
import Text.Parsec.Indent
import Control.Monad.State
import Text.Printf(printf)

import Language

-----------------------------------------------------------------------------------------------------------------------

type IParser a = ParsecT String () (State SourcePos) a


languageDef :: GenLanguageDef String st (State SourcePos)
languageDef = P.LanguageDef
   { 
      P.commentStart = "/*",
      P.commentEnd = "*/",
      P.commentLine  = "//",
      P.nestedComments = True,
      P.identStart  = letter,
      P.identLetter = alphaNum <|> oneOf "_'",
      P.reservedNames = 
         [
            "let",
            "in",
            "="
         ],
      P.opStart = P.opLetter languageDef,
      P.opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~",
      P.reservedOpNames = ["+", "-"],
      P.caseSensitive  = True
   }


lexer = P.makeTokenParser languageDef

identifier = P.identifier lexer
reserved = P.reserved lexer 


parse :: SourceName -> String -> [ScDefn Name]
parse srcName input = case iParse program srcName input of
   Right res -> res 
   Left err -> error $ show err 


iParse :: IParser a -> SourceName -> String -> Either ParseError a
iParse aParser srcName input =
   runIndent srcName $ runParserT aParser () srcName input


program :: IParser [ScDefn Name]
program = do 
   defn <- scDefn
   return [defn] 


scDefn :: IParser (ScDefn Name)
scDefn = do
   name <- identifier
   spaces
   argNames <- many $ identifier
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

