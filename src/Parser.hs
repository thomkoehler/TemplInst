-----------------------------------------------------------------------------------------------------------------------

module Parser(parse) where

import Text.Parsec hiding(State, parse)
import qualified Text.Parsec.Token as P
import Text.Parsec.Language
import Text.Parsec.Indent
import Text.Parsec.Expr
import Control.Monad.State

import qualified Data.ByteString.Char8 as C

import Language

-----------------------------------------------------------------------------------------------------------------------

type IParser a = ParsecT C.ByteString () (State SourcePos) a


languageDef :: GenLanguageDef C.ByteString st (State SourcePos)
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

integer = P.integer lexer 
braces = P.braces lexer

reserved :: String -> IParser ()
reserved = P.reserved lexer
 
reservedOp :: String -> IParser ()
reservedOp = P.reservedOp lexer


parse :: SourceName -> String -> [ScDefn Name]
parse srcName input = case iParse program srcName input of
   Right res -> res 
   Left err -> error $ show err 


iParse :: IParser a -> SourceName -> String -> Either ParseError a
iParse aParser srcName input =
   runIndent srcName $ runParserT aParser () srcName $ C.pack input


program :: IParser [ScDefn Name]
program = many scDefn 


scHead :: IParser (Name, [Name])
scHead = do
   name <- identifier
   argNames <- many identifier
   reservedOp "="
   return (name, argNames)
   

scDefn :: IParser (ScDefn Name)
scDefn = withBlock (\(name, args) [bl] -> ScDefn name args bl) scHead expr


table = 
   [
      [Prefix (prefix "-")]
   ]

expr :: IParser (Expr  Name)
expr = do
   buildExpressionParser table term
   <?> "expression"


literal :: IParser (Expr  Name)
literal = do
   i <- integer
   return $ ENum $ fromEnum i

term :: IParser (Expr  Name)
term = choice 
   [
      literal,
      braces expr      
   ]
   
prefix :: String -> IParser (Expr Name -> Expr Name)
prefix name = do
   reservedOp name
   return (EAp (EVar "neg"))


-----------------------------------------------------------------------------------------------------------------------

