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
            "in"
         ],
      P.opStart = P.opLetter languageDef,
      P.opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~",
      P.reservedOpNames = ["+", "-"],
      P.caseSensitive  = True
   }


lexer :: P.GenTokenParser C.ByteString () (State SourcePos)
lexer = P.makeTokenParser languageDef

identifier :: IParser String
identifier = P.identifier lexer

integer :: IParser Integer
integer = P.integer lexer 

parens :: IParser a -> IParser a
parens = P.parens lexer

reserved :: String -> IParser ()
reserved = P.reserved lexer
 
reservedOp :: String -> IParser ()
reservedOp = P.reservedOp lexer


parse :: SourceName -> C.ByteString -> [ScDefn Name]
parse srcName input = case iParse program srcName input of
   Right res -> res 
   Left err -> error $ show err 


iParse :: IParser a -> SourceName -> C.ByteString -> Either ParseError a
iParse aParser srcName input =
   runIndent srcName $ runParserT aParser () srcName input


program :: IParser [ScDefn Name]
program = do
   p <- many scDefn
   spaces 
   eof
   return p


scDefn :: IParser (ScDefn Name)
scDefn = do
   name <- identifier
   argNames <- many identifier
   reservedOp "="
   e <- expr
   return $ ScDefn name argNames e


table :: [[Operator C.ByteString () (State SourcePos) (Expr Name)]]
table = 
   [
      [Prefix (prefix "-")]
   ]

expr :: IParser (Expr  Name)
expr =
   buildExpressionParser table apExpr
   <?> "expression"
   
   
apExpr :: IParser (Expr  Name)
apExpr = do
   t <- term
   ts <- spacePrefix term
   return $ createEApExpr $ reverse (t:ts) 
   where
      createEApExpr :: [Expr Name] -> Expr Name
      createEApExpr [e] = e
      createEApExpr (e:es) = EAp (createEApExpr es) e
      createEApExpr  _ = error "Empty expression encountered."    
      

literal :: IParser (Expr  Name)
literal = do
   i <- integer
   return $ ENum $ fromEnum i
   
   
var :: IParser (Expr  Name)
var = do
   ident <- identifier
   return $ EVar ident


term :: IParser (Expr  Name)
term = choice 
   [
      parens expr,
      literal,
      var
   ]

   
prefix :: String -> IParser (Expr Name -> Expr Name)
prefix name = do
   reservedOp name
   return (EAp (EVar "neg"))


spacePrefix :: IParser a -> IParser [a]
spacePrefix p = many . try $ do
   spaces
   indented
   p

-----------------------------------------------------------------------------------------------------------------------

