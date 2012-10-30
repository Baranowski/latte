module Parser (
    parseLatte
) where

import Text.ParserCombinators.Parsec

import LatteAbs

type LtParser st res = GenParser Char st res

topParser :: LtParser st LatteTree
topParser = do
    spaces
    funL <- funParser `sepBy` (many1 space)
    spaces
    return $ LtTop funL

funParser :: LtParser st LatteFun
funParser = do
    resT <- typeParser
    spaces
    name <- idParser
    spaces
    argL <- between (char '(' >> spaces) (char ')' >> spaces)
	(argParser `sepBy` (spaces >> (char ',') >> spaces))
    spaces
    body <- blockParser
    return $ LtFun name resT argL body

typeParser :: LtParser st LatteType
typeParser = do
    result <- try (string "int" >> return LtInt)
	  <|> try (string "string" >> return LtString)
	  <|> try (string "boolean" >> return LtBool)
	  <|> try (string "void" >> return LtVoid)
    return result

idParser :: LtParser st LatteId
idParser = do
    first <- oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ "_"
    others <- many (oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ "_" ++ ['0'..'9'])
    return $ LtId $ first:others

blockParser :: LtParser st LatteStmt
blockParser = do
    char '{'
    many $ noneOf "{}"
    nested <- blockParser `endBy` (many $ noneOf "{}")
    char '}'
    return $ LtBlock nested

argParser :: LtParser st LatteArg
argParser = do
    t <- typeParser
    spaces
    name <- idParser
    return $ LtArg name t

parseLatte :: String -> String -> Either ParseError LatteTree
parseLatte = parse topParser
