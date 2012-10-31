module Parser (
    parseLatte
) where

import Text.ParserCombinators.Parsec

import LatteAbs

type LtParser st res = GenParser Char st res

mylex :: LtParser st a -> LtParser st a
mylex p = do
    result <- p
    spaces
    many (comment >> spaces)
    return result

comment :: LtParser st ()
comment = do
     do try (string "#" <|> string "//")
        anyChar `manyTill` (char '\n')
        return ()
    <|> do
        string "/*"
        anyChar `manyTill` (try $ string "*/")
        return ()

topParser :: LtParser st LatteTree
topParser = do
    spaces
    funL <- mylex $ many funParser
    eof
    return $ LtTop funL

funParser :: LtParser st LatteFun
funParser = do
    resT <- mylex $ typeParser
    name <- mylex $ idParser
    argL <- mylex $ between (char '(' >> spaces) (char ')' >> spaces)
	(argParser `sepBy` (spaces >> (char ',') >> spaces))
    body <- mylex $ blockParser
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
    t <- mylex $ typeParser
    name <- idParser
    return $ LtArg name t

parseLatte :: String -> String -> Either ParseError LatteTree
parseLatte = parse topParser
