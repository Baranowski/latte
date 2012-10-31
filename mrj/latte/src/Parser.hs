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

type LtSParser = LtParser st LatteStmt
blockParser, declParser, assParser, incrParser, decrParser, retParser,
    ifParser, whileParser, sExprParser :: LtSParser

blockParser = do
    mylex $ char '{'
    stmtL <- mylex $ many $ mylex stmtParser
    mylex $ char '}'
    return $ LtBlock stmtL

declParser = do
    t <- mylex typeParser
    declL <- (mylex $ declItemParser t) `sepBy` (mylex $ char ',')
    mylex $ char ';'
    return $ LtDBlock declL

assParser = do
    name <- mylex idParser
    mylex $ char '='
    expr <- mylex exprParser
    mylex $ char ';'
    return LtAss name expr

incrParser = do
    name <- mylex idParser
    mylex $ string "++"
    mylex $ char ';'
    return LtIncr name

decrParser = do
    name <- mylex idParser
    mylex $ string "--"
    mylex $ char ';'
    return LtDecr name

retParser = do
    mylex $ keyword "return"
    res <- optionMaybe $ mylex $ exprParser
    mylex $ char ';'
    return $ LtReturn $ case res of
        Just e -> e
        Nothing -> LtExp ()

ifParser = do
    mylex $ keyword "if"
    mylex $ char '('
    cond <- mylex exprParser
    mylex $ char ')'
    ifStmt <- mylex stmtParser
    mbeElse <- optionMaybe $ do
        mylex $ keyword "else"
        mylex stmtParser
    mylex $ char ';'
    let elseStmt = case mbeElse of {
        Just s -> s ;
        Nothing -> LtPass }
    return $ LtIf cond ifStmt elseStmt

whileParser = do
    mylex $ keyword "while"
    mylex $ char '('
    cond <- mylex exprParser
    mylex $ char ')'
    stmt <- mylex stmtParser
    return LtWhile cond stmt

sExprParser = do
    e <- mylex exprParser
    return LtSExpr e

stmtParser :: LtSParser
stmtParser = do
        char ';' >> return LtPass
    <|> blockParser
    <|> try declParser
    <|> try retParser
    <|> try ifParser
    <|> try whileParser
    <|> try assParser
    <|> try incrParser
    <|> try decrParser
    <|> try sExprParser

argParser :: LtParser st LatteArg
argParser = do
    t <- mylex $ typeParser
    name <- idParser
    return $ LtArg name t

parseLatte :: String -> String -> Either ParseError LatteTree
parseLatte = parse topParser
