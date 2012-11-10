module Parser (
    parseLatte
) where

import Text.ParserCombinators.Parsec

import LatteAbs

type LtParser st res = GenParser Char st res

-- Helpers

wordFirstChar =  ['a'..'z'] ++ ['A'..'Z'] ++ "_"
wordChars = ['a'..'z'] ++ ['A'..'Z'] ++ "_" ++ ['0'..'9']
reservedKeywords = [ "return", "if", "else", "while", "int", "string", "void",
    "boolean", "true", "false" ]

wordParser :: LtParser st String
wordParser = do
    first <- oneOf wordFirstChar
    others <- many (oneOf wordChars)
    return $  first:others

keyword :: String -> LtParser st ()
keyword s = do
    string s
    lookAhead $ noneOf wordChars
    return ()

mylex :: LtParser st a -> LtParser st a
mylex p = do
    result <- p
    spaces
    many (comment >> spaces)
    return result

comment :: LtParser st ()
comment = do
     do try (string "#" <|>  string "//")
        anyChar `manyTill` (char '\n')
        return ()
    <|> do
        try $ string "/*"
        anyChar `manyTill` (try $ string "*/")
        return ()

mypos :: LtParser st Pos
mypos = do
    sourcePos <- getPosition
    return $ Pos (sourceLine sourcePos) (sourceColumn sourcePos)

loc p = do
    pos <- mypos
    res <- p
    return $ Loc pos res
-- Highest-level parsers

topParser :: LtParser st LatteTree
topParser = do
    spaces
    many (comment >> spaces)
    funL <- mylex $ many1 $ loc funParser
    eof
    return $ LtTop funL

idParser :: LtParser st LatteId
idParser = try $ do
    word <- wordParser
    if word `elem` reservedKeywords
        then unexpected $ "reserved keyword: \"" ++ word ++ "\""
        else return word


funParser :: LtParser st LatteFun
funParser = do
    resT <- mylex $ typeParser
    name <- mylex $ idParser
    argL <- mylex $ between (char '(' >> spaces) (char ')' >> spaces)
	((loc argParser) `sepBy` (spaces >> (char ',') >> spaces))
    body <- mylex $ loc blockParser
    return $ LtFun name resT argL body

typeParser :: LtParser st LatteType
typeParser = do
    result <- try (keyword "int" >> return LtInt)
	  <|> try (keyword "string" >> return LtString)
	  <|> try (keyword "boolean" >> return LtBool)
	  <|> try (keyword "void" >> return LtVoid)
    return result

-- Statements

blockParser, declParser, assParser, incrParser, decrParser, retParser,
    ifParser, whileParser, sExprParser, passParser :: LtParser st LatteStmt
stmtParser :: LtParser st (Located LatteStmt)

blockParser = do
    mylex $ char '{'
    stmtL <- mylex $ many $ mylex stmtParser
    mylex $ char '}'
    return $ LtBlock stmtL

declItemParser :: LatteType -> LtParser st LatteDecl
declItemParser t = do
    name <- mylex idParser
    initV <- optionMaybe $ do
        mylex $ char '='
        mylex exprParser
    case initV of
        Just e -> return $ LtDExpr name e
        Nothing -> return $ LtDEmpty name

declParser = do
    t <- mylex typeParser
    declL <- (loc $ mylex $ declItemParser t) `sepBy` (mylex $ char ',')
    mylex $ char ';'
    return $ LtDBlock t declL

assParser = do
    name <- mylex idParser
    mylex $ char '='
    expr <- mylex exprParser
    mylex $ char ';'
    return $ LtAss name expr

incrParser = do
    name <- mylex idParser
    mylex $ string "++"
    mylex $ char ';'
    return $ LtIncr name

decrParser = do
    name <- mylex idParser
    mylex $ string "--"
    mylex $ char ';'
    return $ LtDecr name

retParser = do
    mylex $ keyword "return"
    res <- optionMaybe $ mylex $ exprParser
    mylex $ char ';'
    pos <- mypos
    return $ LtReturn $ case res of
        Just e -> e
        Nothing -> Loc pos LtEVoid

ifParser = do
    mylex $ keyword "if"
    mylex $ char '('
    cond <- mylex exprParser
    mylex $ char ')'
    ifStmt <- mylex stmtParser
    mbeElse <- optionMaybe $ do
        mylex $ keyword "else"
        mylex stmtParser
    pos <- mypos
    let elseStmt = case mbeElse of {
        Just s -> s ;
        Nothing -> Loc pos LtPass }
    return $ LtIf cond ifStmt elseStmt

whileParser = do
    mylex $ keyword "while"
    mylex $ char '('
    cond <- mylex exprParser
    mylex $ char ')'
    stmt <- mylex stmtParser
    return $ LtWhile cond stmt

sExprParser = do
    e <- mylex exprParser
    mylex $ char ';'
    return $ LtSExpr e

stmtParser = loc $ do
        blockParser
    <|> try declParser
    <|> try retParser
    <|> try ifParser
    <|> try whileParser
    <|> try assParser
    <|> try incrParser
    <|> try decrParser
    <|> try sExprParser
    <|> try passParser

passParser = do
    mylex $ char ';'
    return LtPass

argParser :: LtParser st LatteArg
argParser = do
    t <- mylex $ typeParser
    name <- idParser
    return $ LtArg name t

-- Expressions


-- exprAbs[A]Parser nextParser opParserL constr
-- Use nextParser to parse sequence of expressions, separated by operators
-- recognized by members of opParserL.
-- Assumption: every operator parser returns a constant value

-- When there is only one operator parser, we can ignore its value
exprAbsParser nextParser [opParser] constr = do
    pos <- mypos
    eL <- (mylex nextParser) `sepBy1` (mylex opParser)
    case eL of
        [e] -> return e
        _ -> return $ Loc pos $ constr eL

-- When there are more operator parsers, the result will be more complex:
-- We associate an operator with every expression except for the first one
exprAbsAParser nextParser opParserL constr = do
    pos <- mypos
    eFirst <- mylex nextParser
    eL <- many $ do
        op <- mylex $ choice opParserL
        e <- mylex nextParser
        return (op, e)
    case eL of
        [] -> return eFirst
        _ -> return $ Loc pos $ constr eFirst eL

exprParser :: LtParser st (Located LatteExpr)
exprParser = exprOrParser
exprOrParser = exprAbsParser exprAndParser [try $ string "||"] LtEOr
exprAndParser = exprAbsParser exprRelParser [try $ string "&&"] LtEAnd
exprRelParser = do
    pos <- mypos
    e1 <- mylex exprAddParser
    opMbe <- mylex $ optionMaybe $ choice [
        try $ string "<=" >> return Rle,
        try $ string "<" >> return Rlt,
        try $ string ">=" >> return Rge,
        try $ string ">" >> return Rgt,
        try $ string "==" >> return Req,
        try $ string "!=" >> return Rne ]
    case opMbe of
        Nothing -> return e1
        Just op -> do
            e2 <- mylex exprAddParser
            return $ Loc pos $ LtERel op e1 e2
exprAddParser = exprAbsAParser exprMulParser [
    (char '+') >> return Ladd,
    (char '-') >> return Lsub] LtEAdd
exprMulParser = exprAbsAParser exprUnaryParser [
    (char '*') >> return Lmul,
    (char '/') >> return Ldiv,
    (char '%') >> return Lmod ] LtEMul
exprUnaryParser = do
        loc $ do
           mylex $ char '!'
           e <- exprBasicParser
           return $ LtENot e
    <|> (loc $ do
           char '-'
           e <- exprBasicParser
           return $ LtENeg e)
    <|> exprBasicParser

exprBasicParser = do
        do mylex $ char '('
           e <- mylex exprParser
           mylex $ char ')'
           return e
    <|> (loc $ do
            do
               char '"'
               str <- many (noneOf "\\\""
                        <|> (char '\\' >> anyChar))
               char '"'
               return $ LtEStr str
        <|> try (keyword "true" >> return LtETrue)
        <|> try (keyword "false" >> return LtEFalse)
        <|> do
               str <- many1 (oneOf ['0'..'9'])
               let i = (read str) :: Int
               return $ LtEInt i
        <|> do
               name <- mylex idParser
               args <- optionMaybe $ do
                   mylex $ char '('
                   eL <- exprParser `sepBy` (mylex $ char ',')
                   mylex $ char ')'
                   return eL
               case args of
                   Nothing -> return $ LtEId name
                   Just eL -> return $ LtEApp name eL)

parseLatte :: String -> String -> Either ParseError LatteTree
parseLatte = parse topParser