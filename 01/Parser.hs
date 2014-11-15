module Parser where

import Lang
import Printable
import Text.Parsec
import Text.Parsec.Char

program = do spaces
             res <- (many $ do gd <- globalDecl
                               spaces
                               return gd)
             eof
             return $ Program res

globalDecl = (functionDecl >>= return . Function)
             <|> (varDecl >>= return . GlobalVar)
             <?> "global decl"
spaces1 = skipMany1 space

varDecl = do string "var"
             spaces1
             varName <- name
             return $ VarDecl varName

functionDecl = do string "function"
                  spaces1
                  fName <- name
                  spaces
                  parameters <- parameterList
                  spaces
                  body <- blockStmt
                  return $ FunctionDecl fName parameters body

parameterList = do char '('
                   spaces
                   res <- name `sepBy` (spaces >> char ',' >> spaces)
                   spaces
                   char ')'
                   return res

name = do first <- lower
          rest <- many alphaNum
          return (first : rest)

stmt = blockStmt <|> returnStmt <|> conditionalStmt <|> loopStmt <|> emptyStmt <|> varDeclStmt <|> exprStmt <?> "statement"

blockStmt = do char '{'
               spaces
               stmts <- many (do { s <- stmt; spaces; return s; })
               char '}'
               return $ BlockStmt stmts

exprStmt = do e <- expr
              spaces >> char ';'
              return $ ExprStmt e

returnStmt = do string "return" >> spaces1
                e <- expr
                spaces >> char ';'
                return $ ReturnStmt e

conditionalStmt = do string "if" >> spaces
                     major <- branch
                     (hasElse, rest) <- elseifBranches-- (spaces >> string "else" >> spaces >> string "if" >> spaces)
                     elseBranch <- if hasElse then stmt else return EmptyStmt
                     return $ ConditionalStmt (major:rest) elseBranch
               where branch = do char '(' >> spaces
                                 cond <- condition
                                 spaces >> char ')' >> spaces
                                 stmt >>= return . (,) cond
                     elseifBranches = elseifBranches' <|> return (False, [])
                     elseifBranches' = do spaces >> string "else" >> spaces
                                          elseifBranch <|> return (True, [])
                     elseifBranch = do string "if" >> spaces
                                       s <- branch
                                       (b, rest) <- elseifBranches'
                                       return (b, s:rest)

loopStmt = do string "while"
              spaces >> char '(' >> spaces
              cond <- condition
              spaces >> char ')' >> spaces
              stmt >>= return . LoopStmt cond

emptyStmt = char ';' >> return EmptyStmt
varDeclStmt = do vd <- varDecl
                 spaces >> char ';'
                 return $ VarDeclStmt vd

condition = do e1 <- expr
               spaces
               (op, e2) <- testOp "<" Less
                           <|> testOp ">" Greater
                           <|> testOp "==" Equal
                           <|> testOp "!=" NotEqual
               return $ Condition op e1 e2
         where testOp s op = string s >> spaces >> expr >>= return . (,) op

argList = do char '(' >> spaces
             res <- expr `sepBy` (spaces >> char ',' >> spaces)
             spaces
             char ')'
             return res

literalExpr = (char '-' >> integer >>= \x -> return $ LiteralExpr (-x))
              <|> (integer >>= return . LiteralExpr)
    where integer = many1 digit >>= return . (read :: String -> Integer)

parenExpr = do char '(' >> spaces
               e <- expr
               spaces >> char ')'
               return e

valueExpr = (do name <- name
                spaces
                try (argList >>= return . CallExpr name)
                    <|> try (char '=' >> spaces >> expr >>= return . AssignExpr name)
                    <|> (return $ ReferenceExpr name)
            ) <|> literalExpr <|> parenExpr <?> "value expr"

binExprLevel exprParser ops = do e1 <- exprParser
                                 spaces
                                 foldl1 (<|>) (map (binExpr e1) ops) <|> return e1
                           where binExpr e1 (ch, op) = do char ch >> spaces
                                                          e2 <- binExprLevel exprParser ops
                                                          return $ BinaryExpr op e1 e2

level0Expr = binExprLevel level1Expr [ ('+', Add), ('-', Subtract) ]
level1Expr = binExprLevel unaryExpr [ ('*', Multiply), ('/', Divide), ('%', Mod) ]

unaryExpr = (char '-' >> spaces >> valueExpr >>= return . UnaryExpr Negate)
            <|> valueExpr

expr = level0Expr

parseProgram :: String -> Either ParseError Program
parseProgram = parse program "(unknown src)"

test :: (Printable a) => SourceName -> Parsec String () a -> IO ()
test f p = readFile f >>= test'' f p


test' :: (Printable a) => Parsec String () a -> String -> IO ()
test' = test'' "(unknown)"

test'' :: (Printable a) => SourceName -> Parsec String () a -> String -> IO ()
test'' f p s = putStrLn $ case parse p f s of
                Left err -> "Error " ++ show err
                Right res -> Printable.print 0 res
