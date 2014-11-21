module PreAsm where
import Lang
import Data.Maybe (isJust)
import Control.Monad.State.Lazy
import Control.Monad
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Prelude hiding (log)

type TemporaryId = Int
type Label = Int
data DataCell = GlobalVarCell Name | TemporaryCell TemporaryId | ParemeterCell Name
data FunctionUnit = FunctionUnit Name [Instruction]

data Instruction = Return TemporaryId | FunctionCall Name [TemporaryId] TemporaryId
                   | JumpWithCompare Label CompareOp TemporaryId TemporaryId
                   | JumpWithCompareTo0 Label CompareOp TemporaryId
                   | Jump Label
                   | AssignWithBinaryOp BinaryOp TemporaryId TemporaryId
                   | AssignWithUnaryOp UnaryOp
                   | AssignLiteral Literal TemporaryId
                   | Assign TemporaryId TemporaryId
                   | LabelMark Label

data LogMsgType = ErrMsg | WarnMsg | InfoMsg
type LogMsg = (String, Maybe Name, LogMsgType)
data PreAsmState = PreAsmState { functions :: M.HashMap Name Int --FunctionName -> Parameter count
                               , globals :: S.HashSet Name
                               , parameters :: S.HashSet Name
                               , locals :: M.HashMap Name (Maybe TemporaryId) -- VarName -> is_initialized
                               , scopes :: [[Name]]
                               , units :: [FunctionUnit]
                               , msgLog :: [LogMsg]
                               , fatalErrorOccured :: Bool
                               , functionName :: Maybe Name
                               , temporaryCounter :: Int
                               , labelCounter :: Int
                               }

compile :: Program -> Either [LogMsg] ([LogMsg], [FunctionUnit])
compile (Program gs) = evalState compile' initState
    where compile' = do foldM_ (const compileGlobalDecl) () gs
                        errOccured <- gets fatalErrorOccured
                        if errOccured then do msgs <- gets msgLog
                                              return $ Left $ reverse msgs
                                      else do res <- gets units
                                              msgs <- gets msgLog
                                              return $ Right (reverse msgs, reverse res)
          initState = PreAsmState { functions = M.empty
                                  , globals = S.empty
                                  , parameters = S.empty
                                  , locals = M.empty
                                  , scopes = []
                                  , units = []
                                  , msgLog = []
                                  , fatalErrorOccured = False
                                  , functionName = Nothing
                                  , temporaryCounter = 0
                                  , labelCounter = 0
                                  }

modifyFunctions f = modify $ \s -> s { functions = f $ functions s}
modifyGlobals f = modify $ \s -> s { globals = f $ globals s}
modifyScopes f = modify $ \s -> s { scopes = f $ scopes s}
modifyLocals f = modify $ \s -> s { locals = f $ locals s}
modifyFunctionName f = modify $ \s -> s { functionName = f $ functionName s}
modifyParameters f = modify $ \s -> s { parameters = f $ parameters s}

allocLabel :: State PreAsmState Label
allocLabel = do counter <- gets labelCounter
                modify $ \s -> s { labelCounter = counter + 1 }
                return counter
allocTemporary :: State PreAsmState TemporaryId
allocTemporary = do counter <- gets temporaryCounter
                    modify $ \s -> s { temporaryCounter = counter + 1 }
                    return counter

setParameters = modifyParameters . const
setFunctionName = modifyFunctionName . const

addUnit unit = modify (\s -> s {units = unit : units s})

compileGlobalDecl :: GlobalDecl -> State PreAsmState ()
compileGlobalDecl (GlobalVar (VarDecl name)) = modifyGlobals $ S.insert name
compileGlobalDecl (ExternFunction (ExternFunctionDecl name params)) = modifyFunctions $ M.insert name (length params)
compileGlobalDecl (Function (FunctionDecl name params stmt)) = do modifyFunctions $ M.insert name (length params)
                                                                  setParameters $ S.fromList params
                                                                  setFunctionName $ Just name
                                                                  is <- compileStmt stmt
                                                                  setFunctionName $ Nothing
                                                                  setParameters $ S.empty
                                                                  errOccured <- gets fatalErrorOccured
                                                                  if not errOccured then addUnit $ FunctionUnit name is
                                                                                    else return ()

log :: String -> LogMsgType -> State PreAsmState ()
log msg msgType = do fName <- gets functionName
                     modify $ \s -> s { msgLog = (msg, fName, msgType) : msgLog s }

logWarn = flip log WarnMsg
logErr = flip log ErrMsg
logInfo = flip log InfoMsg

reportFatal :: State PreAsmState ()
reportFatal = modify $ \s -> s {fatalErrorOccured = True }

warnUnusedVariable name = logWarn $ "Unused variable : " ++ name
errorVariableAlreadyDefined name = logErr $ "Variable " ++ name ++ " is already defined in function"

addToScope name = modifyScopes $ \(h:t) -> (name : h) : t

remLocal name = do isInitialized <- gets $ isJust . (M.! name) . locals
                   if not isInitialized then warnUnusedVariable name
                                        else return ()
                   modifyLocals $ M.delete name
addLocal name = do locs <- gets locals
                   case M.lookup name locs of
                        Nothing -> do modifyLocals $ M.insert name Nothing
                                      addToScope name
                        Just _ -> do errorVariableAlreadyDefined name
                                     reportFatal

initScope = modifyScopes ([] :)
remScope = do scope <- fmap head $ gets scopes
              foldM_ (const remLocal) () scope
              modifyScopes tail


compileStmt :: Stmt -> State PreAsmState [Instruction]
compileStmt (BlockStmt stmts) = do initScope
                                   foldM_ f [] stmts
                                   remScope
                                   return []
                             where f is stmt = fmap (is ++) $ compileStmt stmt
compileStmt (ExprStmt expr) = fmap fst $ compileExpr expr
compileStmt (ReturnStmt expr) = do (is, tid) <- compileExpr expr
                                   return $ is ++ [Return tid]
compileStmt (ConditionalStmt elifs elseStmt) = do endifLabel <- allocLabel
                                                  elifs' <- foldM (\a b -> a ++ (compileElif endifLabel b)) [] elifs
                                                  
                                                  


compileExpr :: Expr -> State PreAsmState ([Instruction], TemporaryId)
compileExpr = undefined


