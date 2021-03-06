module PreAsm (ErrorType(..), LogMsg, LogMsgType(..), PreAsmState(..), TemporaryId(..),
               ParameterId(..), Label(..), DataCell(..), FunctionUnit(..),
               OperandPair(..), BinaryOp'(..), Instruction(..), compilePreAsm) where
import Lang
import Data.Maybe (isJust)
import Control.Monad.State.Lazy
import Control.Monad.Identity
import Control.Monad.Writer.Lazy
import Control.Monad.Except
import Control.Monad
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Data.Hashable (Hashable)
import Prelude hiding (log)
import Data.List (intercalate)

type TemporaryId = Int
type ParameterId = Int
type Label = Int
data ExtendedDataCell = EGlobalVarCell Name | TemporaryCell TemporaryId | EParameterCell ParameterId
data DataCell = GlobalVarCell Name | ParameterCell ParameterId
data FunctionUnit = FunctionUnit Name [Instruction]

instance Show FunctionUnit where
    show (FunctionUnit name is) = "function " ++ name ++ " (" ++ (show $ length is) ++ " instructions) \n  " ++ (intercalate "\n   " $ map show is)

data OperandPair = TT TemporaryId TemporaryId | TM TemporaryId DataCell | MT DataCell TemporaryId
                    | TL TemporaryId Literal | ML DataCell Literal

showTid tid = "<" ++ (show tid) ++ ">"
instance Show DataCell where
    show (GlobalVarCell name) = "[global "++ name ++ "]"
    show (ParameterCell paramId) = "[param #" ++ (show paramId) ++ "]"

showOpPair :: OperandPair -> (String, String)
showOpPair (TT tid tid2) = ((showTid tid), (showTid tid2))
showOpPair (TM tid cell) = ((showTid tid), (show cell))
showOpPair (MT cell tid) = ((show cell), (showTid tid))
showOpPair (TL tid lit) = ((showTid tid), (show lit))
showOpPair (ML cell lit) = ((show cell), (show lit))


data BinaryOp' = Arith BinaryOp | Assign

data Instruction = Return TemporaryId
                   | FunctionCall Name [TemporaryId] TemporaryId
                   | JumpWithCompare Label CompareOp OperandPair
                   | Jump Label
                   | BinaryOperation BinaryOp' OperandPair
                   | UnaryOperation UnaryOp TemporaryId
                   | LabelMark Label
instance Show Instruction where
    show (Return tid) = "return " ++ (showTid tid)
    show (FunctionCall name args tid) = (showTid tid) ++ " = " ++ name ++ "(" ++ (intercalate ", " $ map (\x -> (showTid x)) args) ++ ")"
    show (JumpWithCompare label op opPair) = let (operand1, operand2) = showOpPair opPair
                                              in "jump if " ++ operand1 ++ " " ++ (show op) ++ " " ++ operand2 ++ "  to label" ++ (show label)
    show (Jump label) = "jump to label" ++ (show label)
    show (BinaryOperation Assign opPair) = let (operand1, operand2) = showOpPair opPair
                                            in operand1 ++ " = " ++ operand2
    show (BinaryOperation (Arith op) opPair) = let (operand1, operand2) = showOpPair opPair
                                                in operand1 ++ " = " ++ operand1 ++ " " ++ (show op) ++ " " ++ operand2
    show (UnaryOperation op tid) = (showTid tid) ++ " = " ++ (show op) ++ (showTid tid)
    show (LabelMark label) = "label" ++ (show label) ++ ":"



data LogMsgType = ErrMsg | WarnMsg | InfoMsg
    deriving Show
type LogMsg = (String, Maybe Name, LogMsgType)
data PreAsmState = PreAsmState { functions :: M.HashMap Name Int --FunctionName -> Parameter count
                               , globals :: S.HashSet Name
                               , parameters :: M.HashMap Name ParameterId
                               , locals :: M.HashMap Name (Maybe TemporaryId) -- VarName -> is_initialized
                               , scopes :: [[Name]]
                               , units :: [FunctionUnit]
                               , functionName :: Maybe Name
                               , temporaryCounter :: Int
                               , labelCounter :: Int
                               }

instance Show PreAsmState where
    show st = intercalate "\n" $ (map ("; global var " ++) $ S.toList $ globals st) ++ ["\n\n"] ++ (map show $ units st)


data ErrorType = VariableAlreadyDefined Name | VariableAlreadyDefinedInParams Name | VariableNotInitialized Name | CustomError String | VariableNotExist Name
        deriving (Show)

type Compilation a = StateT PreAsmState (ExceptT ErrorType (WriterT [LogMsg] Identity)) a
runCompilation :: Compilation a -> PreAsmState -> (Either ErrorType (a, PreAsmState), [LogMsg])
runCompilation comp st = runIdentity $ runWriterT $ runExceptT $ runStateT comp st


execCompilation comp st = let (res, log) = runCompilation comp st
                          in  (case res of
                                Left err -> Left err
                                Right (r, st) -> Right st, log)
evalCompilation comp st = let (res, log) = runCompilation comp st
                          in  (case res of
                                Left err -> Left err
                                Right (r, st) -> Right r, log)

compilePreAsm :: Program -> (Either ErrorType PreAsmState, [LogMsg])
compilePreAsm (Program gs) = execCompilation compile' initState
    where compile' = foldM_ (const compileGlobalDecl) () gs
          initState = PreAsmState { functions = M.empty
                                  , globals = S.empty
                                  , parameters = M.empty
                                  , locals = M.empty
                                  , scopes = []
                                  , units = []
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

allocLabel :: Compilation Label
allocLabel = do counter <- gets labelCounter
                modify $ \s -> s { labelCounter = counter + 1 }
                return counter
allocTemporary :: Compilation TemporaryId
allocTemporary = do counter <- gets temporaryCounter
                    modify $ \s -> s { temporaryCounter = counter + 1 }
                    return counter

setParameters = modifyParameters . const
setFunctionName = modifyFunctionName . const

addUnit unit = modify (\s -> s {units = units s ++ [unit]})

compileGlobalDecl :: GlobalDecl -> Compilation ()
compileGlobalDecl (GlobalVar (VarDecl name)) = modifyGlobals $ S.insert name
compileGlobalDecl (ExternFunction (ExternFunctionDecl name params)) = modifyFunctions $ M.insert name (length params)
-- @TODO add checking if function doesn't exist
compileGlobalDecl (Function (FunctionDecl name params stmt)) = do modifyFunctions $ M.insert name (length params)
                                                                  setParameters $ M.fromList $ zip params [1..]
                                                                  setFunctionName $ Just name
                                                                  is <- compileStmt stmt
                                                                  setFunctionName $ Nothing
                                                                  setParameters $ M.empty
                                                                  addUnit $ FunctionUnit name is

log :: String -> LogMsgType -> Compilation ()
log msg msgType = do fName <- gets functionName
                     tell [(msg, fName, msgType)]

logWarn = flip log WarnMsg
logErr = flip log ErrMsg
logInfo = flip log InfoMsg

cLookup :: (Eq a, Hashable a) => (PreAsmState -> M.HashMap a b) -> a -> Compilation (Maybe b)
cLookup mapF key = do map <- gets mapF
                      return $ M.lookup key map


addToScope name = modifyScopes $ \(h:t) -> (name : h) : t

remLocal name = do isInitialized <- gets $ isJust . (M.! name) . locals
                   if not isInitialized then logWarn $ "Unused variable : " ++ name
                                        else return ()
                   modifyLocals $ M.delete name

addLocal :: Name -> Compilation ()
addLocal name = do mParamId <- cLookup parameters name
                   if isJust $ mParamId
                      then throwError $ VariableAlreadyDefinedInParams name
                      else do loc <- cLookup locals name
                              case loc of
                                   Nothing -> (modifyLocals $ M.insert name Nothing) >> addToScope name
                                   Just _ -> throwError $ VariableAlreadyDefined name

getLocal :: Bool -> Name -> Compilation (Maybe TemporaryId)
getLocal needInited name = do loc <- cLookup locals name
                              case loc of
                                   Nothing -> return Nothing
                                   Just Nothing -> if needInited
                                                   then throwError $ VariableNotInitialized name
                                                   else do tid <- allocTemporary
                                                           modifyLocals $ M.insert name (Just tid)
                                                           return $ Just tid
                                   Just (Just tid) -> return $ Just tid

getVar :: Bool -> Name -> Compilation (Maybe ExtendedDataCell)
getVar needInited name = do mLoc <- getLocal needInited name
                            case mLoc of
                                Just tid -> return $ Just $ TemporaryCell tid
                                Nothing -> tryGlobOrParam name
                where tryGlobOrParam name = do globals' <- gets globals
                                               mParamId <- cLookup parameters name
                                               case mParamId of
                                                  Just paramId -> return $ Just $ EParameterCell paramId
                                                  _ -> if S.member name globals'
                                                          then return $ Just $ EGlobalVarCell name
                                                          else return Nothing
requireVar :: Bool -> Name -> Compilation ExtendedDataCell
requireVar needInited name = do mVar <- getVar needInited name
                                case mVar of
                                    Just cell -> return cell
                                    Nothing -> throwError $ VariableNotExist name

initScope = modifyScopes ([] :)
remScope = do scope <- fmap head $ gets scopes
              foldM_ (const remLocal) () scope
              modifyScopes tail

mapConcatM :: Monad m => (a -> m [b]) -> [a] -> m [b]
mapConcatM f (a:as) = do ss <- f a
                         rest <- mapConcatM f as
                         return $ ss ++ rest
mapConcatM f [] = return []

-- @TODO we can optimize this function even more
jumpWithCompare :: CompareOp -> Expr -> Expr -> Label -> Compilation [Instruction]
jumpWithCompare op (LiteralExpr lit) (LiteralExpr lit2) label = return $ if evalCompareOp op lit lit2
                                                                            then [Jump label]
                                                                            else []
jumpWithCompare op e1@(LiteralExpr lit) e2 label = jumpWithCompare (negateCompareOp op) e2 e1 label
jumpWithCompare op e1 (LiteralExpr lit) label = do (is1, tid1) <- compileExpr e1
                                                   return [JumpWithCompare label op $ TL tid1 lit]
jumpWithCompare op e1 e2 label = do (is1, tid1) <- compileExpr e1
                                    (is2, tid2) <- compileExpr e2
                                    return $ is1 ++ is2 ++ [JumpWithCompare label op $ TT tid1 tid2]

jumpIfCondition (Condition op e1 e2) = jumpWithCompare op e1 e2
jumpIfNotCondition (Condition op e1 e2) = jumpWithCompare (negateCompareOp op) e1 e2


compileStmt :: Stmt -> Compilation [Instruction]
compileStmt (BlockStmt stmts) = do initScope
                                   is <- foldM f [] stmts
                                   remScope
                                   return is
                             where f is stmt = fmap (is ++) $ compileStmt stmt
compileStmt (ExprStmt expr) = fmap fst $ compileExpr expr
compileStmt (ReturnStmt expr) = do (is, tid) <- compileExpr expr
                                   return $ is ++ [Return tid]
compileStmt (ConditionalStmt elifs elseStmt) = do endif <- allocLabel
                                                  elifs' <- mapConcatM (compileElif endif) elifs
                                                  elseStmt' <- compileStmt elseStmt
                                                  return $ elifs' ++ elseStmt' ++ [LabelMark endif]
        where compileElif endif (cond, stmt) = do endElif <- allocLabel
                                                  is1 <- jumpIfNotCondition cond endElif
                                                  is2 <- compileStmt stmt
                                                  return $ is1 ++ is2 ++ [Jump endif, LabelMark endElif]
compileStmt (LoopStmt cond stmt) = do end <- allocLabel
                                      start <- allocLabel
                                      is1 <- jumpIfNotCondition cond end
                                      is2 <- compileStmt stmt
                                      return $ [LabelMark start] ++ is1 ++ is2 ++ [Jump start, LabelMark end]
compileStmt EmptyStmt = return []
compileStmt (VarDeclStmt (VarDecl name)) = addLocal name >> return []


compileExpr :: Expr -> Compilation ([Instruction], TemporaryId)
compileExpr e = do tid <- allocTemporary
                   is <- compileExpr' e tid
                   return (is, tid)

composeOpPairT :: TemporaryId -> ExtendedDataCell -> OperandPair
composeOpPairT tid (TemporaryCell tid2) = TT tid tid2
composeOpPairT tid (EGlobalVarCell name) = TM tid $ GlobalVarCell name
composeOpPairT tid (EParameterCell name) = TM tid $ ParameterCell name
composeOpPairM :: ExtendedDataCell -> TemporaryId -> OperandPair
composeOpPairM (TemporaryCell tid) tid2 = TT tid tid2
composeOpPairM (EGlobalVarCell name) tid = MT (GlobalVarCell name) tid
composeOpPairM (EParameterCell name) tid = MT (ParameterCell name) tid


compileExpr' :: Expr -> TemporaryId -> Compilation [Instruction]
compileExpr' (AssignExpr name e) tid = do varCell <- requireVar False name
                                          is <- compileExpr' e tid
                                          return $ is ++ [BinaryOperation Assign $ composeOpPairM varCell tid]
compileExpr' (BinaryExpr op e1 e2) tid = do is1 <- compileExpr' e1 tid
                                            let bop = BinaryOperation (Arith op)
                                            case e2 of
                                                LiteralExpr lit -> return $ is1 ++ [bop $ TL tid lit]
                                                ReferenceExpr name -> do mTid <- getLocal True name
                                                                         case mTid of
                                                                            Nothing -> straightForward is1
                                                                            Just tid2 -> return $ is1 ++ [bop $ TT tid tid2]
                                                _ -> straightForward is1
                            where straightForward is1 = do tid2 <- allocTemporary
                                                           is2 <- compileExpr' e2 tid2
                                                           return (is1 ++ is2 ++ [BinaryOperation (Arith op) $ TT tid tid2])
compileExpr' (UnaryExpr op e) tid = do is <- compileExpr' e tid
                                       return $ is ++ [UnaryOperation op tid]
-- @TODO add checking function declaration (param count)
compileExpr' (CallExpr name argExprs) tid = do args <- mapM compileExpr argExprs
                                               let (iss, tids) = unzip args
                                               return $ (concat iss) ++ [FunctionCall name tids tid]
compileExpr' (ReferenceExpr name) tid = do varCell <- requireVar True name
                                           return $ [BinaryOperation Assign $ composeOpPairT tid varCell]
compileExpr' (LiteralExpr lit) tid = return $ [BinaryOperation Assign $ TL tid lit]
