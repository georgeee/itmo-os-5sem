module Printable(Printable(..)) where
import Prelude hiding (print)
import Lang
import Data.List

type Indent = Int

class Printable a where
    print :: Indent -> a -> String

placeIndent i = foldl (++) "" $ take i $ repeat "   "
newLine i = "\n" ++ placeIndent i

instance Printable Program where
    print i (Program gs) = intercalate (newLine i) $ map (print i) gs
instance Printable GlobalDecl where
    print i (Function f) = print i f
    print i (GlobalVar v) = print i v
instance Printable FunctionDecl where
    print i (FunctionDecl name ps stmt) = "function " ++ name ++ "(" ++ (intercalate ", " $ map id ps) ++ ") " ++ (print i stmt)
instance Printable Stmt where
    print i (BlockStmt stmts) = "{" ++ delim ++ (intercalate delim $ map (print $ i + 1) stmts) ++ (newLine i) ++ "}"
        where delim = newLine $ i + 1
    print i (ExprStmt expr) = print (i + 1) expr ++ ";"
    print i (ReturnStmt expr) = "return " ++ print (i + 1) expr ++ ";"
    print i (ConditionalStmt cs stmt) = (intercalate (newLine i ++ "else ") $ map branch cs) ++ elseBranch stmt
                                      where elseBranch EmptyStmt = ""
                                            elseBranch stmt = newLine i ++ "else " ++ print i stmt
                                            branch (cond, stmt) = "if (" ++ print (i + 1) cond ++ ") " ++ print i stmt
    print i (LoopStmt cond stmt) = "while (" ++ print (i + 1) cond ++ ") " ++ print i stmt
    print _ EmptyStmt = ";"
    print i (VarDeclStmt varDecl) = print i varDecl ++ ";"

instance Printable Expr where
    print i (AssignExpr name expr) = "(" ++ name ++ " = " ++ print i expr ++ ")"
    print i (BinaryExpr op e1 e2) =  "(" ++ print i e1 ++ " " ++ print i op ++ " " ++ print i e2 ++ ")"
    print i (UnaryExpr op e)  =  "(" ++ print i op ++ print i e ++ ")"
    print i (CallExpr name es) = name ++ "(" ++ (intercalate ", " $ map (print i) es) ++ ")"
    print _ (ReferenceExpr name) = name
    print i (LiteralExpr literal) = print i literal
instance Printable Integer where
    print _ = show
instance Printable Condition where
    print i (Condition op e1 e2) = print i e1 ++ " " ++ print i op ++ " " ++ print i e2
instance Printable BinaryOp where
    print _ Add = "+"
    print _ Subtract = "-"
    print _ Multiply = "*"
    print _ Divide = "/"
    print _ Mod  = "%"
instance Printable UnaryOp where
    print _ Negate = "-"
instance Printable CompareOp where
    print _ Less = "<"
    print _ Greater = ">"
    print _ Equal = "=="
    print _ NotEqual = "!="
instance Printable VarDecl where
    print _ (VarDecl name) = "var " ++ name

