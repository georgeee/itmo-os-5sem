module Lang where


newtype Program = Program [GlobalDecl]

data GlobalDecl = Function FunctionDecl | GlobalVar VarDecl | ExternFunction ExternFunctionDecl
data FunctionDecl = FunctionDecl Name [Parameter] Stmt
data ExternFunctionDecl = ExternFunctionDecl Name [Parameter]
type Parameter = Name
type Name = String
data Stmt = BlockStmt [Stmt]
            | ExprStmt Expr
            | ReturnStmt Expr
            | ConditionalStmt [(Condition, Stmt)] Stmt
            | LoopStmt Condition Stmt
            | EmptyStmt
            | VarDeclStmt VarDecl
data VarDecl = VarDecl Name
data Expr = AssignExpr Name Expr
            | BinaryExpr BinaryOp Expr Expr
            | UnaryExpr UnaryOp Expr
            | CallExpr Name [Expr]
            | ReferenceExpr Name
            | LiteralExpr Literal
type Literal = Integer
data Condition = Condition CompareOp Expr Expr
data BinaryOp = Add | Subtract | Multiply | Divide | Mod
data UnaryOp = Negate
data CompareOp = Less | Greater | Equal | NotEqual

