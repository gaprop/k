module AST where

-- FIXME: This can propably be merged
data Expr 
  = NoBlock ExprWithoutBlock
  -- | Block ExprWithBlock

data ExprWithoutBlock 
  = Operator OperatorExpr
  | Literal LiteralExpr
  | Function Ident FuncParams [Stmt] -- Here we make the block expr explicit, however it could be moved into its own data type.

-- data ExprWithBlock
  -- = BlockExpr [Stmt] ExprWithoutBlock -- Perhaps we should just remove the last expr so that the last element in a block has to be a return statement
  -- deriving (Show)

data LiteralExpr 
  = CharLiteral Char
  | StringLiteral String
  | ByteLiteral Int -- Perhaps use a better representation
  | IntegerLiteral Integer
  | FloatLiteral Float
  | TrueVal | FalseVal

data OperatorExpr 
  = Negation NegExpr
  | ArithmeticOrLogical ArithmeticOrLogicalExpr 
  | Comparison ComparisonExpr
  | LazyBoolean LazyBooleanExpr

data NegExpr
  = Negative Expr
  | Not Expr

data ArithmeticOrLogicalExpr -- Perhaps not create this and just put it into the operator Expr structure
  = Plus       Expr Expr
  | Minus      Expr Expr
  | Times      Expr Expr
  | Div        Expr Expr
  | Modulo     Expr Expr
  | ByteAnd    Expr Expr
  | ByteOr     Expr Expr
  | ByteXor    Expr Expr
  | LeftShift  Expr Expr
  | RightShift Expr Expr

data ComparisonExpr 
  = Equal     Expr Expr
  | NotEqual  Expr Expr
  | Less      Expr Expr
  | LessEq    Expr Expr
  | Greater   Expr Expr
  | GreaterEq Expr Expr
  -- | Greater  Expr Expr
  -- FIXME: We can do these smarter

data LazyBooleanExpr
  = Or  Expr Expr
  | And Expr Expr

data AssignmentExpr = Assign Expr Expr

data CompAssignmentExpr
  = CompPlus       Expr Expr
  | CompMinus      Expr Expr
  | CompTimes      Expr Expr
  | CompDiv        Expr Expr
  | CompModulo     Expr Expr
  | CompByteAnd    Expr Expr
  | CompByteOr     Expr Expr
  | CompByteXor    Expr Expr
  | CompLeftShift  Expr Expr
  | CompRightShift Expr Expr

-- Statements
type Ident = String
type FuncParams = [(Ident, Maybe Type)]
type Program = [Stmt]

data Stmt
  -- = Function Ident FuncParams [Stmt] -- Here we make the block expr explicit, however it could be moved into its own data type.
  = LetStmt Ident (Maybe Type) Expr
  | ConstStmt Ident (Maybe Type) Expr
  | ExprStmt Expr

data Type 
  = TChar
  | TString
  | TInt
  | TFloat
  | TBool
  | TFun
