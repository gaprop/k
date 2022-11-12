module AST where

data Expr 
  = NoBlock ExprWithoutBlock
  -- | ExprBlock 

data ExprWithoutBlock 
  = Operator OperatorExpr
  | Literal LiteralExpr

data LiteralExpr 
  = CharLiteral Char
  | StringLiteral String
  | ByteLiteral Int -- Perhaps use a better representation
  | IntegerLiteral Integer
  | FloatLiteral Float
  | TrueVal | FalseVal
  deriving (Show)

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
