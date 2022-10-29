module AST
    ( someFunc
    ) where

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

data OperatorExpr 
  = Negation NegationExpr
  | ArithmeticOrLogical ArithmeticOrLogicalExpr 
  | Comparison ComparisonExpr
  | LazyBoolean LazyBooleanExpr

data NegationExpr
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
  = Equal    Expr Expr
  | NotEqual Expr Expr
  | Less     Expr Expr
  | Greater  Expr Expr
  | Greater  Expr Expr
