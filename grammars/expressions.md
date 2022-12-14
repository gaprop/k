https://doc.rust-lang.org/reference/introduction.html

Expr ::= ExprWithoutBlock
       | ExprWithBlock

ExprWithoutBlock ::= OperatorExpr
                  |  LiteralExpr.
                  ...

LiteralExpr ::= CHAR_LITERAL
             |  STRING_LITERAL
             <!-- |  BYTE_LITERAL -->
             |  INTEGER_LITERAL
             |  FLOAT_LITERAL
             | true | false.
             ...

OperatorExpr ::= NegationExpr
              |  ArithmeticOrLogicalExpr
              |  ComparisonExpr
              |  LazyBooleanExpr
              |  AssignmentExpr
              |  CompundAssignExpr.
              ...

NegationExpr ::= "-" Expr
              |  "!" Expr.

ArithmeticOrLogicalExpr ::= Expr "+"  Expr
                         |  Expr "-"  Expr
                         |  Expr "*"  Expr
                         |  Expr "/"  Expr
                         |  Expr "%"  Expr
                         |  Expr "&"  Expr
                         |  Expr "|"  Expr
                         |  Expr "^"  Expr
                         |  Expr "<<" Expr
                         |  Expr ">>" Expr.

comparisonExpr ::= Expr "==" Expr
                |  Expr "!=" Expr
                |  Expr ">" Expr
                |  Expr "<" Expr
                |  Expr ">=" Expr
                |  Expr "<=" Expr.

LazyBooleanExpr ::= Expr "||" Expr
                 |  Expr "&&" Expr.

AssignmentExpr ::= Expr "=" Expr.

CompundAssignExpr ::= 
      Expr += Expr
    | Expr -= Expr
    | Expr \*= Expr
    | Expr /= Expr
    | Expr %= Expr
    | Expr &= Expr
    | Expr |= Expr
    | Expr ^= Expr
    | Expr <<= Expr
    | Expr >>= Expr.
