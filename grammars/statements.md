
Statements ::= ";" <!-- Skal den her være her --> 
             | Item
             | LetStatement
             | ConstStatement
             | ExpressionStatement
             | Item

LetStatement ::= "let" IDENT ("=" Expr)? ";"

Item ::= Function
      |  ...

Function ::= "fn" IDENT "(" FunctionParameters? ")" BlockExpr <!-- Denne her er ikke færdig -->
          |  ...

<!-- PatternNoTopAlt  -- For til et andet tidspunkt -->
