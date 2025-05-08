```
Program             = TopLevel*
TopLevel            = FuncDecl | Stmt | ";";

FuncDecl            = "func" IDENTIFIER "(" Params? ")" ("returns" "(" Param ")")? Block
Params              = Param ("," Param)*
Param               = IDENTIFIER ":" Type
Block               = "{" Stmt* "}"
FuncCall            = IDENTIFIER "(" Args? ")"
Args                = Expr ("," Expr)*


Stmt                = ReturnStmt | PrintStmt | IfStmt | WhileStmt | ForStmt
                    | VarDecl | ExprStmt
ReturnStmt          = "return" Expr? ";"
PrintStmt           = "print" Expr ";"
IfStmt              = "if" "(" Expr ")" Block ("else" Block)?
WhileStmt           = "while" "(" Expr ")" Block
ForStmt             = "for" "(" IDENTIFIER ":=" Expr ";" Expr ")" Block
VarDecl             = Type IDENTIFIER (":=" Expr)? ";"
ExprStmt            = Expr ";"


Expr                = AssignExpr
AssignExpr          = IDENTIFIER ":=" AssignExpr | LogicalExpr
LogicalExpr         = ComparisonExpr (("and" | "or") ComparisonExpr)*
ComparisonExpr      = AdditiveExpr (("=" | "!=" | "<" | "<=" | ">" | ">=") AdditiveExpr)*
AdditiveExpr        = MultiplicativeExpr (("+" | "-") MultiplicativeExpr)*
MultiplicativeExpr  = PrimaryExpr (("*" | "/" | "%") PrimaryExpr)*
PrimaryExpr         = "(" Expr ")" | Literal | IDENTIFIER | UnaryExpr | FuncCall
UnaryExpr           = ("!" | "-") PrimaryExpr


Literal             = INT_LITERAL | FLOAT_LITERAL | STRING_LITERAL | BOOL_LITERAL | CHAR_LITERAL | "null"
Type                = ("mut")? ("u0" | "u8" | "u16" | "u32" | "u64" | "i8" | "i16" | "i32" | "i64" | "f64")



## NOTES:
+ The recursive descent parser format (top down) where the outermost grammar (Expr) works its way down into the nested subexpressions before reaching the leaves which are the primary. This is ordered in precedence.
+ Assignment is an expression with the lowest precedence, and it follows right associativity.
    + It becomes a statement when used within an ExprStmt


## Expansion of Expression Grammar to Display Precedence:
Expr                = AssignExpr
AssignExpr          = IDENTIFIER ":=" AssignExpr | LogicalOrExpr
LogicalOrExpr       = LogicalAndExpr ("or" LogicalAndExpr)*
LogicalAndExpr      = EqualityExpr ("and" EqualityExpr)*
EqualityExpr        = ComparisonExpr (("=" | "!=") ComparisonExpr)*
ComparisonExpr      = AdditiveExpr (("<" | "<=" | ">" | ">=") AdditiveExpr)*
AdditiveExpr        = MultiplicativeExpr (("+" | "-") MultiplicativeExpr)*
MultiplicativeExpr  = PrimaryExpr (("*" | "/" | "%") PrimaryExpr)*
PrimaryExpr         = "(" Expr ")" | Literal | IDENTIFIER | UnaryExpr | FuncCall
UnaryExpr           = ("!" | "-") PrimaryExpr
```
