```
Program             = TopLevel*
TopLevel            = FuncDecl | Stmt | ";";

FuncDecl            = "func" IDENTIFIER "(" Params? ")" ("returns" "(" Param ")")? Block
Params              = Param ("," Param)*
Param               = IDENTIFIER ":" Type
Block               = "{" Stmt* "}"
FuncCall            = IDENTIFIER "(" Args? ")"
Args                = Expr ("," Expr)*


Stmt                = ReturnStmt | PrintStmt | IfStmt | WhileStmt | ForStmt | VarDecl | ExprStmt | Block | "break" | "continue"
ReturnStmt          = "return" Expr? ";"
PrintStmt           = "print" Expr ";"
IfStmt              = "if" "(" Expr ")" Block ("else" Block)?
WhileStmt           = "while" "(" Expr ")" Block
ForStmt             = "for" "(" (VarDecl | ExprStmt | ";") Expr? ";" Expr? ")" Block
VarDecl             = Type IDENTIFIER (":=" Expr)? ("," IDENTIFIER (":=" Expr)?)* ";"
ExprStmt            = Expr ";"


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


Literal             = INT_LITERAL | FLOAT_LITERAL | STRING_LITERAL | CHAR_LITERAL | "null" | "true" | "false"
Type                = ("mut")? ("u0" | "u8" | "u16" | "u32" | "u64" | "i8" | "i16" | "i32" | "i64" | "f64")
```
#### NOTES:
+ The recursive descent parser format (top down) where the outermost grammar (Expr) works its way down into the nested subexpressions before reaching the leaves which are the primary. This is ordered in precedence.
    + Thus the leaf nodes are the operations we must operate on first, before the others (post-order traversal)
    + Will have to handle the nodes as operations in reverse polish
    + (5 + 2 + 3) ==> (5 + 2) + 3
    + (1 + 2 * 3) ==> 1 + (2 * 3)
    + (3 * (8 + 2)) ==> 3 * (8 + 2)
    + (a := b := c) ==> a := (b := c)
+ Assignment is an expression with the lowest precedence, and it follows right associativity.
    + It becomes a statement when used within an ExprStmt
