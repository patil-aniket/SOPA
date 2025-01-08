%:- use_rendering(svgtree).
:- table expression//1.
:- table commands//1.

% program ::= block
program(prgm(Block)) --> block(Block).

% block ::= start decls commands stop
block(blk(start, Decls, Commands, stop)) --> [kw(start)], decls(Decls), commands(Commands), [kw(stop)].

% decls ::= decl_list | []
decls(Decls) --> decl_list(Decls).
decls(decl([])) --> [].

% decl_list ::= decl ';' decl_list | decl ';'
decl_list(decl_blk(Decl, ';', Rest)) --> decl(Decl), [';'], decl_list(Rest).
decl_list(Decl) --> decl(Decl), [';'].

% decl ::= const identifier '=' assignments_allowed | var identifier
decl(decl(const, Id, '=', Assign)) --> [kw(const)], identifier(Id), ['='], assignments_allowed(Assign).
decl(decl(var, Id)) --> [kw(var)], identifier(Id).

assignments_allowed(Assign) --> expression(Assign).

% string_literal ::= '"' string_chars '"'
string_literal(string_literal(StringContent)) --> [string(StringContent)].

% commands ::= command ';' commands | command commands | empty
commands(Cmds) --> command_list(Cmds).
commands(cmd([])) --> [].

command_list(cmd_blk(Cmd, ';', Rest)) --> command_requiring_semicolon(Cmd), [';'], command_list(Rest).
command_list(Cmd) --> command_requiring_semicolon(Cmd), [';'].
command_list(cmd_blk(Cmd, Rest)) --> command_not_requiring_semicolon(Cmd), command_list(Rest).
command_list(Cmd) --> command_not_requiring_semicolon(Cmd).

% command_requiring_semicolon ::= identifier := expression | print_statement
command_requiring_semicolon(cmd(Id, ':=', Expr)) --> identifier(Id), [':='], expression(Expr).
command_requiring_semicolon(cmd(PrintStmt)) --> print_statement(PrintStmt).

% command_not_requiring_semicolon ::= if statement_list commands else commands endif
%                                   | repeat repeat_statement commands endrepeat
%                                   | unless statement_list commands until
%                                   | block
command_not_requiring_semicolon(cmd(if(Cond, ThenCmds, ElseCmds, endif))) --> [kw(if)], statement_list(Cond), commands(ThenCmds), [kw(else)], commands(ElseCmds), [kw(endif)].
command_not_requiring_semicolon(cmd(repeat(RepeatStmt, Cmds, endrepeat))) --> [kw(repeat)], repeat_statement(RepeatStmt), commands(Cmds), [kw(endrepeat)].
command_not_requiring_semicolon(cmd(unless(Cond, Cmds, until))) --> [kw(unless)], statement_list(Cond), commands(Cmds), [kw(until)].
command_not_requiring_semicolon(cmd(Block)) --> block(Block).

% Statement List
statement_list(Cond) --> ['('], expression(Cond), [')'].

% repeat_statement ::= '(' identifier ',' number ',' number ')'
repeat_statement(repeat_params(Id, From, To)) --> ['('], identifier(Id), [','], number(From), [','], number(To), [')'].

% print_statement ::= show expression | show string_literal
print_statement(show(Str)) --> [kw(show)], string_literal(Str).
print_statement(show(Expr)) --> [kw(show)], expression(Expr).

% expression ::= logical_or_expression '?' expression ':' expression | logical_or_expression
expression(ternary(Cond, Expr1, Expr2)) --> logical_or_expression(Cond), ['?'], expression(Expr1), [':'], expression(Expr2).
expression(Expr) --> logical_or_expression(Expr).

% logical_or_expression ::= logical_and_expression ('||' logical_and_expression)*
logical_or_expression(Expr) --> logical_and_expression(Left), logical_or_expression_rest(Left, Expr).

logical_or_expression_rest(Acc, Expr) --> ['||'], logical_and_expression(Right), { NewAcc = or(Acc, Right) }, logical_or_expression_rest(NewAcc, Expr).
logical_or_expression_rest(Acc, Acc) --> [].

% logical_and_expression ::= equality_expression ('&&' equality_expression)*
logical_and_expression(Expr) --> equality_expression(Left), logical_and_expression_rest(Left, Expr).

logical_and_expression_rest(Acc, Expr) --> ['&&'], equality_expression(Right), { NewAcc = and(Acc, Right) }, logical_and_expression_rest(NewAcc, Expr).
logical_and_expression_rest(Acc, Acc) --> [].

% equality_expression ::= relational_expression (('==' | '!=') relational_expression)*
equality_expression(Expr) --> relational_expression(Left), equality_expression_rest(Left, Expr).

equality_expression_rest(Acc, Expr) --> ['=='], relational_expression(Right), { NewAcc = eq(Acc, Right) }, equality_expression_rest(NewAcc, Expr).
equality_expression_rest(Acc, Expr) --> ['!='], relational_expression(Right), { NewAcc = ne(Acc, Right) }, equality_expression_rest(NewAcc, Expr).
equality_expression_rest(Acc, Acc) --> [].

% relational_expression ::= addition_expression (('<' | '<=' | '>' | '>=') addition_expression)*
relational_expression(Expr) --> addition_expression(Left), relational_expression_rest(Left, Expr).

relational_expression_rest(Acc, Expr) --> ['<'], addition_expression(Right), { NewAcc = lt(Acc, Right) }, relational_expression_rest(NewAcc, Expr).
relational_expression_rest(Acc, Expr) --> ['<='], addition_expression(Right), { NewAcc = le(Acc, Right) }, relational_expression_rest(NewAcc, Expr).
relational_expression_rest(Acc, Expr) --> ['>'], addition_expression(Right), { NewAcc = gt(Acc, Right) }, relational_expression_rest(NewAcc, Expr).
relational_expression_rest(Acc, Expr) --> ['>='], addition_expression(Right), { NewAcc = ge(Acc, Right) }, relational_expression_rest(NewAcc, Expr).
relational_expression_rest(Acc, Acc) --> [].

% addition_expression ::= multiplication_expression (('+' | '-') multiplication_expression)*
addition_expression(Expr) --> multiplication_expression(Left), addition_expression_rest(Left, Expr).

addition_expression_rest(Acc, Expr) --> ['+'], multiplication_expression(Right), { NewAcc = add(Acc, Right) }, addition_expression_rest(NewAcc, Expr).
addition_expression_rest(Acc, Expr) --> ['-'], multiplication_expression(Right), { NewAcc = sub(Acc, Right) }, addition_expression_rest(NewAcc, Expr).
addition_expression_rest(Acc, Acc) --> [].

% multiplication_expression ::= unary_expression (('*' | '/' | '%') unary_expression)*
multiplication_expression(Expr) --> unary_expression(Left), multiplication_expression_rest(Left, Expr).

multiplication_expression_rest(Acc, Expr) --> ['*'], unary_expression(Right), { NewAcc = mul(Acc, Right) }, multiplication_expression_rest(NewAcc, Expr).
multiplication_expression_rest(Acc, Expr) --> ['/'], unary_expression(Right), { NewAcc = div(Acc, Right) }, multiplication_expression_rest(NewAcc, Expr).
multiplication_expression_rest(Acc, Expr) --> ['%'], unary_expression(Right), { NewAcc = mod(Acc, Right) }, multiplication_expression_rest(NewAcc, Expr).
multiplication_expression_rest(Acc, Acc) --> [].

% unary_expression ::= ('!' | '-') unary_expression | primary_expression
unary_expression(not(Expr)) --> ['!'], unary_expression(Expr).
unary_expression(neg(Expr)) --> ['-'], unary_expression(Expr).
unary_expression(Expr) --> primary_expression(Expr).

% primary_expression ::= sqrt primary_expression | '(' expression ')' | identifier ':=' expression | identifier | number | true | false | string_literal
primary_expression(sqrt(Expr)) --> [kw(sqrt)], primary_expression(Expr).
primary_expression(Expr) --> ['('], expression(Expr), [')'].
primary_expression(assign(Id, Expr)) --> identifier(Id), [':='], expression(Expr).
primary_expression(Id) --> identifier(Id).
primary_expression(Num) --> number(Num).
primary_expression(true) --> [kw(true)].
primary_expression(false) --> [kw(false)].
primary_expression(StrLit) --> string_literal(StrLit).

% identifier ::= [Identifier], { atom(Identifier) }
identifier(id(Identifier)) --> [id(Identifier)].

% number ::= [Number], { number(Number) }
number(num(Number)) --> [num(Number)].
