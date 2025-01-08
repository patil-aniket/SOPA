%%%%%% Evaluator %%%%%%
% eval_program(+AST, -EnvOut)
eval_program(prgm(Block), EnvOut) :-
    eval_block(Block, [], EnvOut).

% eval_block(+BlockAST, +EnvIn, -EnvOut)
eval_block(blk(start, Decls, Commands, stop), EnvIn, EnvOut) :-
    eval_decls(Decls, EnvIn, EnvMid),
    eval_commands(Commands, EnvMid, EnvOut).

% eval_decls(+DeclsAST, +EnvIn, -EnvOut)
eval_decls(decl([]), EnvIn, EnvIn).

eval_decls(DeclListAST, EnvIn, EnvOut) :-
    eval_decl_list(DeclListAST, EnvIn, EnvOut).

% eval_decl_list(+DeclListAST, +EnvIn, -EnvOut)
eval_decl_list(decl_blk(DeclAST, ';', RestAST), EnvIn, EnvOut) :-
    eval_decl(DeclAST, EnvIn, EnvMid),
    eval_decl_list(RestAST, EnvMid, EnvOut).

eval_decl_list(DeclAST, EnvIn, EnvOut) :-
    eval_decl(DeclAST, EnvIn, EnvOut).

% eval_decl(+DeclAST, +EnvIn, -EnvOut)
eval_decl(decl(const, id(Id), '=', AssignAST), EnvIn, EnvOut) :-
    eval_expression(AssignAST, EnvIn, EnvMid, Value),
    ( member(Id=_, EnvIn) ->
        format('Error: variable ~w already declared~n', [Id]),
        fail
    ;
        EnvOut = [Id=const(Value)|EnvMid]
    ).

eval_decl(decl(var, id(Id)), EnvIn, EnvOut) :-
    ( member(Id=_, EnvIn) ->
        format('Error: variable ~w already declared~n', [Id]),
        fail
    ;
        EnvOut = [Id=var(uninitialized)|EnvIn]
    ).

% eval_commands(+CommandsAST, +EnvIn, -EnvOut)
eval_commands(cmd([]), EnvIn, EnvIn).

eval_commands(CmdListAST, EnvIn, EnvOut) :-
    eval_command_list(CmdListAST, EnvIn, EnvOut).

% eval_command_list(+CmdListAST, +EnvIn, -EnvOut)
eval_command_list(cmd_blk(CmdAST, ';', RestAST), EnvIn, EnvOut) :-
    eval_command(CmdAST, EnvIn, EnvMid),
    eval_command_list(RestAST, EnvMid, EnvOut).

eval_command_list(cmd_blk(CmdAST, RestAST), EnvIn, EnvOut) :-
    eval_command(CmdAST, EnvIn, EnvMid),
    eval_command_list(RestAST, EnvMid, EnvOut).

eval_command_list(CmdAST, EnvIn, EnvOut) :-
    eval_command(CmdAST, EnvIn, EnvOut).

% eval_command(+CmdAST, +EnvIn, -EnvOut)
% Assignment
eval_command(cmd(id(Id), ':=', ExprAST), EnvIn, EnvOut) :-
    eval_expression(ExprAST, EnvIn, EnvMid, Value),
    update_variable(Id, Value, EnvMid, EnvOut).

% Print Statement
eval_command(cmd(PrintStmtAST), EnvIn, EnvOut) :-
    eval_print_statement(PrintStmtAST, EnvIn),
    EnvOut = EnvIn.

% If Statement
eval_command(cmd(if(CondAST, ThenCmdsAST, ElseCmdsAST, endif)), EnvIn, EnvOut) :-
    eval_expression(CondAST, EnvIn, EnvMid, BoolValue),
    ( BoolValue == true ->
        eval_commands(ThenCmdsAST, EnvMid, EnvOut)
    ;
        eval_commands(ElseCmdsAST, EnvMid, EnvOut)
    ).

% Repeat Loop
eval_command(cmd(repeat(repeat_params(id(Id), num(From), num(To)), CmdsAST, endrepeat)), EnvIn, EnvOut) :-
    eval_expression(num(From), EnvIn, EnvMid1, FromValue),
    eval_expression(num(To), EnvMid1, EnvMid2, ToValue),
    numlist(FromValue, ToValue, LoopValues),
    eval_repeat_loop(LoopValues, Id, CmdsAST, EnvMid2, EnvOut).

% Evaluation of 'unless' as a loop
eval_command(cmd(unless(CondAST, CmdsAST, until)), EnvIn, EnvOut) :-
    eval_unless_loop(CondAST, CmdsAST, EnvIn, EnvOut).

% Block
eval_command(cmd(BlockAST), EnvIn, EnvOut) :-
    eval_block(BlockAST, EnvIn, EnvOut).

% Helper predicate to handle the looping behavior
eval_unless_loop(CondAST, CmdsAST, EnvIn, EnvOut) :-
    eval_expression(CondAST, EnvIn, EnvMid, BoolValue),
    (BoolValue == true ->
        eval_commands(CmdsAST, EnvMid, EnvAfterCmds),
        eval_unless_loop(CondAST, CmdsAST, EnvAfterCmds, EnvOut)
    ;
        EnvOut = EnvMid
    ).

% eval_repeat_loop(+Values, +Id, +CmdsAST, +EnvIn, -EnvOut)
eval_repeat_loop([], _Id, _CmdsAST, EnvIn, EnvIn).

eval_repeat_loop([Value|RestValues], Id, CmdsAST, EnvIn, EnvOut) :-
    update_or_add_variable(Id, Value, EnvIn, EnvMid),
    eval_commands(CmdsAST, EnvMid, EnvAfterCmds),
    eval_repeat_loop(RestValues, Id, CmdsAST, EnvAfterCmds, EnvOut).

% update_or_add_variable(+Id, +Value, +EnvIn, -EnvOut)
update_or_add_variable(Id, Value, EnvIn, EnvOut) :-
    ( select(Id=var(_), EnvIn, RestEnv) ->
        EnvOut = [Id=var(Value)|RestEnv]
    ;
        EnvOut = [Id=var(Value)|EnvIn]
    ).

% eval_print_statement(+PrintStmtAST, +EnvIn)
eval_print_statement(show(string_literal(StringContent)), _EnvIn) :-
    writeln(StringContent), !.

eval_print_statement(show(ExprAST), EnvIn) :-
    eval_expression(ExprAST, EnvIn, _EnvOut, Value),
    writeln(Value).

% eval_expression(+ExprAST, +EnvIn, -EnvOut, -Value)
% Ternary Expression
eval_expression(ternary(CondAST, Expr1AST, Expr2AST), EnvIn, EnvOut, Value) :-
    eval_expression(CondAST, EnvIn, EnvMid, BoolValue),
    ( BoolValue == true ->
        eval_expression(Expr1AST, EnvMid, EnvOut, Value)
    ;
        eval_expression(Expr2AST, EnvMid, EnvOut, Value)
    ).

% Logical OR
eval_expression(or(LeftAST, RightAST), EnvIn, EnvOut, Value) :-
    eval_expression(LeftAST, EnvIn, EnvMid, LeftValue),
    ( LeftValue == true ->
        Value = true,
        EnvOut = EnvMid
    ;
        eval_expression(RightAST, EnvMid, EnvOut, Value)
    ).

% Logical AND
eval_expression(and(LeftAST, RightAST), EnvIn, EnvOut, Value) :-
    eval_expression(LeftAST, EnvIn, EnvMid, LeftValue),
    ( LeftValue == false ->
        Value = false,
        EnvOut = EnvMid
    ;
        eval_expression(RightAST, EnvMid, EnvOut, Value)
    ).

% Equality
eval_expression(eq(LeftAST, RightAST), EnvIn, EnvOut, Value) :-
    eval_expression(LeftAST, EnvIn, EnvMid, LeftValue),
    eval_expression(RightAST, EnvMid, EnvOut, RightValue),
    ( LeftValue == RightValue -> Value = true ; Value = false ).

% Not Equal
eval_expression(ne(LeftAST, RightAST), EnvIn, EnvOut, Value) :-
    eval_expression(LeftAST, EnvIn, EnvMid, LeftValue),
    eval_expression(RightAST, EnvMid, EnvOut, RightValue),
    ( LeftValue \== RightValue -> Value = true ; Value = false ).

% Less Than
eval_expression(lt(LeftAST, RightAST), EnvIn, EnvOut, Value) :-
    eval_expression(LeftAST, EnvIn, EnvMid, LeftValue),
    eval_expression(RightAST, EnvMid, EnvOut, RightValue),
    ( LeftValue < RightValue -> Value = true ; Value = false ).

% Less Than or Equal
eval_expression(le(LeftAST, RightAST), EnvIn, EnvOut, Value) :-
    eval_expression(LeftAST, EnvIn, EnvMid, LeftValue),
    eval_expression(RightAST, EnvMid, EnvOut, RightValue),
    ( LeftValue =< RightValue -> Value = true ; Value = false ).

% Greater Than
eval_expression(gt(LeftAST, RightAST), EnvIn, EnvOut, Value) :-
    eval_expression(LeftAST, EnvIn, EnvMid, LeftValue),
    eval_expression(RightAST, EnvMid, EnvOut, RightValue),
    ( LeftValue > RightValue -> Value = true ; Value = false ).

% Greater Than or Equal
eval_expression(ge(LeftAST, RightAST), EnvIn, EnvOut, Value) :-
    eval_expression(LeftAST, EnvIn, EnvMid, LeftValue),
    eval_expression(RightAST, EnvMid, EnvOut, RightValue),
    ( LeftValue >= RightValue -> Value = true ; Value = false ).

% Addition
eval_expression(add(LeftAST, RightAST), EnvIn, EnvOut, Value) :-
    eval_expression(LeftAST, EnvIn, EnvMid, LeftValue),
    eval_expression(RightAST, EnvMid, EnvOut, RightValue),
    Value is LeftValue + RightValue.

% Subtraction
eval_expression(sub(LeftAST, RightAST), EnvIn, EnvOut, Value) :-
    eval_expression(LeftAST, EnvIn, EnvMid, LeftValue),
    eval_expression(RightAST, EnvMid, EnvOut, RightValue),
    Value is LeftValue - RightValue.

% Multiplication
eval_expression(mul(LeftAST, RightAST), EnvIn, EnvOut, Value) :-
    eval_expression(LeftAST, EnvIn, EnvMid, LeftValue),
    eval_expression(RightAST, EnvMid, EnvOut, RightValue),
    Value is LeftValue * RightValue.

% Division
eval_expression(div(LeftAST, RightAST), EnvIn, EnvOut, Value) :-
    eval_expression(LeftAST, EnvIn, EnvMid, LeftValue),
    eval_expression(RightAST, EnvMid, EnvOut, RightValue),
    Value is LeftValue / RightValue.

% Modulo
eval_expression(mod(LeftAST, RightAST), EnvIn, EnvOut, Value) :-
    eval_expression(LeftAST, EnvIn, EnvMid, LeftValue),
    eval_expression(RightAST, EnvMid, EnvOut, RightValue),
    Value is mod(LeftValue, RightValue).

% Unary Not
eval_expression(not(ExprAST), EnvIn, EnvOut, Value) :-
    eval_expression(ExprAST, EnvIn, EnvOut, InnerValue),
    ( InnerValue == true -> Value = false ; Value = true ).

% Unary Negation
eval_expression(neg(ExprAST), EnvIn, EnvOut, Value) :-
    eval_expression(ExprAST, EnvIn, EnvOut, InnerValue),
    Value is -InnerValue.

% Square Root
eval_expression(sqrt(ExprAST), EnvIn, EnvOut, Value) :-
    eval_expression(ExprAST, EnvIn, EnvOut, InnerValue),
    Value is sqrt(InnerValue).

% Assignment within Expression
eval_expression(assign(id(Id), ExprAST), EnvIn, EnvOut, Value) :-
    eval_expression(ExprAST, EnvIn, EnvMid, Value),
    update_variable(Id, Value, EnvMid, EnvOut).

% Boolean literals
eval_expression(true, EnvIn, EnvIn, true).
eval_expression(false, EnvIn, EnvIn, false).

% Variable
eval_expression(id(Id), EnvIn, EnvIn, Value) :-
    ( member(Id=var(Value), EnvIn) ->
        true
    ; member(Id=const(Value), EnvIn) ->
        true
    ; format('Error: variable "~w" not found~n', [Id]),
      fail
    ).

% Number
eval_expression(num(Number), EnvIn, EnvIn, Number).

% String literals
eval_expression(string_literal(StringContent), EnvIn, EnvIn, StringContent).

% update_variable(+Id, +Value, +EnvIn, -EnvOut)
update_variable(Id, Value, EnvIn, EnvOut) :-
    ( select(Id=var(_), EnvIn, RestEnv) ->
        EnvOut = [Id=var(Value)|RestEnv]
    ; select(Id=const(_), EnvIn, _RestEnv) ->
        format('Error: cannot assign to constant variable "~w~n"', [Id]),
        fail
    ; 
        format('Error: variable "~w" not declared~n', [Id]),
        fail
    ).
