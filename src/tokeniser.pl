% Deterministic Tokenizer that returns tokens as atoms or numbers

% Entry point: tokenize the input string into a list of tokens
tokenize(Input, Tokens) :-
    (   string(Input)
    ->  string_codes(Input, Codes)
    ;   atom(Input)
    ->  atom_codes(Input, Codes)
    ),
    phrase(tokens(Tokens), Codes).

% Tokens: Parses a sequence of tokens
tokens(Tokens) --> skip, tokens_nonempty(Tokens), !.
tokens([]) --> skip, [].

% Non-empty tokens: Parses at least one token
tokens_nonempty([Token|Tokens]) -->
    token(Token), skip,
    tokens_nonempty(Tokens).
tokens_nonempty([]) --> [].

% Skip: Consumes whitespace and comments
skip --> (whitespace_char ; comment_char), !, skip.
skip --> [].

% Whitespace character
whitespace_char --> [C], { is_whitespace(C) }.

% Whitespace characters
is_whitespace(9).   % tab
is_whitespace(10).  % line feed
is_whitespace(13).  % carriage return
is_whitespace(32).  % space

% Comment: Skips anything between '**' and '**'
comment_char --> "**", comment_content.

comment_content --> "**", !.
comment_content --> [_], comment_content.

% Token definitions with cuts to prevent backtracking
token(Token) --> keyword(Token), !.
token(Token) --> ids(Token), !.
token(Token) --> nums(Token), !.
token(Token) --> string(Token), !.
token(Token) --> operator(Token), !.
token(Token) --> symbol(Token), !.

% Keywords with cut to prevent backtracking
keyword(Keyword) -->
    word(Cs),
    {
        atom_codes(A, Cs),
        member(A, [start, stop, const, var, if, then, else, endif,
                   repeat, endrepeat, unless, until, show, sqrt,
                   true, false, not]),
        Keyword = kw(A)
    }, !.

% Identifiers that are not keywords, with cut to prevent backtracking
ids(id(Id)) -->
    [C], { is_lowercase(C) },
    identifier_chars(Cs),
    {
        atom_codes(TempId, [C|Cs]),
        \+ member(TempId, [start, stop, const, var, if, then, else, endif,
                           repeat, endrepeat, unless, until, show, sqrt,
                           true, false, not]),
        Id = TempId
    }, !.

identifier_chars([C|Cs]) -->
    [C], { is_alnum(C) ; C = 95 }, % 95 is '_'
    identifier_chars(Cs).
identifier_chars([]) --> [].

% Numbers
nums(num(N)) -->
    "-", digits(Ds),
    { number_codes(N1, Ds), N is -N1 }, !.
nums(num(N)) -->
    digits(Ds),
    { number_codes(N, Ds) }, !.

digits([D|Ds]) -->
    [D], { is_digit(D) },
    digits(Ds).
digits([D]) -->
    [D], { is_digit(D) }.

% Strings
string(string(S)) -->
    "\"", string_chars_token(Cs), "\"",
    { atom_codes(S, Cs) }, !.

string_chars_token([C|Cs]) -->
    [C], { C \= 34 }, % 34 is '"'
    string_chars_token(Cs).
string_chars_token([]) --> [].

% Operators: Match multi-character operators before single-character ones
operator(Op) -->
    multi_char_operator(OpCodes),
    { atom_codes(Op, OpCodes) }, !.

operator(Op) -->
    single_char_operator(C),
    { atom_codes(Op, [C]) }, !.

multi_char_operator(OpCodes) -->
    (   "==",   { OpCodes = "==" }
    ;   "<=",   { OpCodes = "<=" }
    ;   ">=",   { OpCodes = ">=" }
    ;   "!=",   { OpCodes = "!=" }
    ;   "&&",   { OpCodes = "&&" }
    ;   "||",   { OpCodes = "||" }
    ;   ":=",   { OpCodes = ":=" }
    ), !.

single_char_operator(C) -->
    [C],
    { memberchk(C, `+-*/%^<>=!?:`) }, !.

% Symbols
symbol(Sym) -->
    [C],
    { memberchk(C, `;(),`),
      char_code(Sym, C) }, !.

% Word: used for keywords and identifiers
word([C|Cs]) -->
    [C], { is_alpha(C) },
    word_chars(Cs).
word_chars([C|Cs]) -->
    [C], { is_alnum(C) ; C = 95 },
    word_chars(Cs).
word_chars([]) --> [].

% Character classification predicates
is_alpha(C) :-
    between(65, 90, C) ; % A-Z
    between(97, 122, C). % a-z

is_alnum(C) :-
    is_alpha(C) ;
    is_digit(C).

is_digit(C) :-
    between(48, 57, C). % 0-9

is_lowercase(C) :-
    between(97, 122, C). % a-z
