:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

%% LEXER PART %%

program_tokenized(Tokens) --> source_start, sequence(token, Tokens), blanks.

source_start, ` ` --> ``.

token(bare_word(Bare_Word_Atom)) --> blank, blanks, csym(Bare_Word_Atom).
token(number(Number)) --> blank, blanks, number(Number).
token(char(Char_Code)), ` ` --> blanks, `'`, char_content(Char_Code), `'`.
token(string(String)), ` ` --> blanks, `"`, string_content(String), `"`.
token(special(Special_Atom)), ` ` --> blanks, single_prefix_special(Special_Atom).
token(special(Special_Atom)), ` ` --> blanks, { special(Special_Atom), atom_codes(Special_Atom, Content) }, Content.

single_prefix_special(Special_Atom) --> nonblank(Code), { atom_codes(Special_Atom, [Code]), single_prefix_special(Special_Atom) }.

single_prefix_special(Special_Atom) :- (
                                        semicolon(Special_Atom)
                                       ;
                                        paren_kind(Special_Atom)
                                       ;
                                        single_prefix_other_operator_related(Special_Atom)
                                       ).

semicolon(';').

paren_kind(Special_Atom) :- member(Special_Atom, ['(', ')', '[', ']', '{', '}']).

% per se not an operator in the language but is part of operators (eg. ternary parts)
single_prefix_other_operator_related(Special_Atom) :- member(Special_Atom, ['?', ':']).

special(Special_Atom) :- basic_operator(Special_Atom).

basic_operator(Special_Atom) :- member(Special_Atom, ['&&', '||', '+=', '/=', '*=', '-=', '<<=', '>>=', '<=', '>=', '==', '<<', '>>', '->', '>', '<', '=', '&', '.', '++', '--', '+', '-', '*', '/', '!', '%', '~']).

safe_char_content(32) --> ` `.
safe_char_content(Char_Code) --> nonblank(Char_Code), { \+ member(Char_Code, [34, 39, 92]) }.
quote_char_content(34) --> `"`.
apostrophe_char_content(39) --> `'`.

escape_char_content(Char_Code) --> `\\`, escape_determiner_part(Char_Code).

escape_determiner_part(9) --> `t`.
escape_determiner_part(10) --> `n`.
escape_determiner_part(13) --> `r`.
escape_determiner_part(34) --> `"`.
escape_determiner_part(39) --> `'`.
escape_determiner_part(92) --> `\\`.
escape_determiner_part(Char_Code) --> `x{`, hexchar_descriptor(Char_Code), `}`.
escape_determiner_part(Char_Code) --> `x`, hexchar_descriptor(Char_Code).

hexchar_descriptor(Char_Code) --> xinteger(Char_Code), { between(0, 255, Char_Code) }.

char_content(Char_Code) --> escape_char_content(Char_Code).
char_content(Char_Code) --> quote_char_content(Char_Code).
char_content(Char_Code) --> safe_char_content(Char_Code).

char_content_in_string(Char_Code) --> escape_char_content(Char_Code).
char_content_in_string(Char_Code) --> apostrophe_char_content(Char_Code).
char_content_in_string(Char_Code) --> safe_char_content(Char_Code).

string_content(String) --> sequence(char_content_in_string, Codes), { string_codes(String, Codes) }.

%% PARSER PART %%

function_declaration(_) --> function_signature(_), special(';').

function_definition(_) --> function_signature(_), block(_).

block(_) --> special('{'), sequence(statement, _), special('}').

statement(_) --> variable_definition(_).
% TODO: typedef? function declaration?
statement(_) --> prefix_control_statement(_). % if, while, for(?)
statement(_) --> do_while_statement(_).
statement(_) --> switch_statement(_). % or model as prefix_control_statement? I only want to allow `case` here tbh
statement(_) --> goto_statement(_).
statement(_) --> full_expression(_), [special(';')].

paren_expression(sealed(E)) --> [special('(')], full_expression(E), [special(')')].
% TODO: cast operator cannot be parsed
prefix_operator_expression(E) --> [Prefix_Op_Symbol], { prefix_operator(Prefix_Op_Symbol, Prefix_Op) }, prefix_followup(Operand_Structure), { with_unary(Operand_Structure, Prefix_Op, E) }.

%infix:
expression_transformation(_), [E] --> easily_processed_subexpression(EPS), [Infix_Op_Symbol], { infix_operator(Infix_Op_Symbol, Infix_Op) }, prefix_followup(Right_Operand), { with_binary(EPS, Infix_Op-Right_Operand, E) }.
%postcircumfix:
expression_transformation(_), [E] --> easily_processed_subexpression(EPS), [special(Postcircumfix_Opener)], { postcircumfix_operator(Postcircumfix_Opener, Postcircumfix_Closer), atom_concat(Postcircumfix_Opener, Postcircumfix_Closer, Postcircumfix_Op) }, prefix_followup(Right_Operand), { with_binary(EPS, postcircumfix(Postcircumfix_Op)-Right_Operand, E) }.
%postfix:
expression_transformation(_), [E] --> easily_processed_subexpression(EPS), [Postfix_Op_Symbol], { postfix_operator(Postfix_Op_Symbol, Postfix_Op) }, { with_unary(EPS, Postfix_Op, E) }.
%conditional:
expression_transformation(_), [E] --> easily_processed_subexpression(EPS), [special('?')], full_expression(Middle_Operand), [special(':')], prefix_followup(Right_Operand), { with_ternary(EPS, Middle_Operand-ternary('?:')-Right_Operand, E) }.
%parenthesized:
expression_transformation(_), [E] --> paren_expression(E).
%prefix:
expression_transformation(_), [E] --> prefix_operator_expression(E).

full_expression(E) --> sequence(expression_transformation, _), easily_processed_subexpression(EPS), { unsealed(EPS, E) }.

prefix_followup(atom(Content)) --> c_atom(Content).
prefix_followup(E) --> paren_expression(E).
prefix_followup(E) --> prefix_operator_expression(E).

easily_processed_subexpression(sealed(Expression)) --> [sealed(Expression)].
easily_processed_subexpression(expression(E, O)) --> [expression(E, O)].
easily_processed_subexpression(atom(Content)) --> c_atom(Content).

c_atom(Content) --> [number(Content)].
c_atom(Content) --> [char(Content)].
c_atom(Content) --> [string(Content)]. % TODO could be converted into 0-terminated byte sequence or something
c_atom(Content) --> [bare_word(Content)]. % TODO keywords not really modelled

prefix_operator(bare_word(sizeof), prefix(sizeof)).
prefix_operator(special(Spec), prefix(Spec)) :- member(Spec, ['+', '-', '++', '--', '!', '~', '&', '*']).
infix_operator(special(Spec), infix(Spec)) :- member(Spec, ['&&', '||', '&=', '|=', '^=', '+=', '/=', '*=', '-=', '<<=', '>>=', '<=', '>=', '==', '<<', '>>', '->', '>', '<', '=', '.', '+', '-', '*', '/', '&', '|', '^', '%']).
postcircumfix_operator('(', ')'). % function call
postcircumfix_operator('[', ']'). % indexing
postfix_operator(special(Spec), postfix(Spec)) :- member(Spec, ['++', '--']).

unsealed(atom(Content), atom(Content)).
unsealed(expression(Operator, Operands), expression(Operator, Operands_Unsealed)) :- maplist(unsealed, Operands, Operands_Unsealed).
unsealed(sealed(Expression), Unsealed) :- unsealed(Expression, Unsealed).

operator_properties(Op, 1, left) :- member(Op, [postcircumfix('()'), postcircumfix('[]'), infix('.'), infix('->'), postfix('++'), postfix('--')]).
operator_properties(prefix(_), 2, right).
operator_properties(infix(Op_Name), 3, left) :- member(Op_Name, ['*', '/', '%']).
operator_properties(infix(Op_Name), 4, left) :- member(Op_Name, ['+', '-']).
operator_properties(infix(Op_Name), 5, left) :- member(Op_Name, ['<<', '>>']).
operator_properties(infix(Op_Name), 6, left) :- member(Op_Name, ['<', '<=', '>', '>=']).
operator_properties(infix(Op_Name), 7, left) :- member(Op_Name, ['==', '!=']).
operator_properties(infix('&'), 8, left).
operator_properties(infix('^'), 9, left).
operator_properties(infix('|'), 10, left).
operator_properties(infix('&&'), 11, left).
operator_properties(infix('||'), 12, left).
operator_properties(ternary('?:'), 13, right).
operator_properties(infix(Op_Name), 14, left) :- member(Op_Name, ['=', '+=', '-=', '*=', '/=', '%=', '&=', '^=', '|=', '<<=', '>>=']).
operator_properties(infix(','), 15, left).

with_unary(General_Expression, Unary_Operator, With_Unary) :- with_operator(General_Expression, Unary_Operator, [], With_Unary).
with_binary(General_Expression, Binary_Operator-Right_Operand, With_Binary) :- with_operator(General_Expression, Binary_Operator, [Right_Operand], With_Binary).
with_ternary(General_Expression, Middle_Operand-Ternary_Operator-Right_Operand, With_Ternary) :- with_operator(General_Expression, Ternary_Operator, [Middle_Operand, Right_Operand], With_Ternary).

zero_precedence(atom(_)).
zero_precedence(sealed(_)).

with_operator(General_Expression, Operator, Operands_To_Add, expression(Operator, Operands)) :- zero_precedence(General_Expression) -> append([General_Expression], Operands_To_Add, Operands).
with_operator(expression(Root_Operator, Operands), Operator, Operands_To_Add, With_Unary) :-
        operator_properties(Root_Operator, Root_Precedence, _Root_Associativity),
        operator_properties(Operator, Current_Precedence, Current_Associativity),
        ((Root_Precedence < Current_Precedence; Root_Precedence =:= Current_Precedence, Current_Associativity == left) ->
                With_Unary = expression(Operator, New_Operands),
                append([expression(Root_Operator, Operands)], Operands_To_Add, New_Operands)
        ;
                length(Operands, Last_1ndex),
                nth1(Last_1ndex, Operands, Last_Operand, Untouched_Operands),
                nth1(Last_1ndex, New_Operands, Updated_Operand, Untouched_Operands),
                With_Unary = expression(Root_Operator, New_Operands),
                with_operator(Last_Operand, Operator, Operands_To_Add, Updated_Operand)
        ).
