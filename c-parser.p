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

basic_operator(Special_Atom) :- member(Special_Atom, ['&&', '||', '+=', '/=', '*=', '-=', '<<=', '>>=', '<=', '>=', '==', '<<', '>>', '->', '>', '<', '=', '.', '+', '-', '*', '/', '!', '%']).

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
statement(_) --> expression(_), special(';').

% expressions will be tough:
% - consider handling precedence
% - parens
% - function applications
% - all operators, including indexing and ternaries
