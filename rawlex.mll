{
    open Rawparse
    open Lexing
}

let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let ws = [' ' '\t']
rule token = parse
    | ws            { token lexbuf }
    | '\n'          { print_string "ENDLINE "; ENDLINE }
    | ';'           { print_string "SEP "; SEP }
(*  | '\n'ws*';'    { print_string "ENDBLOCK "; flush stdout; ENDBLOCK}
    | ';'ws*'\n'    { print_string "ENDBLOCK "; flush stdout; ENDBLOCK }*)
    | ';'ws*';'     { print_string "ENDBLOCK "; flush stdout; ENDBLOCK }
    | digit+ as num 
                    { print_string (num^" "); NUM (float_of_string num)}
    | "where"       { print_string "WHERE "; WHERE }
    | alpha+ as symb { print_string (symb^" "); SYMB symb} 
    | '+'
    | '-'
    | '*' as lop    
                    { print_string "LOP "; LOP (Char.escaped lop) }
    | '^' as rop
                    { print_string "ROP "; ROP (Char.escaped rop) }
    | '('           { print_string "("; LPAREN }
    | ')'           { print_string ")"; RPAREN }
    | '='           { print_string "="; EQ }
    | '!'           { print_string "!"; UNIQUE }
    | _             { token lexbuf }
    | eof           { EOF }                 