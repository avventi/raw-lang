{
    open Rawparse
    open Lexing
    
    let stbl = Hashtbl.create 100;;
    Hashtbl.add stbl "where" WHERE;;
    
    let otbl = Hashtbl.create 100;;
    Hashtbl.add otbl "+" (LOP "+");;
    Hashtbl.add otbl "-" (LOP "-");;
    Hashtbl.add otbl "*" (LOP "*");;
    Hashtbl.add otbl "^" (ROP "^");;
 
}

let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let ws = [' ' '\t']
rule tokens = parse
    | ws            { tokens lexbuf }
    | '\n'          { print_string "ENDLINE "; [ENDLINE] }
    | ';'           { print_string "SEP "; [SEP] }
  (*| '\n'ws*';'    { print_string "ENDBLOCK "; flush stdout; [ENDBLOCK]}
    | ';'ws*'\n'    { print_string "ENDBLOCK "; flush stdout; [ENDBLOCK] }*)
    | ';'ws*';'     { print_string "ENDBLOCK "; flush stdout; [ENDBLOCK] }
    | digit+ as num 
                    { print_string (num^" "); [NUM (float_of_string num)]}
    | alpha+ as symb { print_string (symb^" "); try [Hashtbl.find stbl symb] with Not_found -> [SYMB symb]} 
    | '+'
    | '-'
    | '*' 
    | '^' as op
                    { let _op = Char.escaped op in print_string _op; [Hashtbl.find otbl _op] }
    | '('           { print_string "("; [LPAREN] }
    | ')'           { print_string ")"; [RPAREN] }
    | '='           { print_string "="; [EQ] }
    | '!'           { print_string "!"; [UNIQUE] }
    | _             { tokens lexbuf }
    | eof           { [EOF] }                 