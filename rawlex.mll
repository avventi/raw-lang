{
    open Rawparse
    open Lexing
    
    let block_count = ref 0;;
    
    let close_all_blocks () = 
        let rec close_ l = if !block_count <= 0 
                              then l
                              else begin 
                                  decr block_count;
                                  print_string "ENDLINE ENDBLOCK ";
                                  close_ (ENDLINE::ENDBLOCK::l) 
                              end
        in print_string "TOP_SEP "; close_ [TOP_SEP];;                    
    
    (* stbl contains nullary functions that return
    the corresponding token while optionally performing
    side effects. *)
    
    Hashtbl.add stbl "where" (fun () -> incr block_count;WHERE);;
    
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
(*    | '\n'ws*';'    { print_string "ENDLINE ENDBLOCK "; flush stdout; [ENDLINE;ENDBLOCK]}
    | ';'ws*'\n'    { print_string "ENDLINE ENDBLOCK "; flush stdout; [ENDLINE;ENDBLOCK]}*)
    | '\n'ws*'\n'    { print_string "TOP_SEP "; flush stdout; close_all_blocks () }
    | ';'ws*';'     { print_string "ENDLINE ENDBLOCK "; flush stdout; decr block_count; [ENDLINE;ENDBLOCK] }
    | digit+ as num 
                    { print_string (num^" "); [NUM (float_of_string num)]}
    | alpha+ as symb { print_string (symb^" "); try [(Hashtbl.find stbl symb) ()] with Not_found -> [SYMB symb]} 
    | '+'
    | '-'
    | '*' 
    | '^' as op
                    { let _op = Char.escaped op in print_string _op; [Hashtbl.find otbl _op] }
    | '('           { print_string "("; [LPAREN] }
    | ')'           { print_string ")"; [RPAREN] }
    | '='           { print_string "="; [EQ] }
    | '!'           { print_string "!"; [UNIQUE] }
    | ':'           { print_string ":"; [IN] }
    | _             { tokens lexbuf }
    | eof           { List.append (close_all_blocks ()) [TOP_SEP;EOF] }                 