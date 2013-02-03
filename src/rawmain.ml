(* file: main.ml *)

let main () =
  try
    print_string "reading: ";
    let lexbuf = Lexing.from_channel stdin in
    let cache = begin
        let l = ref [] in
        fun lexbuf -> match !l with
            | x::xs -> l := xs; x
            | [] -> match Rawlex.tokens lexbuf with
                    | [] -> failwith "oops"
                    | x::xs -> l := xs; x
        end in
    while true do
      Parser.input cache lexbuf
    done
  with End_of_file -> exit 0
      
let _ = Printexc.print main ()