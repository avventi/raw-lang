(* file: main.ml *)

let main () =
  try
    print_string "reading: ";
    let lexbuf = Lexing.from_channel stdin in
    while true do
      Rawparse.input Rawlex.token lexbuf
    done
  with End_of_file -> exit 0
      
let _ = Printexc.print main ()