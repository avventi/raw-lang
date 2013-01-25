type expr=
    | Number of float
    | Constant of string
    | Variable of string
    | Binary of string * expr * expr
    | Where of expr *  (string * expr) list 

let compute_binary op a b = match op with
    | "+" -> a +. b
    | "-" -> a -. b
    | "^" -> a ** b
    | _ -> raise (Invalid_argument "unsupported operator");;

let resolve_expr theexpr =
    (* *)
    let rec _rec_resolve main_expr value_tbl = 
        match main_expr with 
        | Where (main_expr, expr_list) -> let tbl = _build_tbl expr_list (Hashtbl.create 10)
                                          in _rec_resolve main_expr tbl
        | Number num -> num
        | Binary (op, expr1, expr2) ->  let value1 = _rec_resolve expr1 value_tbl in
                                        let value2 = _rec_resolve expr2 value_tbl in
                                        compute_binary op value1 value2
        | Variable name -> Hashtbl.find value_tbl name
        | Constant name -> raise (Invalid_argument "unresolver constant")
    and _build_tbl expr_list tbl = match expr_list with 
        | (name,sub_expr)::tail_list -> let value = _rec_resolve sub_expr tbl in
                                            Hashtbl.add tbl name value; _build_tbl tail_list tbl 
        | [] -> tbl
    in _rec_resolve theexpr (Hashtbl.create 10);;