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

let rec resolve_expr theexpr =
    match theexpr with 
        | Where (main_expr, (name,sub_expr)::expr_list) -> 
            let value = resolve_expr sub_expr in
            let new_expr = subst_expr main_expr name value in
            resolve_expr (Where (new_expr,expr_list))
        | Where (main_expr, []) -> resolve_expr main_expr
        | Number num -> num
        | Binary (op, expr1, expr2) ->  let value1 = resolve_expr expr1 in
                                        let value2 = resolve_expr expr2 in
                                        compute_binary op value1 value2
        | Variable name -> raise (Invalid_argument "unresolver variable")
        | Constant name -> raise (Invalid_argument "unresolver constant")
and subst_expr main_expr expr_name value = match main_expr with
    | Variable name -> if expr_name = name then Number value else Variable name
    | Constant name -> raise (Invalid_argument "unresolver constant")
    | Number num -> Number num
    | Binary (op, expr1, expr2) -> let new_expr1 = subst_expr expr1 expr_name value in
                                   let new_expr2 = subst_expr expr2 expr_name value in
                                   Binary (op, new_expr1, new_expr2)
    | Where _ -> raise (Invalid_argument "unexpected where block");;