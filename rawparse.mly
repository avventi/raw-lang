%{
open Lexing
open Rawast
%}

/* Tokens */
%token ENDLINE
%token SEP
%token EOF
%token WHERE
%token ENDBLOCK
%token LPAREN RPAREN
%token EQ
%token UNIQUE
%token <float> NUM
%token <string> LOP
%token <string> ROP
%token <string> FUN
%token <string> SYMB

// user defined operators
%left LOP
%right ROP

// intrinsic operators
%left WHERE

%start input
%type <unit> input

/* Grammar */
%%

input:  /* empty */                 {}        
        | input sentence            { let (var,theexpr) = $2 in 
                                      let value = string_of_float (Rawast.resolve_expr theexpr) in
                                      print_string ("\ngot: "^(Rawast.get_name var)^" = "^value^"\nreading: "); 
                                      flush stdout}
        | input ENDLINE             { }
        | input EOF                 { print_string "\ncompleted!\n"; raise End_of_file }
;
sentence: rhs EQ lhs    { ($1,$3) }  
;
rhs: SYMB { Rawast.Normal $1 }
    | UNIQUE SYMB { Rawast.Unique $2 }
;
lhs: exp  { $1 }
     |  exp WHERE whereblock blockend   { Rawast.Where ($1, List.rev $3) }
;
blockend:   ENDBLOCK    {}
;
blocksep: SEP           { }
         | ENDLINE      { }
;
whereblock:   /* empty */               { [] }
        | sentence                      { [$1] }
        | whereblock blocksep sentence  { $3::$1 }
;
exp:    NUM                             { Rawast.Number $1 }
        | SYMB                          { Rawast.Variable $1 }
        | exp LOP exp                   { Rawast.Binary ($2,$1,$3) }
        | exp LOP ENDLINE exp %prec LOP { Rawast.Binary ($2,$1,$4) }
        | exp ROP exp                   { Rawast.Binary ($2,$1,$3) }
        | exp ROP ENDLINE exp %prec ROP { Rawast.Binary ($2,$1,$4) }
        | LPAREN exp RPAREN             { $2 }
        | LPAREN ENDLINE exp RPAREN     { $3 }
;
