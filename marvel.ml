type primitive =   BOOLEAN of bool   
                 | INTEGER of int    
                 | STRING  of string 
                 | LIST    of primitive list 
                 | FUNC    of (primitive list -> primitive)
                 | Nil;;

type lisp =   SEXP     of lisp list
            | SYMBOL   of string 
            | CONSTANT of primitive;;

let is_true p =
 match p with 
 | BOOLEAN(false) -> false
 | Nil            -> false
 | _              -> true;;

type env = Env of (string, primitive) Hashtbl.t * env option;;

let create_env () = 
  Env(Hashtbl.create 100, None)

let env_add e name value =
 match e with Env(h,p) ->
  Hashtbl.replace h name value

let rec env_find e name = 
 match e with Env(h,p) ->
  try
   Hashtbl.find h name
  with _ ->
   match p with Some(e) ->
    env_find e name;;

let env_child vars xs parent = 
 match vars with SEXP(vars') ->
  let vars'' = List.map (fun v -> match v with SYMBOL(s) -> s) vars' in 
  let e      = Env(Hashtbl.create 100, Some parent) in
   List.map (fun (n,v) -> env_add e n v) (List.combine vars'' xs);
   e;;

let rec quote args = 
 let quote' a =
  match a with
  | SEXP(sexp)  -> quote sexp
  | SYMBOL(sym) -> STRING(sym)
  | CONSTANT(c) -> c
 in
  LIST(List.map quote' args);;

let eval_symbol sym e = 
 match sym with SYMBOL(s) ->
  env_find e s;;

let call syms =
  match syms with
  | FUNC(f) :: args -> f args;;

let to_nil _ = Nil;;

let rec eval exp env = 
    match exp with
    | SEXP(sexp)  -> eval_sexp   sexp env
    | SYMBOL(sym) -> eval_symbol  exp env
    | CONSTANT(c) -> c

and eval_sexp sexp e =
    match sexp with
    | SYMBOL(command) :: args ->
        match command with 
        | "do"     -> to_nil (List.map (fun x -> eval x e) args) 
        | "quote"  -> quote args
        | "def"    -> (match args with 
                       | SYMBOL(name) :: value :: xs -> 
                          to_nil(env_add e name (eval value e)))
        | "fn"     -> (match args with vars :: fexp :: xs -> 
                        FUNC(fun xs -> eval fexp (env_child vars xs e)))
        | "if"     -> (match args with 
                       | cond :: success :: failure :: xs -> 
                          if is_true (eval cond e) 
                          then eval success e 
                          else eval failure e)
        | _        -> let es = List.map (fun x -> eval x e) sexp in 
                       call es;;

