(* ***** Types ***** *)

type primitive =   BOOLEAN of bool   
                 | INTEGER of int    
                 | STRING  of string 
                 | LIST    of primitive list 
                 | FUNC    of (primitive list -> primitive)
                 | SYMBOL   of string 
                 | Nil;;

type lisp =   SEXP     of lisp list
            | CONSTANT of primitive;;

(* ***** Read ***** *)

open Str;;

let tokenise s =
  let seperate_brackets t = Str.global_replace (Str.regexp "[(|)]") " \\0 " t in
  let split t             = Str.split (Str.regexp "[ \r\n]+") t in
  split (seperate_brackets s)

let matches p s =
 Str.string_match (Str.regexp p) s 0

let remove_quotes s =
 let n = String.length s in
  String.sub s 1 (n - 2);;

let convert p =
 match p with 
 | "true"                      -> CONSTANT(BOOLEAN(true))
 | "false"                     -> CONSTANT(BOOLEAN(false))
 | "nil"                       -> CONSTANT(Nil)
 | s when matches "^\".*\"$" s -> CONSTANT(STRING(remove_quotes s))
 | s when matches "^[0-9]+$" s -> CONSTANT(INTEGER(int_of_string s))
 | s                           -> CONSTANT(SYMBOL(s));; 

let parse tokens =
 let rec parse' ts out = 
  match ts with 
   | [] -> (SEXP(out), ts)
   | t :: rest -> 
    (match t with
     | "(" -> (let (s, r) = (parse' rest []) in
               parse' r (s :: out))
     | ")" -> (SEXP(List.rev out), rest)
     | s   -> parse' rest ((convert s) :: out))
  
 in let (s, r) = parse' tokens [] in
  match s with SEXP(xs) -> SEXP(CONSTANT(SYMBOL("do")) :: (List.rev xs));;

let read s =
 let tokens = tokenise s in
  parse tokens;;

(* ***** Print ***** *)

let rec print p =
  match p with 
  | BOOLEAN(b) -> string_of_bool b
  | INTEGER(i) -> string_of_int i
  | STRING(s)  -> String.concat "" ["\""; s; "\""]
  | LIST(l)    -> String.concat " " ["("; String.concat " " (List.map print l); ")"]
  | FUNC(f)    -> "<function>"
  | Nil        -> "Nil"
  | SYMBOL(s)  -> s;;

let rec print_lisp e =
 match e with
 | CONSTANT(SYMBOL(s)) -> s
 | CONSTANT(c)         -> print c
 | SEXP(xs)            -> String.concat " " ["("; String.concat " " (List.map print_lisp xs); ")"];;

(* ***** Eval ***** *)

let is_true p =
 match p with 
 | BOOLEAN(false) -> false
 | Nil            -> false
 | _              -> true;;

type env = Env of (string, primitive) Hashtbl.t * env option;;

let create_env () = 
    Env(Hashtbl.create 100, None);;

  
let env_add e name value =
 match e with Env(h,p) ->
  Hashtbl.replace h name value

let rec env_find e name = 
 match e with Env(h,p) ->
  try
   Hashtbl.find h name
  with _ ->
   match p with 
   | Some(e) -> env_find e name
   | _       -> Printf.printf "ERROR: no match for symbol '%s'\n" name;
                raise Not_found;;


let env_child vars xs parent = 
 match vars with SEXP(vars') ->
  let vars'' = List.map (fun v -> match v with CONSTANT(SYMBOL(s)) -> s) vars' in 
  let e      = Env(Hashtbl.create 100, Some parent) in
   List.map (fun (n,v) -> env_add e n v) (List.combine vars'' xs);
   e;;

let root_env = create_env ();;

let macro_table = create_env ();;

let is_macro name =
 match macro_table with Env(h,p) ->
  try
   Hashtbl.find h name;
   Printf.printf "FOUND MACRO '%s'\n" name;
   true
  with _ ->
    false;;

let rec quote args = 
 let quote' a =
  (*Printf.printf "QUOTE' '%s'\n" (print_lisp a);*)
  match a with
  | SEXP(sexp)  -> quote sexp
  | CONSTANT(c) -> c
 in
  LIST(List.map quote' args);;

let rec unquote args = 
 (*Printf.printf "UNQUOTE '%s'\n" (print args);*)
 match args with
 | LIST(xs) -> SEXP(List.map unquote xs) 
 | x        -> CONSTANT(x);;


let eval_symbol sym e =
  (*Printf.printf "EVAL_SYMBOL '%s'\n" sym;*)
  env_find e sym;;

let call syms =
  match syms with
  | FUNC(f) :: args -> f args;;

let to_nil _ = Nil;;

let last l = 
 let n = List.length l in
  List.nth l (n - 1);;

let lisp_cons x xs = 
 match xs with LIST(ys) ->
     x :: ys;;

let rec eval exp env = 
    (*Printf.printf "EVAL: %s\n" (print_lisp exp);*)
    match exp with
    | SEXP(sexp)            -> eval_sexp   sexp env
    | CONSTANT(SYMBOL(sym)) -> eval_symbol  sym env
    | CONSTANT(c)           -> c

and eval_sexp sexp e =
    match sexp with
    | SEXP(s) :: args -> (let es = (List.map (fun x -> eval x e) sexp) in
                            call es)
    | CONSTANT(SYMBOL command) :: args ->
        match command with 
        | "do"     -> last (List.map (fun x -> eval x e) args) 
        | "quote"  -> quote args
        | "def"    -> (match args with 
                       | CONSTANT(SYMBOL name) :: value :: xs -> 
                          to_nil(env_add e name (eval value e)))
        | "defm"   -> (match args with 
                       | CONSTANT(SYMBOL name) :: value :: xs -> 
                          to_nil(env_add macro_table name (eval value root_env)))
        | "fn"     -> (match args with vars :: fexp :: xs -> 
                        FUNC(fun xs -> eval fexp (env_child vars xs e)))
        | "if"     -> (match args with 
                       | cond :: success :: failure :: xs -> 
                          if is_true (eval cond e) 
                          then eval success e 
                          else eval failure e)
        | m when is_macro m -> (let f = (eval (CONSTANT (SYMBOL m)) macro_table) in 
                                 eval (unquote (call (lisp_cons f (quote args)))) e) 
        | _  -> let es = (List.map (fun x -> eval x e) sexp) in
                            call es;;

(* ***** Standard functions **** *)


let deff s f = env_add root_env s (FUNC f);;

deff "list" (fun xs -> LIST xs);;

deff "car" (fun args -> match args with
                        | LIST []        :: _ -> Nil
                        | LIST (x :: xs) :: _ -> x);;

deff "cdr" (fun args -> match args with
                        | LIST []        :: _ -> Nil
                        | LIST (x :: xs) :: _ -> LIST(xs));;

deff "cons" (fun args -> match args with 
                         | x :: LIST(xs) :: _ -> LIST(x :: xs));;

deff "nil?" (fun args -> match args with
                         | Nil :: _ -> BOOLEAN true
                         | _        -> BOOLEAN false);;

deff "=" (fun args -> match args with 
                      | a :: b :: _ -> BOOLEAN (a = b)
                      | _           -> BOOLEAN false);;

deff "symbol" (fun args -> match args with
                           | (STRING s) :: _ -> SYMBOL(s));;

let list_math f i xs =
 let ns = List.map (fun n -> match n with INTEGER m -> m) xs in
 match ns with
 | []       -> INTEGER(i)
 | n :: ns' -> 
   (let r = List.fold_left f n ns' in
     INTEGER r);;

deff "+" (list_math (+)   0);;
deff "-" (list_math (-)   0);;
deff "*" (list_math ( * ) 1);;
deff "/" (list_math (/)   1);;

deff "str" (fun xs -> STRING (print (LIST xs)));;
  
(* ***** REPL ***** *)

let rep s = print (eval (read s) root_env);;

(* ***** MAIN ***** *)

let load_file f =
 let ic = open_in f in
 let n = in_channel_length ic in
 let s = String.create n in
  really_input ic s 0 n;
  close_in ic;
  (s);;

let main () =
 let s = load_file Sys.argv.(1) in
 let r = print (eval (read s) root_env) in
  Printf.printf "Result = '%s'\n" r;;

main ();;
    
