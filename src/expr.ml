(* expr.ml *)
type expr =
  | Num of int
  | Str of string
  | Var of string
  | Add of expr * expr
  | Mul of expr * expr
  | Concat of expr * expr
  | Length of expr
  | Let of string * expr * expr

type value =
  | VNum of int
  | VStr of string

type env = (string * value) list

exception EvalError of string

let rec lookup env x =
  match env with
  | [] -> raise (EvalError ("Unbound variable: " ^ x))
  | (y, v) :: env' -> if x = y then v else lookup env' x

let rec eval env e =
  match e with
  | Num n -> VNum n
  | Str s -> VStr s
  | Var x -> lookup env x
  | Add (e1, e2) ->
    (match eval env e1, eval env e2 with
     | VNum n1, VNum n2 -> VNum (n1 + n2)
     | _ -> raise (EvalError "Addition requires two numbers"))
  | Mul (e1, e2) ->
    (match eval env e1, eval env e2 with
     | VNum n1, VNum n2 -> VNum (n1 * n2)
     | _ -> raise (EvalError "Multiplication requires two numbers"))
  | Concat (e1, e2) ->
    (match eval env e1, eval env e2 with
     | VStr s1, VStr s2 -> VStr (s1 ^ s2)
     | _ -> raise (EvalError "Concatenation requires two strings"))
  | Length e ->
    (match eval env e with
     | VStr s -> VNum (String.length s)
     | _ -> raise (EvalError "Length requires a string"))
  | Let (x, e1, e2) ->
    let v1 = eval env e1 in
    let env' = (x, v1) :: env in
    eval env' e2
