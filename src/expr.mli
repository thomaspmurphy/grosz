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

val eval : env -> expr -> value
val lookup : env -> string -> value
