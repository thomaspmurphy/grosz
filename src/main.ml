(* main.ml *)
open Expr

let () =
  let env = [] in

  (* 1 + 2 * 3 *)
  let expr1 = Add (Num 1, Mul (Num 2, Num 3)) in
  let result1 = eval env expr1 in
  (match result1 with
   | VNum n -> Printf.printf "1 + 2 * 3 = %d\n" n
   | _ -> ());

  (* "Hello" ^ " " ^ "world" *)
  let expr2 = Concat (Concat (Str "Hello", Str " "), Str "world") in
  let result2 = eval env expr2 in
  (match result2 with
   | VStr s -> Printf.printf "\"Hello\" ^ \" \" ^ \"world\" = %s\n" s
   | _ -> ());

  (* length("Hello") *)
  let expr3 = Length (Str "Hello") in
  let result3 = eval env expr3 in
  (match result3 with
   | VNum n -> Printf.printf "length(\"Hello\") = %d\n" n
   | _ -> ());

  (* let x = 5 in x + 3 *)
  let expr4 = Let ("x", Num 5, Add (Var "x", Num 3)) in
  let result4 = eval env expr4 in
  (match result4 with
   | VNum n -> Printf.printf "let x = 5 in x + 3 = %d\n" n
   | _ -> ())

