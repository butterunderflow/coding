type value =
  | Int of int
  | Closure of string option * string * AST.node * env_t
[@@deriving show]

and env_t = (string * value) list

let rec eval (e : AST.node) (env : env_t) : _ =
  match e.expr with
  | AST.Int i -> Int i
  | AST.Arith (op, e1, e2) -> (
      match (op, eval e1 env, eval e2 env) with
      | "+", Int i1, Int i2 -> Int (i1 + i2)
      | "-", Int i1, Int i2 -> Int (i1 - i2)
      | "*", Int i1, Int i2 -> Int (i1 * i2)
      | "/", Int i1, Int i2 -> Int (i1 / i2)
      | _ -> failwith "Invalid arithmetic operation")
  | AST.LetRec (x, a, e1, e2) ->
      let closure = Closure (Some x, a, e1, env) in
      eval e2 ((x, closure) :: env)
  | AST.Var x -> List.assoc x env
  | AST.Lam (x, e) -> Closure (None, x, e, env)
  | AST.App (e1, e2) -> (
      let v1 = eval e1 env in
      let v2 = eval e2 env in
      match v1 with
      | Closure (Some x, a, body, closure_env) ->
          let env = (a, v2) :: (x, v1) :: closure_env in
          eval body env
      | Closure (None, a, body, closure_env) ->
          let env = (a, v2) :: closure_env in
          eval body env
      | _ -> failwith "Trying to apply a non-function value")
  | AST.If0 (e1, e2, e3) -> (
      let cond = eval e1 env in
      match cond with
      | Int 0 -> eval e2 env
      | Int _ -> eval e3 env
      | _ -> failwith "Condition of If0 is not an integer")
  | AST.Seq (e1, e2) ->
      let _ = eval e1 env in
      eval e2 env
  | AST.Let (x, e1, e2) ->
      let v1 = eval e1 env in
      eval e2 ((x, v1) :: env)

let%expect_test "eval" =
  let eval_and_print e =
    Printf.printf "start!\n";
    Printf.printf "%s\n" (show_value (eval e []))
  in
  eval_and_print AST.(with_new_id (lazy (mk_bin "+" (mk_int 1) (mk_int 2))));
  [%expect {|
    start!
    (Eval0.Int 3) |}];
  eval_and_print AST.(with_new_id (lazy (mk_lam "x" (mk_int 1))));
  [%expect
    {|
    start!
    (Eval0.Closure (None, "x", { AST.expr = (AST.Int 1); id = 1 }, [])) |}];
  eval_and_print
    AST.(
      with_new_id
        (lazy
          (mk_app
             (mk_lam "x" (mk_bin "+" (mk_var "x") (mk_int 3)))
             (mk_int 4))));
  [%expect {|
    start!
    (Eval0.Int 7) |}]
