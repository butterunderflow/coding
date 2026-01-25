module V = struct
  (* Value domain of Eval2. We finitized the integer value to {Zero, N}. But
     the value domain is finitized yet, there could be infinite address, and
     cause the closures have infinite values *)
  type value =
    | Fail
    | Zero
    | N
    | Closure of string option * string * AST.node * env_t

  and env_t = (string * addr) list

  and addr = int [@@deriving ord, show]

  type memory = value list [@@deriving ord, show]
end

module Ret = struct
  (* The result of eval *)
  type t = {
    v : V.value;
    store : V.memory;
  }
  [@@deriving ord, show]
end

(* evaluation is non-deterministic *)
module Rets = Set.Make (Ret)

let store = ref ([] : V.memory)

let alloc () : V.addr =
  let addr = List.length !store in
  store := !store @ [ N ];
  addr

let set addr v =
  store := List.mapi (fun i x -> if i = addr then v else x) !store

type non_det_value = Rets.t

(* a monadic binding for non-deterministic value *)
let ( let* ) (vs : non_det_value) (f : Ret.t -> non_det_value) :
    non_det_value =
  Rets.fold
    (fun v vs ->
      Rets.union vs
        ((* Every computation branches should use the store that inherited
            from earlier computation *)
         store := v.store;
         f v))
    vs Rets.empty

let rec eval (e : AST.node) (env : V.env_t) : non_det_value =
  match e.expr with
  | AST.Int 0 -> Rets.of_list [ { v = V.Zero; store = !store } ]
  | AST.Int _ ->
      Rets.of_list [ { v = V.N; store = !store } ]
      (* Abstract any integer to N *)
  | AST.Arith (op, e1, e2) -> (
      let* v1 = eval e1 env in
      assert (!store == v1.store);
      let* v2 = eval e2 env in
      assert (!store == v2.store);
      (* save the current store *)
      let store = !store in
      match (op, v1.v, v2.v) with
      | "+", Zero, Zero -> Rets.of_list [ { v = Zero; store } ]
      | "+", Zero, N -> Rets.of_list [ { v = N; store } ]
      | "+", N, Zero -> Rets.of_list [ { v = N; store } ]
      | "+", N, N -> Rets.of_list [ { v = N; store }; { v = Zero; store } ]
      | "-", N, N -> Rets.of_list [ { v = N; store }; { v = Zero; store } ]
      | "-", N, Zero -> Rets.of_list [ { v = N; store } ]
      | "-", Zero, N -> Rets.of_list [ { v = N; store } ]
      | "-", Zero, Zero -> Rets.of_list [ { v = Zero; store } ]
      | "*", Zero, N -> Rets.of_list [ { v = Zero; store } ]
      | "*", N, Zero -> Rets.of_list [ { v = Zero; store } ]
      | "*", N, N -> Rets.of_list [ { v = N; store } ]
      | "/", N, N -> Rets.of_list [ { v = N; store }; { v = Zero; store } ]
      | "/", Zero, N -> Rets.of_list [ { v = Zero; store } ]
      | "/", N, Zero -> Rets.of_list [ { v = Fail; store } ]
      | "/", Zero, Zero -> Rets.of_list [ { v = Fail; store } ]
      | _ -> Rets.of_list [ { v = Fail; store } ])
  | AST.LetRec (x, a, e1, e2) ->
      let closure = V.Closure (Some x, a, e1, env) in
      let addr = alloc () in
      set addr closure;
      let env = (x, addr) :: env in
      eval e2 env
  | AST.Var x ->
      let addr = List.assoc x env in
      let v = List.nth !store addr in
      Rets.of_list [ { v; store = !store } ]
  | AST.Lam (x, e) ->
      Rets.of_list [ { v = Closure (None, x, e, env); store = !store } ]
  | AST.App (e1, e2) -> (
      let* v1 = eval e1 env in
      assert (!store == v1.store);
      let* v2 = eval e2 env in
      assert (!store == v2.store);
      match v1.v with
      | Closure (Some x, a, body, closure_env) ->
          let v1_addr = alloc () in
          set v1_addr v1.v;
          let v2_addr = alloc () in
          set v2_addr v2.v;
          let env = (a, v2_addr) :: (x, v1_addr) :: closure_env in
          eval body env
      | Closure (None, a, body, closure_env) ->
          let addr = alloc () in
          set addr v2.v;
          let env = (a, addr) :: closure_env in
          eval body env
      | Zero ->
          Printf.printf "error of application\n get an zero;";
          Rets.of_list [ { v = Fail; store = !store } ]
      | _ -> Rets.of_list [ { v = Fail; store = !store } ])
  | AST.If0 (e1, e2, e3) -> (
      let* cond = eval e1 env in
      assert (!store == cond.store);
      match cond.v with
      | Zero -> eval e2 env
      | N -> Rets.union (eval e2 env) (eval e3 env)
      | Fail -> Rets.of_list [ { v = Fail; store = !store } ]
      | _ -> failwith "Condition of If0 is not an integer")
  | AST.Seq (e1, e2) ->
      let* v = eval e1 env in
      assert (!store == v.store);
      let* v2 = eval e2 env in
      assert (!store == v2.store);
      Rets.of_list [ v2 ]
  | AST.Let (x, e1, e2) ->
      let* v = eval e1 env in
      assert (!store == v.store);
      let addr = alloc () in
      set addr v.v;
      eval e2 ((x, addr) :: env)

let show_res (vs : non_det_value) : string =
  let strs = List.map (fun v -> Ret.show v) (Rets.elements vs) in
  String.concat "\n| " strs

let%expect_test "Test non determinstic evaluation, after abstracting the \
                 value domain (could divergent because the store's \
                 infinity)" =
  let eval_and_print e =
    Printf.printf "start!\n";
    store := [];
    let res = eval e [] in
    Printf.printf "%s\n" (show_res res)
  in
  eval_and_print AST.(with_new_id (lazy (mk_bin "+" (mk_int 1) (mk_int 2))));
  [%expect
    {|
    start!
    { Eval2.Ret.v = Eval2.V.Zero; store = [] }
    | { Eval2.Ret.v = Eval2.V.N; store = [] } |}];
  eval_and_print
    AST.(with_new_id (lazy (mk_if (mk_int 1) (mk_int 0) (mk_int 3))));
  [%expect
    {|
    start!
    { Eval2.Ret.v = Eval2.V.Zero; store = [] }
    | { Eval2.Ret.v = Eval2.V.N; store = [] } |}];

  (* ((fun x -> x) 0) *)
  eval_and_print
    AST.(with_new_id (lazy (mk_app (mk_lam "x" (mk_var "x")) (mk_int 0))));
  [%expect
    {|
    start!
    { Eval2.Ret.v = Eval2.V.Zero; store = [Eval2.V.Zero] } |}];
  eval_and_print
    AST.(
      with_new_id
        (lazy
          (mk_let "y"
             (mk_lam "x" (mk_var "x"))
             (mk_app (mk_var "y") (mk_int 0)))));
  [%expect
    {|
    start!
    { Eval2.Ret.v = Eval2.V.Zero;
      store =
      [(Eval2.V.Closure (None, "x", { AST.expr = (AST.Var "x"); id = 4 }, []));
        Eval2.V.Zero]
      } |}];
  eval_and_print
    AST.(
      with_new_id
        (lazy
          (mk_let "y"
             (mk_lam "x" (mk_var "x"))
             (mk_app (mk_var "y") (mk_int 2)))));
  [%expect
    {|
    start!
    { Eval2.Ret.v = Eval2.V.N;
      store =
      [(Eval2.V.Closure (None, "x", { AST.expr = (AST.Var "x"); id = 4 }, []));
        Eval2.V.N]
      } |}];
  eval_and_print
    AST.(
      with_new_id
        (lazy
          (mk_let "y"
             (mk_lam "x" (mk_var "x"))
             (mk_seq
                (mk_app (mk_var "y") (mk_int 0))
                (mk_app (mk_var "y") (mk_int 2))))));
  [%expect
    {|
    start!
    { Eval2.Ret.v = Eval2.V.N;
      store =
      [(Eval2.V.Closure (None, "x", { AST.expr = (AST.Var "x"); id = 8 }, []));
        Eval2.V.Zero; Eval2.V.N]
      } |}];

  eval_and_print
    AST.(
      with_new_id
        (lazy
          (mk_if (mk_op "+" (mk_int 1) (mk_int 2)) (mk_int 2) (mk_int 0))));
  [%expect
    {|
    start!
    { Eval2.Ret.v = Eval2.V.Zero; store = [] }
    | { Eval2.Ret.v = Eval2.V.N; store = [] } |}];
  eval_and_print
    AST.(
      with_new_id
        (lazy
          (mk_let "y"
             (mk_lam "x" (mk_int 0))
             (mk_if
                (mk_op "+" (mk_int 1) (mk_int 2))
                (mk_app (mk_var "y") (mk_int 3))
                (mk_int 0)))));
  [%expect
    {|
    start!
    { Eval2.Ret.v = Eval2.V.Zero;
      store =
      [(Eval2.V.Closure (None, "x", { AST.expr = (AST.Int 0); id = 9 }, []))] }
    | { Eval2.Ret.v = Eval2.V.Zero;
      store =
      [(Eval2.V.Closure (None, "x", { AST.expr = (AST.Int 0); id = 9 }, []));
        Eval2.V.N]
      } |}]
