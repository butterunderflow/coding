module V = struct
  (* use variable name to represent the address, to finitize the address *)
  type value =
    | Fail
    | Zero
    | N
    | Closure of string option * string * AST.node * env_t

  and env_t = (string * addr) list

  and addr = string [@@deriving ord, show]

  type t = value [@@deriving ord, show]
end

module VS = Set.Make (V)

type memory = (string * VS.t) list [@@deriving ord]

let pp_memory fmt mem =
  let module HelperMemory = struct
    type t = (string * V.t list) list [@@deriving show]
  end in
  HelperMemory.pp fmt
    (List.map (fun (addr, vs) -> (addr, VS.elements vs)) mem)

module Ret = struct
  (* The result of eval *)
  type t = {
    v : V.value;
    store : memory;
  }
  [@@deriving ord, show]
end

module Rets = Set.Make (Ret)

type non_det_value = Rets.t

let store = ref ([] : memory)

let alloc (x : string) : V.addr =
  let addr = x in
  if List.mem_assoc addr !store then ()
  else store := (addr, VS.empty) :: !store;
  addr

let find addr =
  match List.assoc_opt addr !store with
  | Some vs -> vs
  | None ->
      store := (addr, VS.empty) :: !store;
      VS.empty

let extend addr v =
  let old_vs = find addr in
  let new_vs = VS.add v old_vs in
  store :=
    List.map
      (fun (addr1, vs) ->
        if addr1 = addr then (addr1, new_vs) else (addr1, vs))
      !store

(* a monadic binding for non-deterministic value *)
let ( let* ) (vs : non_det_value) (f : V.t -> non_det_value) : non_det_value
    =
  Rets.fold
    (fun v vs ->
      Rets.union vs
        ((* Every computation branches should use the store that inherited
            from earlier computation *)
         store := v.store;
         f v.v))
    vs Rets.empty

type eval_res = Rets.t

(* This eval function will only work on a finite states, but itself doesn't
   know it! So it will compute starting from a same configuration over and
   over *)
let rec eval (e : AST.node) (env : V.env_t) : eval_res =
  let store = !store in
  match e.expr with
  | AST.Int 0 -> Rets.of_list [ { v = Zero; store } ]
  | AST.Int _ -> Rets.of_list [ { v = N; store } ]
  | AST.Arith (op, e1, e2) -> (
      let* v1 = eval e1 env in
      let* v2 = eval e2 env in
      match (op, v1, v2) with
      | "+", Zero, Zero -> Rets.of_list [ { v = Zero; store } ]
      | "+", Zero, N -> Rets.of_list [ { v = N; store } ]
      | "+", N, Zero -> Rets.of_list [ { v = N; store } ]
      | "+", N, N -> Rets.of_list [ { v = N; store } ]
      | "-", N, N -> Rets.of_list [ { v = N; store } ]
      | "-", N, Zero -> Rets.of_list [ { v = N; store } ]
      | "-", Zero, N -> Rets.of_list [ { v = N; store } ]
      | "-", Zero, Zero -> Rets.of_list [ { v = Zero; store } ]
      | "*", Zero, N -> Rets.of_list [ { v = Zero; store } ]
      | "*", N, Zero -> Rets.of_list [ { v = Zero; store } ]
      | "*", N, N -> Rets.of_list [ { v = N; store } ]
      | "/", N, N -> Rets.of_list [ { v = N; store } ]
      | "/", Zero, N -> Rets.of_list [ { v = Zero; store } ]
      | "/", _, Zero -> Rets.of_list [ { v = Fail; store } ]
      | _ -> Rets.of_list [ { v = Fail; store } ])
  | AST.LetRec (x, a, e1, e2) ->
      let addr = x in
      let closure = V.Closure (Some x, a, e1, env) in
      extend addr closure;
      let env = (x, addr) :: env in
      eval e2 env
  | AST.Var x ->
      let addr = List.assoc x env in
      find addr
      |> VS.elements
      |> List.map (fun v -> { Ret.v; store })
      |> Rets.of_list
  | AST.Lam (x, e) ->
      Rets.of_list [ { v = Closure (None, x, e, env); store } ]
  | AST.App (e1, e2) -> (
      let* v1 = eval e1 env in
      let* v2 = eval e2 env in
      match v1 with
      | Closure (Some x, a, body, closure_env) ->
          let v1_addr = alloc x in
          extend v1_addr v1;
          let v2_addr = alloc a in
          extend v2_addr v2;
          let env = (a, v2_addr) :: (x, v1_addr) :: closure_env in
          eval body env
      | Closure (None, a, body, closure_env) ->
          let addr = a in
          extend addr v2;
          let env = (a, addr) :: closure_env in
          eval body env
      | _ -> Rets.of_list [ { v = Fail; store } ])
  | AST.If0 (e1, e2, e3) -> (
      let* cond = eval e1 env in
      match cond with
      | Zero -> eval e2 env
      | N -> Rets.union (eval e2 env) (eval e3 env)
      | _ -> Rets.of_list [ { v = Fail; store } ])
  | AST.Seq (e1, e2) ->
      let _ = eval e1 env in
      eval e2 env
  | AST.Let (x, e1, e2) ->
      let* v = eval e1 env in
      let addr = alloc x in
      extend addr v;
      eval e2 ((x, addr) :: env)

let show_res (vs : eval_res) : string =
  let strs = List.map (fun v -> Ret.show v) (Rets.elements vs) in
  String.concat "\n| " strs

let%expect_test "Still divergent AI" =
  let eval_and_print e =
    Printf.printf "start!\n";
    store := [];
    Printf.printf "%s\n" (show_res (eval e []))
  in
  eval_and_print AST.(with_new_id (lazy (mk_bin "+" (mk_int 1) (mk_int 2))));
  [%expect {|
    start!
    { Eval3.Ret.v = Eval3.V.N; store = [] } |}];
  eval_and_print
    AST.(with_new_id (lazy (mk_if (mk_int 1) (mk_int 0) (mk_int 3))));
  [%expect
    {|
    start!
    { Eval3.Ret.v = Eval3.V.Zero; store = [] }
    | { Eval3.Ret.v = Eval3.V.N; store = [] } |}];
  eval_and_print
    AST.(with_new_id (lazy (mk_app (mk_lam "x" (mk_var "x")) (mk_int 0))));
  [%expect
    {|
    start!
    { Eval3.Ret.v = Eval3.V.Zero; store = [("x", [Eval3.V.Zero])] } |}];
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
    { Eval3.Ret.v = Eval3.V.Zero;
      store =
      [("x", [Eval3.V.Zero; Eval3.V.N]);
        ("y",
         [(Eval3.V.Closure (None, "x", { AST.expr = (AST.Var "x"); id = 8 }, []))
           ])
        ]
      }
    | { Eval3.Ret.v = Eval3.V.N;
      store =
      [("x", [Eval3.V.Zero; Eval3.V.N]);
        ("y",
         [(Eval3.V.Closure (None, "x", { AST.expr = (AST.Var "x"); id = 8 }, []))
           ])
        ]
      } |}];
  eval_and_print
    AST.(
      with_new_id
        (lazy
          (mk_let "y"
             (mk_lam "x" (mk_var "x"))
             (mk_seq
                (mk_seq
                   (mk_app (mk_var "y") (mk_int 0))
                   (mk_app (mk_var "y") (mk_int 2)))
                (mk_app (mk_var "y") (mk_lam "z" (mk_var "1")))))));
  [%expect
    {|
    start!
    { Eval3.Ret.v = Eval3.V.Zero;
      store =
      [("x",
        [Eval3.V.Zero; Eval3.V.N;
          (Eval3.V.Closure (None, "z", { AST.expr = (AST.Var "1"); id = 1 },
             [("y", "y")]))
          ]);
        ("y",
         [(Eval3.V.Closure (None, "x", { AST.expr = (AST.Var "x"); id = 13 },
             []))
           ])
        ]
      }
    | { Eval3.Ret.v = Eval3.V.N;
      store =
      [("x",
        [Eval3.V.Zero; Eval3.V.N;
          (Eval3.V.Closure (None, "z", { AST.expr = (AST.Var "1"); id = 1 },
             [("y", "y")]))
          ]);
        ("y",
         [(Eval3.V.Closure (None, "x", { AST.expr = (AST.Var "x"); id = 13 },
             []))
           ])
        ]
      }
    | { Eval3.Ret.v =
      (Eval3.V.Closure (None, "z", { AST.expr = (AST.Var "1"); id = 1 },
         [("y", "y")]));
      store =
      [("x",
        [Eval3.V.Zero; Eval3.V.N;
          (Eval3.V.Closure (None, "z", { AST.expr = (AST.Var "1"); id = 1 },
             [("y", "y")]))
          ]);
        ("y",
         [(Eval3.V.Closure (None, "x", { AST.expr = (AST.Var "x"); id = 13 },
             []))
           ])
        ]
      } |}]
