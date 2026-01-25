type value =
  | Int of int
  | Closure of string option * string * AST.node * env_t

and env_t = (string * addr) list

and addr = int

(* We have a global store to represent the heap space *)
let store = ref ([] : value list)

let alloc () : addr =
  let addr = List.length !store in
  store := !store @ [ Int 0 ];
  (* dummy value *)
  addr

let read addr = List.nth !store addr

let set addr v =
  store := List.mapi (fun i x -> if i = addr then v else x) !store

let rec eval (e : AST.node) (env : env_t) : value =
  match e.expr with
  | AST.Int x -> Int x
  | AST.Arith (op, e1, e2) -> (
      let v1 = eval e1 env in
      let v2 = eval e2 env in
      match (op, v1, v2) with
      | "+", Int i1, Int i2 -> Int (i1 + i2)
      | "-", Int i1, Int i2 -> Int (i1 - i2)
      | "*", Int i1, Int i2 -> Int (i1 * i2)
      | "/", Int i1, Int i2 -> Int (i1 / i2)
      | _ -> failwith "Invalid arithmetic operation")
  | AST.LetRec (x, a, e1, e2) ->
      let closure = Closure (Some x, a, e1, env) in
      let addr = alloc () in
      set addr closure;
      eval e2 ((x, addr) :: env)
  | AST.Var x ->
      let addr = List.assoc x env in
      read addr
  | AST.Lam (x, e) -> Closure (None, x, e, env)
  | AST.App (e1, e2) -> (
      let v1 = eval e1 env in
      let v2 = eval e2 env in
      match v1 with
      | Closure (Some x, a, body, closure_env) ->
          let v1_addr = alloc () in
          set v1_addr v1;
          let v2_addr = alloc () in
          set v2_addr v2;
          let env = (a, v2_addr) :: (x, v1_addr) :: closure_env in
          eval body env
      | Closure (None, a, body, closure_env) ->
          let addr = alloc () in
          set addr v2;
          let env = (a, addr) :: closure_env in
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
      let v = eval e1 env in
      let addr = alloc () in
      set addr v;
      eval e2 ((x, addr) :: env)
