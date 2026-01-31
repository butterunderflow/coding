open Sexplib.Conv

module E2 = struct
  type expr =
    | EVar of string
    | ELet of string * expr * expr
    | ENum1 of int
    | ENum2 of int
    | EIfz1 of expr * expr * expr
    | EIfz2 of expr * expr * expr
    | ELam1 of string * expr
    | ELam2 of string * expr
    | EApp1 of expr * expr
    | EApp2 of expr * expr
  [@@deriving sexp]
end

module E1 = struct
  type expr =
    | ELam of string * expr
    | EVar of string
    | ENum of int
    | ELet of string * expr * expr
    | EIfz of expr * expr * expr
    | EApp of expr * expr
  [@@deriving sexp]
end

type value =
  | VInt of int
  | VClos of env * string (* argument name *) * E2.expr (* body *)
  | VCode of E1.expr

and env = (string * value) list [@@derving sexp]

type cont = value -> value

let index = ref 0

let fresh () =
  index := !index + 1;
  Printf.sprintf "_x%d" !index

let bind (e : E1.expr) (k : cont) : value =
  match e with
  | EVar _
  | ENum _ ->
      k (VCode e)
  | EIfz _ -> k (VCode e)
  | ELet (_, _, _) ->
      failwith "Internal error: breaking anf invariants, bind"
  | EApp _
  | ELam _ ->
      let x = fresh () in
      let (VCode body) = k (VCode (EVar x)) in
      VCode (ELet (x, e, body))

(* let x = fresh () in *)
(* let (VCode body) = k (VCode (EVar x)) in *)
(* VCode (ELet (x, e, body)) *)

type environment = (string * value) list

let ( .%[] ) lst x =
  match List.assoc_opt x lst with
  | Some v -> v
  | None -> failwith (Printf.sprintf "Cannot find variable %s." x)

let rec eval (e : E2.expr) (env : environment) (k : cont) =
  match e with
  | ELam1 (x, e) -> k (VClos (env, x, e))
  | ELam2 (x, e) ->
      k (VCode (ELam (x, reify e ((x, VCode (EVar x)) :: env))))
  | EVar x -> k env.%[x]
  | ENum1 i -> k (VInt i)
  | ENum2 i -> k (VCode (ENum i))
  | ELet (x, e1, e2) ->
      eval e1 env (function
          | VCode rhs (* could be a atom expr or a simple expression *) ->
          bind rhs (fun x_v -> eval e2 ((x, x_v) :: env) k))
  | EIfz1 (e1, e2, e3) ->
      eval e1 env (function VInt cond ->
          if cond = 0 then eval e2 env k else eval e3 env k)
  | EIfz2 (e1, e2, e3) ->
      eval e1 env (function VCode cond ->
          k (VCode (EIfz (cond, reify e2 env, reify e3 env))))
  | EApp1 (e1, e2) ->
      eval e1 env (function VClos (env1, x, e) ->
          eval e2 env (fun v -> eval e ((x, v) :: env1) k))
  | EApp2 (e1, e2) ->
      eval e1 env (fun v_e1 ->
          eval e2 env (fun v_e2 ->
              let (VCode func) = v_e1 in
              let (VCode arg) = v_e2 in
              bind (EApp (func, arg)) k))

(* evaluate an expression to a code *)
and reify e env : E1.expr =
  let (VCode res) = eval e env (fun e -> e) in
  res

let%expect_test "Test: Trivial Staging" =
  let[@warning "-26"] print_expr e =
    e
    |> E1.sexp_of_expr
    |> Sexplib.Sexp.to_string_hum ?indent:(Some 2)
    |> Printf.printf "%s\n"
  in
  print_expr
    E2.(
      reify
        (ELet
           ( "x",
             EApp2 (EVar "y", EApp2 (EVar "z", EVar "w")),
             EApp2 (ELam2 ("y", EVar "y"), EVar "x") ))
        [
          ("y", VCode (EVar "y"));
          ("z", VCode (EVar "z"));
          ("w", VCode (EVar "w"));
        ]);
  [%expect
    {|
    (ELet _x1 (EApp (EVar z) (EVar w))
      (ELet _x2 (EApp (EVar y) (EVar _x1))
        (ELet _x3 (EApp (ELam y (EVar y)) (EVar _x2)) (EVar _x3)))) |}]
