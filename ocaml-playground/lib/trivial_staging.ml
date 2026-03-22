open Sexplib.Conv

type expr =
  | ELam of string * expr
  | EVar of string
  | ENum of int
  | ELet of string * expr * expr
  | EIfz of expr * expr * expr
  | EApp of expr * expr

and value = VCode of expr [@@deriving sexp]

type cont = value -> value

let index = ref 0

let fresh () =
  index := !index + 1;
  Printf.sprintf "_x%d" !index

let bind (e : expr) (k : cont) : value =
  match e with
  | EVar _
  | ENum _ ->
      k (VCode e)
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

let rec eval (e : expr) (env : environment) (k : cont) =
  match e with
  | ELam (x, e) -> k (VCode (ELam (x, reify e ((x, VCode (EVar x)) :: env))))
  | EVar x -> k env.%[x]
  | ENum _ -> k (VCode e)
  | ELet (x, e1, e2) ->
      eval e1 env
        (fun v_e1 (* could be a atom expr or a simple expression *) ->
          let (VCode rhs) = v_e1 in
          bind rhs (fun x_v -> eval e2 ((x, x_v) :: env) k))
  | EIfz (e1, e2, e3) ->
      eval e1 env (fun v_e1 ->
          let (VCode cond) = v_e1 in
          k (VCode (EIfz (cond, reify e2 env, reify e3 env))))
  | EApp (e1, e2) ->
      eval e1 env (fun v_e1 ->
          eval e2 env (fun v_e2 ->
              let (VCode func) = v_e1 in
              let (VCode arg) = v_e2 in
              bind (EApp (func, arg)) k))

(* evaluate an expression to a code *)
and reify e env : expr =
  let (VCode res) = eval e env (fun e -> e) in
  res

let%expect_test "Test: Trivial Staging" =
  let[@warning "-26"] print_expr e =
    e
    |> sexp_of_expr
    |> Sexplib.Sexp.to_string_hum ?indent:(Some 2)
    |> Printf.printf "%s\n"
  in
  print_expr
    (reify
       (ELet
          ( "x",
            EApp (EVar "y", EApp (EVar "z", EVar "w")),
            EApp (ELam ("y", EVar "y"), EVar "x") ))
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
