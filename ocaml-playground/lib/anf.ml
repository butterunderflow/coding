open Sexplib.Conv

type expr =
  | ELam of string * expr
  | EVar of string
  | ENum of int
  | ELet of string * expr * expr
  | EIfz of expr * expr * expr
  | EApp of expr * expr
[@@deriving sexp]

type cont = expr -> expr

let index = ref 0

let fresh () =
  index := !index + 1;
  Printf.sprintf "_x%d" !index

let bind (e : expr) (k : cont) =
  let x = fresh () in
  ELet (x, e, k (EVar x))

let rec eval (e : expr) (k : cont) =
  match e with
  | ELam (x, e) -> k (ELam (x, reify e))
  | EVar _ -> k e
  | ENum _ -> k e
  | ELet (x, e1, e2) ->
      eval e1 (fun v_e1 (* could be a atom expr or a simple expression *) ->
          ELet (x, v_e1, eval e2 k))
  | EIfz (e1, e2, e3) ->
      eval e1 (fun v_e1 -> k (EIfz (v_e1, reify e2, reify e3)))
  | EApp (e1, e2) ->
      eval e1 (fun v_e1 -> eval e2 (fun v_e2 -> bind (EApp (v_e1, v_e2)) k))

(* evaluate an expression to a code *)
and reify e : expr = eval e (fun e -> e)

let%expect_test "Test: Anf" =
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
            EApp (ELam ("y", EVar "y"), EVar "x") )));
  [%expect
    {|
    (ELet _x1 (EApp (EVar z) (EVar w))
      (ELet _x2 (EApp (EVar y) (EVar _x1))
        (ELet x (EVar _x2)
          (ELet _x3 (EApp (ELam y (EVar y)) (EVar x)) (EVar _x3))))) |}]
