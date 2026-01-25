type expr =
  | Int of int
  | Arith of string (* operation *) * node * node
  | Let of string * node * node
  | LetRec of
      string (* binder of letrec *)
      * string (* parameter *)
      * node (* function body that could cotain the binder *)
      * node (* body of letrec *)
  | Var of string
  | Lam of string * node
  | App of node * node
  | If0 of node * node * node
  | Seq of node * node

and node = {
  expr : expr;
  id : int;
}
[@@deriving ord, show]

let id = ref 0

let gen_id () =
  id := !id + 1;
  !id

let mk_int i = { expr = Int i; id = gen_id () }

let mk_bin op e1 e2 = { expr = Arith (op, e1, e2); id = gen_id () }

let mk_lam x e = { expr = Lam (x, e); id = gen_id () }

let mk_app e1 e2 = { expr = App (e1, e2); id = gen_id () }

let mk_var x = { expr = Var x; id = gen_id () }

let mk_if e1 e2 e3 = { expr = If0 (e1, e2, e3); id = gen_id () }

let mk_seq e1 e2 = { expr = Seq (e1, e2); id = gen_id () }

let mk_let x e1 e2 = { expr = Let (x, e1, e2); id = gen_id () }

let mk_op op e1 e2 = { expr = Arith (op, e1, e2); id = gen_id () }

type t = node

let compare = compare_node

let reset_id () = id := 0

let with_new_id (f : node lazy_t) : node =
  reset_id ();
  Lazy.force f
