type expr =
  | ELam of string * expr
  | EVar of string
  | ENum of int
  | EIfz1 of expr * expr * expr
  | EIfz2 of expr * expr * expr
  | EApp1 of expr * expr
  | EApp2 of expr * expr
  | ELift of expr

type value =
  | VInt of int
  | VClos of env * string (* argument name *) * expr (* body *)
  | VCode of expr

and env = (string * value) list

type mcont = value -> value

type cont = value * mcont -> value

let i = ref 0

let newvar () =
  i := !i + 1;
  Printf.sprintf "x%d" !i

let lookup env x = List.assoc x env

let eval (env : env) (e : expr) (k : cont) (mk : mcont) : value =
  match e with
  | ELam (x, e) -> k (VClos (env, x, e), mk)
  | _ -> raise Not_found

