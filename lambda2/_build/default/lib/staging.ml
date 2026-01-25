type expr =
  | ELam1 of string * expr
  | ELam2 of string * expr
  | EVar of string
  | ENum of int
  | EIfz1 of expr * expr * expr
  | EIfz2 of expr * expr * expr
  | EApp1 of expr * expr
  | EApp2 of expr * expr
  | ELift of expr
  | ERun of expr

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

let rec eval (env : env) (e : expr) (k : cont) (mk : mcont) : value =
  match e with
  | ELam1 (x, e) -> k (VClos (env, x, e), mk)
  | ELam2 (x, e) ->
      let var = newvar () in
      eval ((x, VCode (EVar var)) :: env) e (function | (VCode e, mk) -> _) _
  | EVar _ -> _
  | ENum _ -> _
  | EIfz1 (_, _, _) -> _
  | EIfz2 (_, _, _) -> _
  | EApp1 (_, _) -> _
  | EApp2 (_, _) -> _
  | ELift _ -> _
  | ERun _ -> _
