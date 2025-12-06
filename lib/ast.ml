type iop =
  | Range
  | Add
  | Mult
  | Leq

type expr =
  | Var of string
  | Int of int
  | Bool of bool
  | InOp of iop * expr * expr
  | Let of string * expr * expr
  | If of expr * expr * expr
  | List of expr list
  | Index of expr * expr
  | Length of expr

type value =
  | VInt of int
  | VBool of bool
  | VList of value list

let rec string_of_val v =
  match v with
  | VInt i -> string_of_int i
  | VBool b -> string_of_bool b
  | VList l -> "["^ String.concat ", " (List.map string_of_val l) ^"]"

type env = (string * value) list

let rec range start stop =
  if start > stop then []
  else start :: range (start+1) stop

let rec eval (env : env) (e : expr) : value =
  match e with
  | Int i -> VInt i
  | Bool b -> VBool b
  | Var x -> 
      (try List.assoc x env 
       with Not_found -> failwith ("Unbound variable: " ^ x))

  | InOp (op, e1, e2) ->
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      (match op, v1, v2 with
      | Range, VInt i1, VInt i2 -> VList (List.map (fun x -> VInt x) (range i1 i2))
      | Add, VInt i1, VInt i2 -> VInt (i1 + i2)
      | Mult, VInt i1, VInt i2 -> VInt (i1 * i2)
      | Leq, VInt i1, VInt i2 -> VBool (i1 <= i2)
      | _ -> failwith "Type error: operand mismatch for operator")

  | If (cond, e_then, e_else) ->
      (match eval env cond with
      | VBool true -> eval env e_then
      | VBool false -> eval env e_else
      | _ -> failwith "Type error: If condition must be a boolean")

  | Let (x, e_def, e_body) ->
      let v_def = eval env e_def in
      let new_env = (x, v_def) :: env in
      eval new_env e_body

  | List exprs ->
      VList (List.map (eval env) exprs)

  | Index (e_list, e_idx) ->
      (match eval env e_list, eval env e_idx with
      | VList l, VInt i ->
        if i>=0&&i<List.length l then
          List.nth l i
        else
          failwith ("Index OOB: "^string_of_int i)
      | _ -> failwith "TypeError" )
  | Length e_list ->
      (match eval env e_list with
      | VList l -> VInt (List.length l)
      | _ -> failwith "len requires list" )
