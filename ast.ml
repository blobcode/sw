type iop =
  | Add
  | Mult
  | Leqt

type expr =
  | Var of string
  | Int of int
  | Bool of bool
  | InOp of iop * expr * expr
  | Let of string * expr * expr
  | If of expr * expr * expr
