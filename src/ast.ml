type lit =
  | Int of int
  | Bool of bool

let str_lit l =
  match l with
  | Int i -> string_of_int i
  | Bool b -> string_of_bool b

type exp =
  | Var of string
  | Lit of lit
  | App of exp * exp
  | Abs of string * exp
  | Let of string * exp * exp

let rec str_exp s =
  match s with
  | Var id -> id
  | Lit l -> str_lit l
  | App (fn, arg) -> str_exp fn ^ " " ^ str_exp arg
  | Abs (arg, e) -> "\\ " ^ arg ^ " -> " ^ str_exp e
  | Let (id, e, b) -> "let " ^ id ^ " = " ^ str_exp e ^ " in " ^ str_exp b

type mono_ty =
  | TVar of string
  | TInt
  | TBool
  | TFun of mono_ty * mono_ty

let rec str_mono_ty t =
  match t with
  | TVar id -> id
  | TInt -> "Int"
  | TBool -> "Bool"
  | TFun (param, ret) -> "(-> " ^ str_mono_ty param ^ " " ^ str_mono_ty ret ^ ")"

type poly_ty =
  | TForAll of string list * mono_ty

let str_poly_ty t =
  match t with
  | TForAll (vars, ty) -> "(ForAll [" ^ String.concat ", " vars ^ "] "
    ^ str_mono_ty ty ^ ")"

module StringSet = Set.Make (String)
module StringMap = Map.Make (String)

let map_bindings m =
  let _, bindings = List.split (StringMap.bindings m) in
  bindings

let remove_ids s ids =
  let open StringSet in
  fold remove ids s
