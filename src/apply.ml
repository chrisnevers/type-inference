open Ast

let rec apply_mono subs ty =
  let _rec ty = apply_mono subs ty in
  let open StringMap in
  match ty with
  | TVar id when mem id subs -> find id subs
  | TFun (p, ret) -> TFun (_rec p, _rec ret)
  | _ -> ty

let rec apply_poly subs ty =
  let open StringMap in
  match ty with
  | TForAll (vars, ty) ->
    let subs' = List.fold_right remove vars subs in
    TForAll (vars, apply_mono subs' ty)

let apply_env subs env =
  let open StringMap in
  map (fun ty -> apply_poly subs ty) env

let null_subs = StringMap.empty

let compose_subs subs1 subs2 =
  let open StringMap in
  let map' = map (fun ty -> apply_mono subs1 ty) subs2 in
  union (fun k a b -> Some a) map' subs1
