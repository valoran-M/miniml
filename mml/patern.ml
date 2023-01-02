open Mml
open Value

let rec unify_pattern (p : pattern_loc) (v : value) heap  =
  match (p.pat, v) with
  | Pat_int n1, VInt n2 -> n1 = n2
  | Pat_bool b1, VBool b2 -> b1 = b2
  | Pat_jok, _ -> true
  | Pat_var _, _ -> true
  | Pat_construct (s, lp), VPtr p -> (
      match Hashtbl.find heap p with
      | VConstr (vs, lv) when vs = s ->
          List.for_all2 (fun v1 p -> unify_pattern p v1 heap) lv lp
      | _ -> false)
  | _, _ -> false

let unify_list_expr_pattern v heap (p, _) =
  unify_pattern p v heap

let rec set_env_for_expr (p : pattern) (v : value) heap env = 
  match p, v with 
  | Pat_var x, _ -> Env.add x v env
  | Pat_construct (_, lp), VPtr p ->(
      match Hashtbl.find heap p with
      | VConstr (_, lv) ->
          List.fold_left2 
            (fun env p v -> set_env_for_expr p.pat v heap env) env lp lv
      | _ -> assert false)
  | _ -> env
