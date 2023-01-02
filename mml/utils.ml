(* 
   distance de Levenshtein

                | max(|s1|, |s2|)       si min(|s1|, |s2|) = 0 
                | lev(s1 - 1, s2 - 1)   si s1[0] = s2[0] 
  lev(s1, s2) = |         | lev(s1 - 1, s2) 
                | 1 + min | lev(s1, s2 - 1) 
                |         | lev(s1 - 1, s2 - 1) *)
let compare s1 s2 =
  let l1 = String.length s1 in 
  let l2 = String.length s2 in
  let rec lev i1 i2 =
    if min (l1 - i1) (l2 - i2) = 0 then
      max (l1 - i1) (l2 - i2)
    else if s1.[i1] = s2.[i2] then
      lev (i1 + 1) (i2 + 1)
    else
      1 + (min 
                (min (lev (i1 + 1) i2) (lev i1 (i2 + 1))) 
                (lev (i1 + 1) (i2 + 1)))
  in 
  lev 0 0

(* inspiration de la compiler_lib
   https://v2.ocaml.org/api/compilerlibref/Compiler_libs.html *)
let spellchecker name ls =
  let cutoff =
    match String.length name with
    | 1 | 2 -> 0
    | 3 | 4 -> 1
    | _     -> 3
  in
  let rec best_distance l best_dist acc =
    match l with
    | [] -> acc
    | s :: l ->
        let c = compare name s in
        if c < best_dist then
          best_distance l c (Some s)
        else
          best_distance l best_dist None
  in
  best_distance ls (cutoff + 1) None

let did_you_mean name l = 
  match spellchecker name l with 
  | None    -> ""
  | Some s  -> Printf.sprintf "Hint: Did you mean %s\n" s
