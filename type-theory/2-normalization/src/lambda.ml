open Buffer

module H = Hashtbl
module Set = Set.Make(String)

type lambda =
  | Variable of string
  | Application of lambda * lambda
  | Lambda of string * lambda
  | Wrapper of lambda ref

let string_of_lambda lambda =
  let buf = create 1000 in
  let add_buf = add_string buf in
  let rec s_e expr = match expr with
    | Variable v -> add_buf v
    | Application (l, r) -> add_buf "("; s_e l; add_buf " "; s_e r; add_buf ")"
    | Lambda (v, l) -> add_buf "("; add_buf "\\"; add_buf v; add_buf "."; s_e l; add_buf ")"
    | Wrapper i -> s_e !i
  in
  s_e lambda;
  contents buf

let name_count = ref 0
let new_name () =
  name_count := !name_count + 1;
  "v" ^ string_of_int !name_count

let vars expr =
  let rec rec_vars expr' set =
    match expr' with
    | Lambda (x, t) -> rec_vars t (Set.add x set)
    | Application (t1, t2) -> Set.union (rec_vars t1 set) (rec_vars t2 set)
    | Variable v -> Set.add v set
    | Wrapper i -> rec_vars !i set
  in rec_vars expr Set.empty

let rec reduce expr =
  let rec anti_wrapper e =
    match e with
    | Wrapper i -> !i
    | _ -> e
  in

  let substitution name too from =
    let new_wrapper =
      match from with
      | Wrapper i -> i := anti_wrapper !i; from
      | _ -> Wrapper (ref from)
    in
    let mapp = (H.create 512 : (string, string) H.t) in
    let right_vars = Set.add name @@ vars from in

    let rec tree_surf x =
      match x with
      | Variable v -> if H.mem mapp v
        then Variable (H.find mapp v)
        else if v = name then new_wrapper else Variable v
      | Application (l, r) -> Application (tree_surf l, tree_surf r)
      | Wrapper i -> tree_surf !i
      | Lambda (v, l) -> if Set.mem v right_vars
        then
          let newv = new_name () in
          H.add mapp v newv;
          let surfed = tree_surf l in
          H.remove mapp v;
          Lambda (newv, surfed)
        else Lambda (v, tree_surf l)
    in
    tree_surf too
  in

  let application_handler l r =
    let (reduced, flag) = reduce l in
    if flag
    then (Application (reduced, r), flag)
    else let (reduced', flag') = reduce r in (Application (l, reduced'), flag')
  in

  match expr with
  | Variable _ -> (expr, false)
  | Lambda (v, l) -> let (reduced, flag) = reduce l in (Lambda (v, reduced), flag)
  | Application ((Lambda (name, too)), from) -> (substitution name too from, true)
  | Application ((Wrapper i), from) -> (
      match !i with
      | Lambda (name, too) -> (substitution name too from, true)
      | _ -> application_handler (Wrapper i) from
    )
  | Application (l, r) -> application_handler l r
  | Wrapper i ->
    let (reduced, flag) = reduce !i in
    if flag then i := anti_wrapper reduced else ();
    (Wrapper i, flag)
  ;;
