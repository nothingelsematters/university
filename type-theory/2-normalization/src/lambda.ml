open Buffer

module H = Hashtbl
module Set = Set.Make(String)

type lambda =
  | Variable of string
  | Application of lambda * lambda
  | Lambda of string * lambda
  | Wrapper of int

let wrapped = (H.create 512 : (int, lambda) H.t)

let wrapper_count = ref 0
let add_wrapped x =
  wrapper_count := !wrapper_count + 1;
  H.add wrapped !wrapper_count x;
  Wrapper !wrapper_count

let string_of_lambda lambda =
  let buf = create 1000 in
  let add_buf = add_string buf in
  let rec s_e expr = match expr with
    | Variable v -> add_buf v
    | Application (l, r) -> add_buf "("; s_e l; add_buf " "; s_e r; add_buf ")"
    | Lambda (v, l) -> add_buf "("; add_buf "\\"; add_buf v; add_buf "."; s_e l; add_buf ")"
    | Wrapper i -> (* add_buf "<"; *) s_e @@ H.find wrapped i; (* add_buf ">"; *)
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
    | Wrapper i -> rec_vars (H.find wrapped i) set
  in rec_vars expr Set.empty

let rec reduce expr =
  let wrapper_flag = ref true in
  let brand_new_wrapper = ref (Variable "") in
  let new_wrapper ex =
    match ex with
    | Wrapper i -> ex
    | _ -> if !wrapper_flag
      then (wrapper_flag := false; brand_new_wrapper := add_wrapped ex; !brand_new_wrapper)
      else !brand_new_wrapper
  in

  let conversion left right name =
    let right_vars = Set.add name @@ vars right in
    let rec rename old new_n exp =
      match exp with
      | Variable v -> Variable (if v = old then new_n else v)
      | Lambda (v, l) -> Lambda (v, rename old new_n l)
      | Application (l, r) -> Application (rename old new_n l, rename old new_n r)
      | Wrapper i -> rename old new_n @@ H.find wrapped i
    in

    let rec tree_surf x =
      match x with
      | Variable _ -> x
      | Application (l, r) -> Application (tree_surf l, tree_surf r)
      | Wrapper i -> tree_surf @@ H.find wrapped i
      | Lambda (v, l) ->
        let (vres, res) =
          if Set.mem v right_vars
          then let name = new_name () in (name, rename v name l)
          else (v, l)
        in
        Lambda (vres, tree_surf res)
    in
    tree_surf left
  in

  let substitution name too from =
    let rec rec_substitution n t f =
      match t with
      | Variable v -> if n = v then new_wrapper f else Variable v
      | Application (l, r) -> Application (rec_substitution n l f, rec_substitution n r f)
      | Lambda (v, l) -> Lambda (v, rec_substitution n l f)
      | Wrapper i -> rec_substitution n (H.find wrapped i) f
    in rec_substitution name (conversion too from name) from
  in

  let application_handler l r =
    let (reduced, flag) = reduce l in
    if flag
    then (Application (reduced, r), flag)
    else let (reduced', flag') = reduce r in (Application (l, reduced'), flag')
  in

  let rec anti_wrapper e =
    match e with
    | Wrapper i -> anti_wrapper @@ H.find wrapped i
    | _ -> e
  in


  match expr with
  | Variable _ -> (expr, false)
  | Lambda (v, l) -> let (reduced, flag) = reduce l in (Lambda (v, reduced), flag)
  | Application ((Lambda (name, too)), from) -> (substitution name (anti_wrapper too) from, true)
  | Application ((Wrapper i), from) -> (
      match H.find wrapped i with
      | Lambda (name, too) -> (substitution name (anti_wrapper too) from, true)
      | _ -> application_handler (Wrapper i) from
    )
  | Application (l, r) -> application_handler l r
  | Wrapper i ->
    let (reduced, flag) = reduce @@ H.find wrapped i in
    H.add wrapped i @@ anti_wrapper reduced;
    (Wrapper i, flag)
  ;;
