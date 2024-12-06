let file = "input/day5.txt"

let parse_input (lines : string list) =
  let ordering_rules_tbl = Hashtbl.create 1000 in
  let reg = Str.regexp {|\([0-9]+|[0-9]+\)|} in
  let reg2 = Str.regexp {|\([0-9]+,\|[0-9]\)+|} in
  (* Build a hash table of all the rules. The key is the second page, the value is the first.
     The list of pages in the 'update' should be reversed.
  *)
  let l =
    lines
    |> List.fold_left
         (fun acc v ->
           if Str.string_match reg v 0 then (
             let split = String.split_on_char '|' v in
             let p1, p2 =
               ( List.nth split 0 |> int_of_string
               , List.nth split 1 |> int_of_string )
             in
             Hashtbl.add ordering_rules_tbl p2 p1;
             acc)
           else if Str.string_match reg2 v 0 then (
             let split =
               String.split_on_char ',' v
               |> List.map (fun v -> int_of_string v)
               |> List.rev
             in
             split :: acc)
           else
             acc)
         []
    |> List.rev
  in
  ordering_rules_tbl, l
;;

(** Returns None if the update is invalid *)
let validate_updates update rules =
  let valid =
    update
    |> List.fold_left
         (fun (acc : int list option) (page_number : int) ->
           match acc with
           | Some [] -> Some (page_number :: [])
           | Some (prev :: rest) ->
             let page_rules = Hashtbl.find_all rules page_number in
             (* if the previous page number is found, the current page number violates the rules *)
             let rule_violoated =
               page_rules |> List.exists (fun p -> p = prev)
             in
             if rule_violoated then
               None
             else
               Some (page_number :: prev :: rest)
           | None -> acc)
         (Some [])
  in
  valid
;;

(** Filters out the invalid updates *)
let get_valid_updates updates = updates |> List.filter_map (fun v -> v)

let get_middle_page update =
  let l = List.length update in
  List.nth update (l / 2)
;;

let solution1 =
  let s = File_reader.read_file file in
  let rules, updates = parse_input s in
  updates
  |> List.fold_left
       (fun acc update ->
         let r = validate_updates update rules in
         r :: acc)
       []
  |> get_valid_updates
  |> List.fold_left
       (fun acc u ->
         let middle_page = get_middle_page u in
         middle_page + acc)
       0
;;
