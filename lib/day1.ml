let file = "input/day1.txt"

let get_lists () =
  let split_location_ids line =
    let location_ids = line |> String.split_on_char ' ' in
    let id1 = location_ids |> List.hd |> int_of_string in
    let id2 = List.nth location_ids 3 |> int_of_string in
    id1, id2
  in
  File_reader.read_file file
  |> List.fold_left
       (fun (l1, l2) line ->
         match l1, l2 with
         | [], _ ->
           let id1, id2 = split_location_ids line in
           id1 :: [], id2 :: []
         | rest1, rest2 ->
           let id1, id2 = split_location_ids line in
           id1 :: rest1, id2 :: rest2)
       ([], [])
;;

let solution1 =
  let sort a b = compare a b in
  let list1, list2 = get_lists () in
  let sorted1 = list1 |> List.sort sort in
  let sorted2 = list2 |> List.sort sort in
  let _, result =
    sorted1
    |> List.fold_left
         (fun (idx, sum) v ->
           let v2 = List.nth sorted2 idx in
           let result = abs (v - v2) + sum in
           idx + 1, result)
         (0, 0)
  in
  result
;;

let solution2 =
  let list1, list2 = get_lists () in
  let list2_hash = Hashtbl.create 1000 in
  List.iter (fun id -> Hashtbl.add list2_hash id 0) list2;
  let list1_hash = Hashtbl.create 1000 in
  List.iter
    (fun id ->
      let count = Hashtbl.find_all list2_hash id |> List.length in
      Hashtbl.add list1_hash id count)
    list1;
  Hashtbl.fold
    (fun k v acc ->
      let r = k * v in
      r + acc)
    list1_hash
    0
;;
