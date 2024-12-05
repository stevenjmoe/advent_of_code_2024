let file = "input/day3.txt"

let parse_line line regex f =
  let regex = Re.compile @@ Re.Pcre.re regex in
  let match_groups = Re.all regex line in
  f match_groups
;;

let result1 match_groups =
  match_groups
  |> List.fold_left
       (fun sum g ->
         let first_num = Re.Group.get g 3 |> int_of_string in
         let second_num = Re.Group.get g 4 |> int_of_string in
         (first_num * second_num) + sum)
       0
;;

(*let result2 s =
  let re1 = Re.compile @@ Re.Pcre.re {|^.*?(don't\(\)|do\(\))|} in
  let first_group = Re.exec re1 s in
  let first_string = Re.Group.get first_group 0 in
  let mul_reg = Re.compile @@ Re.Pcre.re {|mul\(([0-9]{1,3}),([0-9]{1,3})\)|} in
  let group_list = Re.matches mul_reg first_string in
  let r1 =
    List.fold_left
      (fun acc s ->
        let num_regex =
          Re.compile @@ Re.Pcre.re {|([0-9]{1,3}),([0-9]{1,3})|}
        in
        let num_list = Re.all num_regex s in
        let n =
          List.fold_left
            (fun acc2 q ->
              let i = int_of_string q in
              acc2 * i)
            0
            num_list
        in
        n + acc)
      0
      group_list
  in
  Printf.printf "here: %d" r1;
  ()
;;*)

(*let second_string = Re.Group.get second_group 0 in
  print_endline second_string*)

(*match_groups
  |> List.fold_left
       (fun sum g ->
         Printf.printf "%s\n" @@ Re.Group.get g 1;
         0)
       0*)

let solution1 =
  File_reader.read_file file
  |> List.fold_left
       (fun acc l ->
         let r =
           parse_line l {|(mul\((([0-9]{1,3}),([0-9]{1,3}))\))|} result1
         in
         acc + r)
       0
;;

(*let solution2 =
  File_reader.read_file file
  |> List.fold_left (fun acc l -> acc ^ l) ""
  |> result2
  ;;*)
