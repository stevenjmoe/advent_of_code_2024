let file = "input/day3.txt"

let parse_line line =
  let regex = Re.compile @@ Re.Posix.re {|(mul\([0-9]{1,3},[0-9]{1,3}\))|} in
  let matches = Re.matches regex line in
  let regex2 = Re.compile @@ Re.Posix.re {|[0-9]{1,3}|} in
  matches
  |> List.fold_left
       (fun sum l ->
         let matches = Re.matches regex2 l in
         let first_num = List.nth matches 0 |> int_of_string in
         let second_num = List.nth matches 1 |> int_of_string in
         let result = first_num * second_num in
         result + sum)
       0
;;

let solution1 =
  File_reader.read_file file
  |> List.fold_left
       (fun acc l ->
         let r = parse_line l in
         acc + r)
       0
;;
