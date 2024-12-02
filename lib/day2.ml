let file = "input/day2.txt"

let parse_report report =
  report |> String.split_on_char ' ' |> List.map (fun s -> int_of_string s)
;;

let validate_level previous_level current_level =
  match abs (previous_level - current_level) with
  | 0 -> false
  | diff when diff > 3 -> false
  | _ -> true
;;

let level_is_safe
  current_direction
  previous_direction
  current_level
  previous_level
  =
  if
    (current_direction = previous_direction || previous_direction = `None)
    && validate_level previous_level current_level
  then
    true
  else
    false
;;

let report_is_safe report =
  let parsed_report_list = parse_report report in
  let rec loop report previous_level previous_direction is_safe =
    match report with
    | [] -> is_safe
    | current_level :: rest ->
      let current_direction =
        if current_level > previous_level then `Increasing else `Decreasing
      in
      let is_safe =
        level_is_safe
          current_direction
          previous_direction
          current_level
          previous_level
      in
      loop rest current_level current_direction is_safe
  in
  loop parsed_report_list 0 `None false
;;

let solution1 =
  File_reader.read_file file
  |> List.fold_left
       (fun acc line -> if report_is_safe line then acc + 1 else 0)
       0
;;
