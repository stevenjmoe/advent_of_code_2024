let file = "input/day2.txt"

let parse_report report =
  report |> String.split_on_char ' ' |> List.map (fun s -> int_of_string s)
;;

let get_diff previous_level current_level = abs (previous_level - current_level)

let validate_level previous_level current_level =
  match abs (previous_level - current_level) with
  | 0 -> false
  | diff when diff > 3 && previous_level != 0 -> false
  | _ -> true
;;

let level_is_safe
  current_direction
  previous_direction
  current_level
  previous_level
  =
  if
    previous_direction = `None
    || (current_direction = previous_direction
        && validate_level previous_level current_level)
  then
    true
  else
    false
;;

let string_of_d = function
  | `Increasing -> "Increasing"
  | `Decreasing -> "Decreasing"
  | `NoChange -> "NoChange"
  | `None -> "None"
;;

let report_is_safe report =
  let parsed_report_list = parse_report report in
  let rec loop report previous_level previous_direction is_safe =
    match report with
    | [] -> is_safe
    | current_level :: rest ->
      let current_direction =
        if current_level > previous_level then
          `Increasing
        else
          `Decreasing
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

let report_is_safe2 report =
  let parsed_report_list = parse_report report in
  let rec loop
    report
    (previous_level : int option)
    is_safe
    prev_direction
    dampened
    =
    match report with
    | [] -> is_safe
    | current_level :: rest -> (
      match previous_level with
      | None -> loop rest (Some current_level) is_safe `None false
      | Some previous_level -> (
        let current_direction =
          if current_level > previous_level then
            `Increasing
          else if current_level = previous_level then
            `NoChange
          else
            `Decreasing
        in
        let safe =
          (current_direction != `NoChange || prev_direction = `None)
          || (current_direction = prev_direction
              && validate_level previous_level current_level)
        in
        (*Printf.printf
          "prev level, current level: %d %d\n"
          previous_level
          current_level;
          Printf.printf
          "Current Direction, prev direction: %s %s\n"
          (string_of_d current_direction)
          (string_of_d prev_direction);
          Printf.printf "Safe: %b\n" safe;
          Printf.printf "Dampened: %b\n" dampened;*)
        match safe, dampened with
        | false, false ->
          loop rest (Some previous_level) is_safe prev_direction true
        | false, true -> false
        | true, _ ->
          loop rest (Some current_level) is_safe current_direction dampened))
  in
  let res = loop parsed_report_list None true `None false in
  res
;;

let solution1 =
  File_reader.read_file file
  |> List.fold_left
       (fun acc line -> if report_is_safe line then acc + 1 else 0)
       0
;;

let solution2 =
  File_reader.read_file file
  |> List.fold_left
       (fun acc line ->
         if report_is_safe2 line then Printf.printf "Here: %s\n" line else ();
         if report_is_safe2 line then acc + 1 else acc + 0)
       0
;;
