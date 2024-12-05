let file = "input/day4.txt"
let s = 3, 4

type t =
  | X of int
  | M of int
  | A of int
  | S of int

let build_hash (line_number : int) (line : string) tbl =
  line
  |> String.iteri (fun column_number char ->
    let value =
      match char with
      | 'X' -> Some 'X'
      | 'M' -> Some 'M'
      | 'A' -> Some 'A'
      | 'S' -> Some 'S'
      | _ -> None
    in
    let key = line_number, column_number in
    Hashtbl.replace tbl key value);
  tbl
;;

let directions = [ 1, 0; -1, 0; 0, 1; 0, -1; -1, -1; -1, 1; 1, -1; 1, 1 ]

let search_dirdction tbl (row, col) (row_step, col_step) =
  let target_word = "XMAS" in
  let rec aux i prev_chars =
    if i = String.length target_word then
      prev_chars
    else (
      let current_row = row + (i * row_step) in
      let current_col = col + (i * col_step) in
      match Hashtbl.find_opt tbl (current_row, current_col), prev_chars with
      | Some (Some char), Some str when char = target_word.[i] ->
        aux (i + 1) (Some (str ^ String.make 1 char))
      | _ -> None)
  in
  aux 0 None
;;

let count_xmas tbl row col =
  List.fold_left
    (fun count dir ->
      match search_dirdction tbl (row, col) dir with
      | Some _ -> count + 1
      | None -> count)
    0
    directions
;;

(* Just the worst thing I've written that unfortunately works *)
let find tbl =
  Hashtbl.fold
    (fun ((row_number : int), (column_number : int))
      (value : char option)
      (acc : int) ->
      match value with
      | Some c when c = 'X' ->
        let prev_chars = String.make 1 c in
        let rec search_down l (prev_chars : string option) =
          match l with
          | 4 -> prev_chars
          | i -> (
            match
              Hashtbl.find_opt tbl (row_number + i, column_number), prev_chars
            with
            | Some (Some char), Some str when char = 'M' && str = "X" ->
              search_down (i + 1) (Some (str ^ String.make 1 char))
            | Some (Some char), Some str when char = 'A' && str = "XM" ->
              search_down (i + 1) (Some (str ^ String.make 1 char))
            | Some (Some char), Some str when char = 'S' && str = "XMA" ->
              search_down (i + 1) (Some (str ^ String.make 1 char))
            | _, _ -> search_down (i + 1) None)
        in
        let rec search_up l (prev_chars : string option) =
          match l with
          | 4 -> prev_chars
          | i -> (
            match
              Hashtbl.find_opt tbl (row_number - i, column_number), prev_chars
            with
            | Some (Some char), Some str when char = 'M' && str = "X" ->
              search_up (i + 1) (Some (str ^ String.make 1 char))
            | Some (Some char), Some str when char = 'A' && str = "XM" ->
              search_up (i + 1) (Some (str ^ String.make 1 char))
            | Some (Some char), Some str when char = 'S' && str = "XMA" ->
              search_up (i + 1) (Some (str ^ String.make 1 char))
            | _, _ -> search_up (i + 1) None)
        in
        let rec search_left l (prev_chars : string option) =
          match l with
          | 4 -> prev_chars
          | i -> (
            match
              Hashtbl.find_opt tbl (row_number, column_number - i), prev_chars
            with
            | Some (Some char), Some str when char = 'M' && str = "X" ->
              search_left (i + 1) (Some (str ^ String.make 1 char))
            | Some (Some char), Some str when char = 'A' && str = "XM" ->
              search_left (i + 1) (Some (str ^ String.make 1 char))
            | Some (Some char), Some str when char = 'S' && str = "XMA" ->
              search_left (i + 1) (Some (str ^ String.make 1 char))
            | _, _ -> search_left (i + 1) None)
        in
        let rec search_right l (prev_chars : string option) =
          match l with
          | 4 -> prev_chars
          | i -> (
            match
              Hashtbl.find_opt tbl (row_number, column_number + i), prev_chars
            with
            | Some (Some char), Some str when char = 'M' && str = "X" ->
              search_right (i + 1) (Some (str ^ String.make 1 char))
            | Some (Some char), Some str when char = 'A' && str = "XM" ->
              search_right (i + 1) (Some (str ^ String.make 1 char))
            | Some (Some char), Some str when char = 'S' && str = "XMA" ->
              search_right (i + 1) (Some (str ^ String.make 1 char))
            | _, _ -> search_right (i + 1) None)
        in
        let rec search_right_down l (prev_chars : string option) =
          match l with
          | 4 -> prev_chars
          | i -> (
            match
              ( Hashtbl.find_opt tbl (row_number + i, column_number + i)
              , prev_chars )
            with
            | Some (Some char), Some str when char = 'M' && str = "X" ->
              search_right_down (i + 1) (Some (str ^ String.make 1 char))
            | Some (Some char), Some str when char = 'A' && str = "XM" ->
              search_right_down (i + 1) (Some (str ^ String.make 1 char))
            | Some (Some char), Some str when char = 'S' && str = "XMA" ->
              search_right_down (i + 1) (Some (str ^ String.make 1 char))
            | _, _ -> search_right_down (i + 1) None)
        in
        let rec search_left_down l (prev_chars : string option) =
          match l with
          | 4 -> prev_chars
          | i -> (
            match
              ( Hashtbl.find_opt tbl (row_number + i, column_number - i)
              , prev_chars )
            with
            | Some (Some char), Some str when char = 'M' && str = "X" ->
              search_left_down (i + 1) (Some (str ^ String.make 1 char))
            | Some (Some char), Some str when char = 'A' && str = "XM" ->
              search_left_down (i + 1) (Some (str ^ String.make 1 char))
            | Some (Some char), Some str when char = 'S' && str = "XMA" ->
              search_left_down (i + 1) (Some (str ^ String.make 1 char))
            | _, _ -> search_left_down (i + 1) None)
        in
        let rec search_left_up l (prev_chars : string option) =
          match l with
          | 4 -> prev_chars
          | i -> (
            match
              ( Hashtbl.find_opt tbl (row_number - i, column_number - i)
              , prev_chars )
            with
            | Some (Some char), Some str when char = 'M' && str = "X" ->
              search_left_up (i + 1) (Some (str ^ String.make 1 char))
            | Some (Some char), Some str when char = 'A' && str = "XM" ->
              search_left_up (i + 1) (Some (str ^ String.make 1 char))
            | Some (Some char), Some str when char = 'S' && str = "XMA" ->
              search_left_up (i + 1) (Some (str ^ String.make 1 char))
            | _, _ -> search_left_up (i + 1) None)
        in
        let rec search_right_up l (prev_chars : string option) =
          match l with
          | 4 -> prev_chars
          | i -> (
            match
              ( Hashtbl.find_opt tbl (row_number - i, column_number + i)
              , prev_chars )
            with
            | Some (Some char), Some str when char = 'M' && str = "X" ->
              search_right_up (i + 1) (Some (str ^ String.make 1 char))
            | Some (Some char), Some str when char = 'A' && str = "XM" ->
              search_right_up (i + 1) (Some (str ^ String.make 1 char))
            | Some (Some char), Some str when char = 'S' && str = "XMA" ->
              search_right_up (i + 1) (Some (str ^ String.make 1 char))
            | _, _ -> search_right_up (i + 1) None)
        in
        let down = search_down 1 (Some prev_chars) in
        let up = search_up 1 (Some prev_chars) in
        let left = search_left 1 (Some prev_chars) in
        let right = search_right 1 (Some prev_chars) in
        let up_right = search_right_up 1 (Some prev_chars) in
        let up_left = search_left_up 1 (Some prev_chars) in
        let down_left = search_left_down 1 (Some prev_chars) in
        let down_right = search_right_down 1 (Some prev_chars) in
        let count =
          match down with
          | Some v when v = "XMAS" -> 1
          | _ -> 0
        in
        let count =
          count
          +
          match up with
          | Some v when v = "XMAS" -> 1
          | _ -> 0
        in
        let count =
          count
          +
          match left with
          | Some v when v = "XMAS" -> 1
          | _ -> 0
        in
        let count =
          count
          +
          match right with
          | Some v when v = "XMAS" -> 1
          | _ -> 0
        in
        let count =
          count
          +
          match up_right with
          | Some v when v = "XMAS" -> 1
          | _ -> 0
        in
        let count =
          count
          +
          match up_left with
          | Some v when v = "XMAS" -> 1
          | _ -> 0
        in
        let count =
          count
          +
          match down_left with
          | Some v when v = "XMAS" -> 1
          | _ -> 0
        in
        let count =
          count
          +
          match down_right with
          | Some v when v = "XMAS" -> 1
          | _ -> 0
        in
        acc + count
      | _ -> acc)
    tbl
    0
;;

let solution1 =
  let tbl = Hashtbl.create 1000 in
  let s = File_reader.read_file file in
  List.iteri
    (fun (line_number : int) (line : string) ->
      let _ = build_hash line_number line tbl in
      ())
    s;
  let _ = find tbl in
  ()
;;
