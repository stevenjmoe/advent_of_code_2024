let file = "input/day1.txt"

let solution = 
    File_reader.read_file file (fun line ->
      let location_ids = line |> String.split_on_char ' ' in
      let id1 = location_ids |>  List.hd in
      let id2 = 3 |>  List.nth location_ids in

      print_endline @@ Printf.sprintf "%s %s" id1 id2;) 
    ()
