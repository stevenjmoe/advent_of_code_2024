let read_file file f () = 
  let ic = open_in file in
  let try_read () = 
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
  | Some line -> 
    f line;
    loop (line :: acc)
  | None -> 
      close_in ic; 
      List.rev acc in
      let _ = loop [] in
      ()
