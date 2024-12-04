let filename = "./day_01_input.txt"

let () =
  let ic = open_in filename in

  let try_read () = try Some (input_line ic) with End_of_file -> None in

  let rec read_lines left right =
    match try_read () with
    | Some line -> (
        let ints =
          Str.split_delim (Str.regexp "   ") line |> List.map int_of_string
        in
        match ints with
        | [ a; b ] -> read_lines (a :: left) (b :: right)
        | _ -> failwith "invalid")
    | None ->
        close_in ic;
        (List.sort compare left, List.sort compare right)
  in
  let left, right = read_lines [] [] in
  let sum =
    List.map2 ( - ) left right |> List.map abs |> List.fold_left ( + ) 0
  in
  Printf.sprintf "Sum %d" sum |> print_endline;
  let sim =
    List.map
      (fun x -> x * (List.filter (fun y -> x == y) right |> List.length))
      left
    |> List.fold_left ( + ) 0
  in
  Printf.sprintf "Sim %d" sim |> print_endline
