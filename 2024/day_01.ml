let filename = "./day_01_input.txt"
let try_read ic = try Some (input_line ic) with End_of_file -> None

let line_to_ints s =
  Str.split_delim (Str.regexp "   ") s |> List.map int_of_string

let rec read_lines left right ic =
  match try_read ic with
  | Some line -> (
      line_to_ints line |> fun ints ->
      match ints with
      | [ a; b ] -> read_lines (a :: left) (b :: right) ic
      | _ -> failwith "invalid")
  | None ->
      close_in ic;
      (List.sort compare left, List.sort compare right)

let calc_sum left right =
  List.map2 ( - ) left right |> List.map abs |> List.fold_left ( + ) 0

let calc_sim left right =
  List.map
    (fun x -> x * (List.filter (fun y -> x == y) right |> List.length))
    left
  |> List.fold_left ( + ) 0

let () =
  let left, right = open_in filename |> read_lines [] [] in
  calc_sum left right |> Printf.sprintf "Sum %d" |> print_endline;
  calc_sim left right |> Printf.sprintf "Part 2 Simularity %d" |> print_endline
