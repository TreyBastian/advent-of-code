let filename = "./day_02_input.txt"
let try_read ic = try Some (input_line ic) with End_of_file -> None

let is_values_safe a b direction =
  match direction with
  | "up" -> a < b && abs (a - b) < 4
  | "down" -> a > b && abs (a - b) < 4
  | _ -> false

let direction a b = if a < b then "up" else "down"

let is_line_safe list =
  let a, b = (List.hd list, List.tl list |> List.hd) in
  let direction = direction a b in
  List.fold_left
    (fun (acc, prev) x ->
      if prev > -1 then (acc && is_values_safe prev x direction, x) else (acc, x))
    (true, -1) list
  |> fst

let is_line_safe_part_2 list =
  List.mapi
    (fun i _ -> List.filteri (fun j _ -> j <> i) list |> is_line_safe)
    list
  |> List.exists (fun x -> x)

let calculate_safe_score ic is_line_safe =
  let rec calculate acc =
    match try_read ic with
    | Some line -> (
        match
          is_line_safe (String.split_on_char ' ' line |> List.map int_of_string)
        with
        | true -> calculate (acc + 1)
        | false -> calculate acc)
    | None ->
        close_in ic;
        acc
  in
  calculate 0

let () =
  let ic = open_in filename in
  calculate_safe_score ic is_line_safe
  |> Printf.sprintf "Safe %d" |> print_endline;

  let ic = open_in filename in
  calculate_safe_score ic is_line_safe_part_2
  |> Printf.sprintf "Safe %d" |> print_endline
