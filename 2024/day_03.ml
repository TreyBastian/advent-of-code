let filename = "./day_03_input.txt"
let try_read ic = try Some (input_line ic) with End_of_file -> None

let read_file ic =
  let rec read acc =
    match try_read ic with
    | Some line -> read (line :: acc)
    | None ->
        close_in ic;
        acc
  in
  read [] |> List.rev |> String.concat ""

let regex_mul = Str.regexp "mul(\\([0-9]+\\),\\([0-9]+\\))"
let regex_mul_part_2 = Str.regexp "mul(\\([0-9]+\\),\\([0-9]+\\))\\|don't()"
let regex_do_part_2 = Str.regexp "do()"

let calculate_total str regexp =
  let rec calculate str regexp acc =
    try
      let _ = Str.search_forward regexp str 0 in
      let cmd = Str.matched_group 0 str in
      let new_str =
        String.sub str (Str.match_end ()) (String.length str - Str.match_end ())
      in
      match cmd with
      | "do()" -> calculate new_str regex_mul_part_2 acc
      | "don't()" -> calculate new_str regex_do_part_2 acc
      | cmd when String.starts_with ~prefix:"mul" cmd ->
          let a = Str.matched_group 1 str |> int_of_string in
          let b = Str.matched_group 2 str |> int_of_string in
          calculate new_str regexp (acc + (a * b))
      | _ -> acc
    with Not_found -> acc
  in
  calculate str regexp 0

let () =
  let ic = open_in filename in
  let data = read_file ic in
  calculate_total data regex_mul |> Printf.sprintf "Sum %d" |> print_endline;

  calculate_total data regex_mul_part_2
  |> Printf.sprintf "Part 2 Sum %d"
  |> print_endline
