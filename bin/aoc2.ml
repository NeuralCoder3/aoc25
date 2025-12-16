open Utils

let data = 
  "inputs/2_1.txt"
  |> read_lines
  |> List.hd
  |> String.split_on_char ','
  |> List.map (fun range ->
      let bounds = String.split_on_char '-' range in
      match bounds with
      | [min_s; max_s] -> 
        (int_of_string min_s, int_of_string max_s)
      | _ -> failwith "Invalid range format"
  )


let is_invalid id =
  let str_id = string_of_int id in
  let length = String.length str_id in
  length mod 2 = 0 && 
  let half = length / 2 in
  let first_half = String.sub str_id 0 half in
  let second_half = String.sub str_id half half in
  first_half = second_half


let is_invalid_2 id =
  let str_id = string_of_int id in
  let length = String.length str_id in
  List.init length (fun part_len ->
    part_len > 0 &&
    length mod part_len = 0 &&
    let count = length / part_len in
    let parts = List.init count (fun j ->
      String.sub str_id (j * part_len) part_len
    ) in
    let first_part = List.hd parts in
    List.for_all ((=) first_part) parts
  )
  |> List.exists Fun.id


let () =
  [(is_invalid,1); (is_invalid_2,2)] 
  |> List.iter (fun (is_invalid, part) ->
    data
    |> List.fold_left (fun acc (min, max) -> 
      let invalids = List.init (max - min + 1) (fun i -> min + i)
      |> List.filter is_invalid
      |> List.fold_left (+) 0
      (* |> (+) acc *)
      in
      (* Printf.printf "Range %d-%d has %d invalid IDs\n" min max invalids; *)
      acc + invalids
    ) 0
    |> dump_int part
  ) 