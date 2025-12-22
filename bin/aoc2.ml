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
  List.init (length / 2) (fun i ->
    let part_len = i + 1 in
    length mod part_len = 0
    && String.sub str_id 0 (length - part_len)
       = String.sub str_id part_len (length - part_len))
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
