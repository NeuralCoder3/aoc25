open Utils

[@@@warning "-8"]
let (ranges, ids) = 
  let [ranges; ids] = "inputs/5_1.txt"
    |> read_lines
    |> group_by (fun s -> s = "")
  in
  let ranges = 
    List.map (fun s -> 
      Scanf.sscanf s "%d-%d" (fun a b -> (a,b))
    ) ranges
  in
  let ids = 
    List.map int_of_string ids
  in
  (ranges, ids)

let in_range x (a,b) = a <= x && x <= b

let () =
  ids
  |> List.filter (fun id -> List.exists (in_range id) ranges)
  |> List.length
  |> dump_int 1

let ranges = 
  ranges 
  |> List.filter (fun (a,b) -> a <= b)
  |> List.sort (fun (a,_) (b,_) -> compare a b)

(* let () = 
  Printf.printf "Sorted ranges:\n";
  List.iter (fun (a,b) -> Printf.printf "  %d-%d\n" a b) ranges *)

let merged_ranges =
  let rec aux ranges =
    match ranges with
    | [] -> []
    | [r] -> [r]
    | (a1,b1)::(a2,b2)::rs ->
      if b1 >= a2 then
        aux ((a1, max b1 b2)::rs)
      else
        (a1,b1)::aux ((a2,b2)::rs)
  in
  aux ranges

(* let () = 
  Printf.printf "Merged ranges:\n";
  List.iter (fun (a,b) -> Printf.printf "  %d-%d\n" a b) merged_ranges *)

let () =
  merged_ranges
  |> List.fold_left (fun acc (a,b) -> acc + (b - a + 1)) 0
  |> dump_int 2
