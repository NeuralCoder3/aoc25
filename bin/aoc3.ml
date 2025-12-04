open Utils

let data = 
  "inputs/3_1.txt"
  |> read_lines
  |> List.map (fun s -> explode s |> List.map (fun c -> int_of_char c - int_of_char '0'))

let joltage xs =
  let rec aux xs =
    match xs with
    | [] -> 0
    | [_] -> 0 (* do not try to use 0 as second value *)
    | x1::rest -> 
      (* either take first as max or try later values *)
      max (10 * x1 + List.fold_left max 0 rest)
      (aux rest)
  in
  aux xs

let joltage ?(n=2) xs =
  let xs = 
    List.mapi (fun i x -> (x,i)) xs
    |> List.sort (fun (x1,idx1) (x2,idx2) -> compare (-x1,idx1) (-x2,idx2))
  in
  (* Printf.printf "Sorted joltages: %s\n" (String.concat "," (List.map (fun (x,_) -> string_of_int x) xs)); *)
  let len = List.length xs in
  let rec select n min_idx =
    if n = 0 then 
      []
    else
      let ys = xs |> List.filter (fun (_,idx) -> idx > min_idx && idx <= len-n) in
      (* Printf.printf "  Selecting %d from candidates: %s\n" n (String.concat "," (List.map (fun (x,idx) -> Printf.sprintf "%d(%d)" x idx) ys)); *)
      let (sval,sidx) = List.hd ys in
      sval :: select (n-1) sidx
  in
  select n (-1)



let () =
  [(1,2); (2,12)]
  |> List.iter (fun (part, n) ->
    data
    |> List.map (fun row -> 
      let batteries = joltage ~n:n row in
      let v = List.fold_left (fun acc x -> acc * 10 + x) 0 batteries in
      (* Printf.printf "Joltage of row %s: %d\n" (String.concat "," (List.map (fun x -> String.make 1 (char_of_int (x + int_of_char '0'))) row)) v; *)
      v
    )
    |> List.fold_left (+) 0
    |> dump_int part
  )