open Utils

let data = 
  "inputs/1.txt"
  |> read_lines
  |> List.map (fun s ->
      String.(
        (get s 0 = 'L'), 
        (sub s 1 (String.length s - 1)) |> int_of_string
      )
    )

let iter f n a = List.init n Fun.id |> List.fold_left (fun acc _ -> f acc) a

let move s dir pos m =
  iter (fun (pos,rounds) ->
    let new_pos = (if dir then pos - 1 else pos + 1) mod m in
    (new_pos, rounds + if new_pos = 0 then 1 else 0)
  ) s (pos,0)

let () =
  data
  |> List.fold_left (fun (pos,count, total_count) (dir, steps) ->
      let new_pos, rounds = move steps dir pos 100 in
      (new_pos, count + (if new_pos = 0 then 1 else 0), total_count + rounds)
    ) (50,0,0)
  |> (fun (_,count, total_count) -> 
      dump_int 1 count;
      dump_int 2 total_count
  )
