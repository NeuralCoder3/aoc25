open Utils

type direction = Left | Right

let data = 
  "inputs/1.txt"
  |> read_lines
  |> List.map (fun s ->
      let dir = match String.get s 0 with
        | 'L' -> Left
        | 'R' -> Right
        | _ -> failwith "Invalid direction"
      in
      let steps = int_of_string (String.sub s 1 (String.length s - 1)) in
      (dir, steps)
    )

(* let rec clamped_modulo n m =
  if n = 0 then 
    0, 1
  else if n < 0 then
    let r, c = clamped_modulo (n + m) m in
    r, c + 1
  else if n >= m then
    let r, c = clamped_modulo (n - m) m in
    r, c + 1
  else
    n, 0 *)

let rec clamped_modulo2 n m =
  if 0 <= n && n < m then
    n, (if n = 0 then 1 else 0)
  else 
    let new_pos, circles = clamped_modulo2 (if n < 0 then n + m else n - m) m in
    new_pos, circles + 1
    

let () =
  data
  |> List.fold_left (fun (pos,count, total_count) (dir, steps) ->
      let new_pos = match dir with
        | Left -> pos - steps
        | Right -> pos + steps
      in
      let new_pos, steps = clamped_modulo2 new_pos 100 in
      let overlap = 
        (if pos = 0 && dir = Left then 1 else 0) + (* we counted the turn and the landing *)
        (if new_pos = 0 && dir = Right then 1 else 0)
      in
      let steps = steps - overlap in
      (* Printf.printf "From %d to %d in %d steps\n" pos new_pos steps; *)
      (
        new_pos, 
        count + (if pos = 0 then 1 else 0),
        total_count + steps
      )
    ) (50,0,0)
  |> (fun (_,count, total_count) -> 
      dump_int 1 count;
      dump_int 2 total_count
  )