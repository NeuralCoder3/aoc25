open Utils

let data =
  "inputs/12_1.txt"
  |> read_lines
  |> group_by (fun row -> row = "")

[@@@warning "-8"]
let goals =
  data
  |> List.rev |> List.hd
  |> List.map (fun row ->
    let [size;counts] = split ": " row in
    let [w;h] = String.split_on_char 'x' size in
    ((int_of_string w, int_of_string h),
      String.split_on_char ' ' counts
      |> List.map int_of_string
    )
  )

let figures =
  data
  |> List.rev |> List.tl |> List.rev
  |> List.map (fun rows ->
    List.tl rows
    |> List.map (fun row ->
      explode row |> List.map (fun c -> c = '#')
    )
  )

let rotate figure =
  let h = List.length figure in
  let w = List.length (List.hd figure) in
  List.init w (fun x ->
    List.init h (fun y ->
      List.nth (List.nth figure (h - 1 - y)) x
    )
  )

let flip figure =
  List.map List.rev figure

let shapes =
  figures
  |> List.map (fun figure ->
    let rec aux seen current =
      if List.exists (fun f -> f = current) seen then seen else
      let seen = current :: seen in
      let rotated = rotate current in
      let flipped = flip current in
      let seen = aux seen rotated in
      aux seen flipped
    in
    aux [] figure
  )

let rec solvable ((w,h),counts) =
  (*
    Alternative proxy: true
    This problem is NP-hard.
    Except for very small instances, there is no efficient way to disprove solvability (or find non-trivial solutions).
  *)

  let field = Array.make_matrix h w false in

  let normalized_shapes =
    List.map (fun variants ->
      List.map (fun shape ->
        let coords =
          shape
          |> List.mapi (fun r row ->
               List.mapi (fun c is_set -> if is_set then Some (r, c) else None) row)
          |> List.flatten
          |> List.filter (function Some _ -> true | None -> false)
          |> List.map (function Some x -> x | None -> failwith "impossible")
        in
        match coords with
        | [] -> []
        | (r0, c0) :: rest ->
            (0, 0) :: List.map (fun (r, c) -> (r - r0, c - c0)) rest
      ) variants
    ) shapes
  in

  let rec solve current_counts =
    let rec find_available_piece_index idx list =
      match list with
      | [] -> None
      | c :: _ when c > 0 -> Some idx
      | _ :: rest -> find_available_piece_index (idx + 1) rest
    in

    match find_available_piece_index 0 current_counts with
    | None -> 
        (* (
          Printf.printf "Solution:\n";
          for r = 0 to h - 1 do
            for c = 0 to w - 1 do
              Printf.printf "%c" (if field.(r).(c) then '#' else '.')
            done;
            Printf.printf "\n"
          done;
        ); *)
        true
    | Some p_idx ->
        let variants = List.nth normalized_shapes p_idx in

        let rec try_positions r c =
          if r >= h then false 
          else
            let next_r, next_c = if c + 1 = w then (r + 1, 0) else (r, c + 1) in

            if field.(r).(c) then 
              try_positions next_r next_c
            else
              let rec try_variants vars =
                match vars with
                | [] -> false
                | offsets :: rest_vars ->
                    let fits =
                      List.for_all (fun (dr, dc) ->
                        let nr, nc = r + dr, c + dc in
                        nr >= 0 && nr < h && nc >= 0 && nc < w && not field.(nr).(nc)
                      ) offsets
                    in

                    if fits then (
                      List.iter (fun (dr, dc) -> field.(r + dr).(c + dc) <- true) offsets;
                      
                      let new_counts = 
                        List.mapi (fun i n -> if i = p_idx then n - 1 else n) current_counts 
                      in
                      
                      if solve new_counts then true
                      else (
                        List.iter (fun (dr, dc) -> field.(r + dr).(c + dc) <- false) offsets;
                        try_variants rest_vars
                      )
                    ) else (
                      try_variants rest_vars
                    )
              in

              if try_variants variants then true
              else try_positions next_r next_c
        in
        try_positions 0 0
  in

  solve counts
          




let () =
  goals
  |> List.map (fun (((w,h), counts) as goal) ->
    let max_size = 9 * List.fold_left (+) 0 counts in
    if w * h > max_size then Either.Left true else
    let min_size =
      List.fold_left (fun sum (i,count) ->
        let figure = List.nth figures i in
        let size = List.fold_left (fun acc row ->
          acc + List.fold_left (fun acc c -> if c then acc + 1 else acc) 0 row
        ) 0 figure in
        sum + size * count
      ) 0 (List.mapi (fun i c -> (i,c)) counts)
    in
    if w * h < min_size then Either.Left false else
    Either.Right goal
  )
  |> List.map (function
    | Either.Left x -> Either.Left x
    | Either.Right goal -> Either.Left (solvable goal)
  )
  |> (fun xs ->
    Printf.printf "%d unsolvable, %d solvable, %d undecided\n"
      (List.filter ((=) (Either.Left false)) xs |> List.length)
      (List.filter ((=) (Either.Left true)) xs |> List.length)
      (List.filter (function Either.Right _ -> true | _ -> false) xs |> List.length)
  )
  (* |> dump_int 1 *)
