open Utils

let data =
  "inputs/9_1.txt"
  |> read_lines
  |> List.map (fun row ->
      row
      |> String.split_on_char ','
      |> List.map int_of_string
      |> function
         | [x; y] -> (x, y)
         | _ -> failwith "Invalid input"
  )

let () =
  List.map (fun (x1, y1) ->
    List.map (fun (x2, y2) ->
      abs (x1 - x2 + 1) * abs (y1 - y2 + 1)
    ) data
  ) data
  |> List.flatten
  |> List.sort compare
  |> List.rev
  |> List.hd
  |> dump_int 1


let x_coord =
  List.map fst data
  |> List.sort_uniq compare
  |> List.mapi (fun i x -> (x, i+1))

let y_coord =
  List.map snd data
  |> List.sort_uniq compare
  |> List.mapi (fun i y -> (y, i+1))

let data =
  List.map (fun (x, y) ->
    let x' = List.assoc x x_coord in
    let y' = List.assoc y y_coord in
    (x', y')
  ) data




(* List instead of Array construction adds 1.5s *)
let field =
  let max_x = List.fold_left (fun acc (x, _) -> max acc x) 0 data + 2 in
  let max_y = List.fold_left (fun acc (_, y) -> max acc y) 0 data + 2 in
  (* let arr = Array.init max_x (fun _ -> Array.init max_y (fun _ -> 'X')) in *)
  let lines = zipNext (data @ [List.hd data]) in
  let field = List.init max_x (fun x -> List.init max_y (fun y ->
    if List.exists (fun ((x1,y1),(x2,y2)) ->
      let min_x, max_x = min x1 x2, max x1 x2 in
      let min_y, max_y = min y1 y2, max y1 y2 in
      x >= min_x && x <= max_x && y >= min_y && y <= max_y
    ) lines then '#' else 'X'
  )) in
  let rec flood_fill x y field =
    if x < 0 || y < 0 || x >= max_x || y >= max_y then field
    else
      match List.nth (List.nth field x) y with
      | 'X' ->
        let positions = [(x+1,y); (x-1,y); (x,y+1); (x,y-1)] in
        List.fold_left (fun f (nx, ny) ->
          flood_fill nx ny f
        ) (List.mapi (fun ix row ->
          if ix = x then
            List.mapi (fun iy cell ->
              if iy = y then '.' else cell
            ) row
          else row
        ) field) positions
      | _ -> field
  in
  flood_fill 0 0 field
  (* |> List.map Array.of_list
  |> Array.of_list *)

(* let () =
  List.iter (fun row ->
    let line = String.init (List.length row) (fun i -> List.nth row i) in
    print_endline line
  ) field *)

let () =
  List.mapi (fun i (x1, y1) ->
    List.mapi (fun j (x2, y2) ->
      ((i,j),(x1,x2),(y1,y2))
    ) data
  ) data
  |> List.flatten
  |> List.filter_map (fun ((i,j),(x1,x2),(y1,y2)) ->
    if i >= j then None
    else
      Some (
        ((min x1 x2, min y1 y2), (max x1 x2, max y1 y2))
      )
  )
  |> List.filter (fun ((x1,y1),(x2,y2)) ->
    let rec check_between x y =
      if x <= x2 then
        if y <= y2 then
          (* List instead of array access adds 10s *)
          match List.nth (List.nth field x) y with
          (* match field.(x).(y) with *)
          | '.' -> false
          | '#' | 'X' -> check_between x (y+1)
          | _ -> failwith "Invalid field"
        else
          check_between (x+1) y1
      else
        true
    in
    check_between x1 y1
  )
  |> List.map (fun ((x1,y1),(x2,y2)) ->
    let x1org = List.assoc x1 (List.map (fun (x,i) -> (i,x)) x_coord) in
    let y1org = List.assoc y1 (List.map (fun (y,i) -> (i,y)) y_coord) in
    let x2org = List.assoc x2 (List.map (fun (x,i) -> (i,x)) x_coord) in
    let y2org = List.assoc y2 (List.map (fun (y,i) -> (i,y)) y_coord) in
    let area = (x2org - x1org + 1) * (y2org - y1org + 1) in
    (* Printf.printf "Checking points: (%d,%d) (%d,%d) -> (%d,%d) (%d,%d) (area %d)\n" x1 y1 x2 y2 x1org y1org x2org y2org area; *)
    (area, (x1,y1),(x2,y2))
  )
  |> List.sort compare
  |> List.rev
  |> List.hd
  |> (fun (area, _, _) ->
    dump_int 2 area
  )
