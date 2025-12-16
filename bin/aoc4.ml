open Utils

type content = Paper | Empty

let data = 
  "inputs/4_1.txt"
  |> read_lines
  |> List.map explode
  |> List.map (List.map (function '@' -> Paper | '.' -> Empty | _ -> failwith "invalid input"))

let center entry = List.nth (List.nth entry 1) 1

let accessible entry =
  center entry = Paper && 
  entry 
  |> List.flatten
  |> List.filter (fun x -> x = Paper) 
  |> List.length <= 4

let next neighborhoods = 
  neighborhoods
  |> List.map (List.map (fun entry ->
      if accessible entry then Empty else center entry
  ))


let accessible neighborhoods =
  neighborhoods
  |> List.flatten
  |> List.filter accessible
  |> List.length

let () =
  data
  |> zip_neighbors Empty
  |> accessible
  |> dump_int 1

let () =
  let rec count board = 
    let neighborhoods = zip_neighbors Empty board in
    let amount = accessible neighborhoods in
    let new_board = next neighborhoods in
    if amount > 0 then
      amount + count new_board
    else
      0
  in
  count data
  |> dump_int 2
