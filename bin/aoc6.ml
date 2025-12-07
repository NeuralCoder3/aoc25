open Utils

[@@@ocaml.warning "-8"]
let (lines, operands) = 
  let lines = "inputs/6_1.txt" |> read_lines in
  let operands::lines = List.rev lines in
  let lines = List.rev lines in
  let operands = operands |> String.split_on_char ' ' |> List.filter (fun s -> s <> "") in
  (lines, operands)

let lines1 = 
  lines
  |> List.map (fun line ->
    line
    |> String.split_on_char ' '
    |> List.filter (fun s -> s <> "")
    |> List.map int_of_string
  )
  |> transpose

let () = assert (List.length lines1 = List.length operands)

let data =
  List.map2 (fun nums op ->
    match op with
    | "*" -> List.fold_left (fun acc x -> acc * x) 1 nums
    | "+" -> List.fold_left (fun acc x -> acc + x) 0 nums
    | _ -> failwith ("Unknown operand: " ^ op)
  ) lines1 operands

let () =
  data
  |> List.fold_left (+) 0
  |> dump_int 1




let lines = 
  let lines = List.map (fun l -> List.rev(explode l)) lines in
  let rec process acc xxs =
    if List.for_all (fun xs -> xs = []) xxs then
      [acc]
    else
      let front = List.map List.hd xxs in
      if List.for_all (fun c -> c = ' ') front then
        acc :: process [] (List.map List.tl xxs)
      else
        process (implode front::acc) (List.map List.tl xxs)
  in
  List.rev (process [] lines)
  |> List.map (fun group ->
    group
    |> List.map String.trim
    |> List.map int_of_string
  )

let () =
  List.map2 (fun nums op ->
    match op with
    | "*" -> List.fold_left (fun acc x -> acc * x) 1 nums
    | "+" -> List.fold_left (fun acc x -> acc + x) 0 nums
    | _ -> failwith ("Unknown operand: " ^ op)
  ) lines operands
  |> List.fold_left (+) 0
  |> dump_int 2