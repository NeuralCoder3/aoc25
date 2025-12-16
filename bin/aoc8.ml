open Utils

(* let file, amount = "inputs/8_0.txt", 10 *)
let file, amount = "inputs/8_1.txt", 1000

let data =
  file
  |> read_lines
  |> List.map (fun row ->
      row
      |> split ","
      |> List.map int_of_string
  )

(* squared distance to avoid float/sqrt *)
let dist xs ys =
  List.map2 (fun x y -> (x - y) * (x - y)) xs ys
  |> List.fold_left (+) 0

let all_distances =
  List.mapi (fun i point ->
    List.mapi (fun j other ->
      if i >= j then [] else
      [(dist point other, (i, point), (j, other))]
    ) data
  ) data
  |> List.flatten
  |> List.flatten
  |> List.sort (fun (d1, _, _) (d2, _, _) -> compare d1 d2)


(* this would be much faster with a real union find *)

let rec find i uf =
  let parent = List.nth uf i in
  if parent = i then i
  else find parent uf

let union i j uf =
  let root_i = find i uf in
  let root_j = find j uf in
  if root_i = root_j then
    uf
  else
    (* Update the list: make root_i point to root_j *)
    List.mapi (fun k p -> if k = root_i then root_j else p) uf

let init_uf size =
  List.init size (fun i -> i)

let clusters uf =
  let rec aux seen = function
    | [] -> seen
    | i::is ->
      let root = find i uf in
      match List.find_opt (fun (r, _) -> r = root) seen with
      | Some (_, _members) ->
        let new_seen = List.map (fun (r, ms) ->
          if r = root then (r, i::ms) else (r, ms)
        ) seen in
        aux new_seen is
      | None ->
        aux ((root, [i]) :: seen) is
  in
  aux [] (List.init (List.length uf) (fun i -> i))

let all_same_cluster uf =
  let root = find 0 uf in
  List.for_all (fun i -> find i uf = root) (List.init (List.length uf) (fun i -> i))

let _ =
  let uf = init_uf (List.length data) in
  List.fold_left (fun (uf, count, found) (_d, (id1, p1), (id2, p2)) ->
      let uf = union id1 id2 uf in
      (* (if count > 0 then
        Printf.printf "Distance: %d between point %d (%s) and point %d (%s)\n"
          d id1 (String.concat "," (List.map string_of_int p1)) id2 (String.concat "," (List.map string_of_int p2))
      ); *)
      let new_found = (
        (* needs 20s *)
        if not found && all_same_cluster uf then
          (Printf.printf "All points connected. Final x product: %d\n%!" ( List.hd p1 * List.hd p2); true)
        else found
      ) in
      (if count == 1 then
        clusters uf
        |> List.map (fun (_root, members) -> List.length members)
        |> List.sort compare
        |> List.rev
        (* |> (fun x -> (List.iteri (fun i size -> Printf.printf "Cluster %d size: %d\n" i size) x;x)) *)
        |> take 3
        |> List.fold_left ( * ) 1
        |> Printf.printf "Clusters product (first 3): %d\n%!"
      );
      (uf, count - 1, new_found)
  ) (uf, amount, false) all_distances
