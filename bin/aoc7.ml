open Utils


let data = 
  "inputs/7_1.txt"
  |> read_lines
  |> List.map explode

let init = 
  List.hd data
  |> enumerate
  |> List.filter (fun (_, c) -> c = 'S')
  |> List.map fst

let data = 
  List.tl data
  |> List.map (List.map (fun c -> c='^'))

let next beams xs =
  (* foldl over xs until beam pos reached 
    used for linear time, else use mapi
  *)
  let rec aux beams i xs =
    match beams,xs with
    | [],_ -> []
    | _, [] -> (assert (beams = []); beams)
    | (b,c)::bs, x::xr when b = i ->
      if x then
        (b-1,c)::(b+1,c)::aux bs (i+1) xr
      else
        (b,c)::aux bs (i+1) xr
    | (b,_)::_, _::xr ->
      (assert (b > i);
      aux beams (i+1) xr)
  in
  assert (List.for_all (fun (b,_) -> b >=0 && b < List.length xs) beams);
  aux beams 0 xs

(* simpler version (not measurable slower) *)
let next beams row = 
  List.map (fun b ->
    match b with
    | (pos,count) when List.nth row pos ->
      [(pos-1,count); (pos+1,count)]
    | (pos,count) ->
      [(pos,count)]
  ) beams
  |> List.flatten

let rec dedup beams =
  match beams with
  | [] -> []
  | (b,c)::(b2,c2)::bs when b = b2 ->
    dedup ((b, c + c2)::bs)
  | (b,c)::bs ->
    (b,c)::dedup bs

let (splits,end_beams) =   
  data
  |> List.fold_left (fun (count,beams) row ->
      let new_beams = next beams row in
      let c = List.length new_beams - List.length beams in
      let new_beams = dedup new_beams in
      (count + c, new_beams)
  ) (0,List.map (fun i -> (i,1)) init)

let () =
    splits
    |> dump_int 1

let () =
  end_beams
  |> List.fold_left (fun acc (_,c) -> acc + c) 0
  |> dump_int 2

(* memoization version *)

let count count (pos,rows) =
  match rows with [] -> 1
  | row::rows ->
    if List.nth row pos then (* split happens *)
      count (pos-1,rows) + count (pos+1,rows)
    else
      count (pos,rows)

let () =
  memo_rec count (List.hd init, data)
  |> dump_int 2