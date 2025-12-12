open Utils

let data =
  "inputs/11_1.txt"
  |> read_lines
  |> List.map (fun row ->
    let parts = String.split_on_char ':' row in
    (List.hd parts,
      List.hd (List.tl parts)
      |> String.split_on_char ' '
      |> List.map String.trim
      |> List.filter (fun s -> s <> "")
    )
  )

let paths goal paths node =
  if node = goal then 1 else
  match List.assoc_opt node data with
  | Some ps ->
      ps
      |> List.map (fun next -> paths next)
      |> List.fold_left (+) 0
  | None ->
    0 (* for other goals than out *)
    (* failwith ("No paths for node " ^ node) *)

let paths ?(goal="out") = memo_rec (paths goal)

let () =
  paths "you"
  |> dump_int 1

let () =
  (* (
    (paths "svr" ~goal:"fft") *
    (paths "fft" ~goal:"dac") *
    (paths "dac" ~goal:"out")
  ) *)
  List.map (fun (p1,p2) ->
    List.map (fun (from, to_) ->
      paths from ~goal:to_
    ) [("svr",p1); (p1,p2); (p2,"out")]
    |> List.fold_left ( * ) 1
  ) [("fft","dac"); ("dac","fft")]
  |> List.fold_left (+) 0
  |> dump_int 2

let paths paths (node,(dac,fft)) =
  if node = "out" then
    (if dac && fft then 1 else  0)
  else
  match List.assoc_opt node data with
  | Some ps ->
      ps
      |> List.map (fun next -> paths (next,
        (dac || next = "dac",
         fft || next = "fft"))
      )
      |> List.fold_left (+) 0
  | None -> failwith ("No paths for node " ^ node)

let paths node = memo_rec paths (node,(false,false))

let () =
  paths "svr"
  |> dump_int 2
