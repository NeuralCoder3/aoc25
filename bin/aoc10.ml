open Utils

let data =
  "inputs/10_1.txt"
  |> read_lines
  |> List.map (fun row ->
      let parts = String.split_on_char ' ' row in
      let drop_first_and_last xs =
        xs
        |> List.tl
        |> (fun xs -> List.rev (List.tl (List.rev xs)))
      in
      let goal =
        List.hd parts
        |> explode
        |> drop_first_and_last
      in
      let joltage =
        List.hd (List.rev parts)
        |> explode
        |> drop_first_and_last
        |> implode
        |> split ","
        |> List.map int_of_string
      in
      let buttons =
        parts
        |> drop_first_and_last
        |> List.map (fun xs ->
            xs
            |> explode
            |> drop_first_and_last
            |> implode
            |> split ","
            |> List.map int_of_string
        )
      in
      let len = List.length goal in
      let goal_bin =
        goal
        |> List.map (fun c -> if c = '#' then 1 else 0)
        |> List.fold_left (fun acc x -> acc * 2 + x) 0
      in
      let buttons_bin =
        buttons
        |> List.map (fun btn ->
            List.init len (fun i -> List.exists (fun c -> c = i) btn)
            |> List.map (fun b -> if b then 1 else 0)
            |> List.fold_left (fun acc x -> acc * 2 + x) 0
        )
      in
      ((goal_bin, buttons_bin), (joltage, buttons))
    )

(* dfs too slow *)
(* let () =
  data
  |> List.map (fun (goal,joltage,buttons) ->
      let find_best find_best current =
        if current = 0 then
          0
        else
          List.fold_left (fun best btn ->
            if best > 0 then
              min best (find_best (current lxor btn) + 1)
            else
              best
          ) Int.max_int buttons
      in
      let find_best = memo_rec find_best in
      find_best goal
  )
  |> List.fold_left (+) 0
  |> dump_int 1 *)

let () =
  data
  |> List.map (fun ((goal,buttons), _) ->
      (* Initialize Queue with (current_value, steps_taken) *)
      let q = Queue.create () in
      Queue.add (goal, 0) q;

      (* Keep track of visited states to avoid cycles *)
      let visited = Hashtbl.create 1024 in
      Hashtbl.add visited goal ();

      (* Recursive function to process the Queue *)
      let rec run_bfs () =
        if Queue.is_empty q then
          0 (* or failwith "No solution" depending on guarantee *)
        else
          let current, steps = Queue.take q in
          if current = 0 then
            steps
          else begin
            buttons |> List.iter (fun btn ->
              let next_val = current lxor btn in
              if not (Hashtbl.mem visited next_val) then begin
                Hashtbl.add visited next_val ();
                Queue.add (next_val, steps + 1) q
              end
            );
            run_bfs ()
          end
      in
      run_bfs ()
  )
  |> List.fold_left (+) 0
  |> dump_int 1




(* Solution using smt solver *)

(* let () =
  data
  |> List.map (fun (_,(joltage,buttons)) ->
      let vars =
        List.init (List.length buttons) (fun i -> Smt.mkInt ("btn_" ^ string_of_int i))
      in
      let btn_constraints =
        List.mapi (fun i goal ->
          Smt.( int goal ==
            List.fold_left (fun acc (btn,var) ->
              acc + (var * (int (if List.exists Stdlib.(fun c -> c = i) btn then 1 else 0)))
            ) (int 0) (List.combine buttons vars)
          )
        ) joltage
      in
      let domain_constraints =
        vars
        |> List.map (fun v ->
          Smt.( (v >= int 0) )
        )
      in
      Smt.eval_opt
        (btn_constraints@domain_constraints)
        (List.fold_left Smt.(+) (Smt.int 0) vars)
        (fun res ->
          match res with
          | Error msg -> failwith msg
          | Ok model ->
            vars
            |> List.map (Smt.eval_int model)
            |> List.fold_left (+) 0
        )
  )
  |> List.fold_left (+) 0
  |> dump_int 2 *)




(* Minimal Rational Arithmetic Module to avoid floating point errors *)
module Rat = struct
  type t = { num : int; den : int }

  let rec gcd a b = if b = 0 then abs a else gcd b (a mod b)

  let make n d =
    if d = 0 then failwith "Division by zero";
    let common = gcd n d in
    let s = if (n < 0) <> (d < 0) then -1 else 1 in
    { num = s * (abs n / common); den = abs d / common }

  let of_int n = { num = n; den = 1 }
  let zero = { num = 0; den = 1 }
  let one = { num = 1; den = 1 }

  let add a b =
    make (a.num * b.den + b.num * a.den) (a.den * b.den)

  let sub a b =
    make (a.num * b.den - b.num * a.den) (a.den * b.den)

  let mul a b =
    make (a.num * b.num) (a.den * b.den)

  let div a b =
    make (a.num * b.den) (a.den * b.num)

  let is_zero a = a.num = 0
  let is_neg a = a.num < 0

  (* returns Some integer if denominator is 1, else None *)
  let to_int_opt a =
    if a.den = 1 then Some a.num else None
end

let solve_machine buttons targets =
  let num_vars = List.length buttons in
  let num_eqs = List.length targets in

  (* simplify buttons (together with target) *)
  let matrix = Array.make_matrix num_eqs (num_vars + 1) Rat.zero in

  List.iteri (fun btn_idx affected_counters ->
    List.iter (fun row_idx ->
      if row_idx < num_eqs then
        matrix.(row_idx).(btn_idx) <- Rat.one
    ) affected_counters
  ) buttons;

  List.iteri (fun row_idx target_val ->
    matrix.(row_idx).(num_vars) <- Rat.of_int target_val
  ) targets;



  (* Gaussian Elimination *)
  let pivot_cols = ref [] in
  let pivot_row = ref 0 in

  for col = 0 to num_vars - 1 do
    if !pivot_row < num_eqs then (
      let pivot_idx = ref !pivot_row in
      while !pivot_idx < num_eqs && Rat.is_zero matrix.(!pivot_idx).(col) do
        incr pivot_idx
      done;

      if !pivot_idx < num_eqs then (
        let temp = matrix.(!pivot_row) in
        matrix.(!pivot_row) <- matrix.(!pivot_idx);
        matrix.(!pivot_idx) <- temp;

        pivot_cols := col :: !pivot_cols;

        let scale = matrix.(!pivot_row).(col) in
        for c = col to num_vars do
          matrix.(!pivot_row).(c) <- Rat.div matrix.(!pivot_row).(c) scale
        done;

        for r = 0 to num_eqs - 1 do
          if r <> !pivot_row && not (Rat.is_zero matrix.(r).(col)) then
            let factor = matrix.(r).(col) in
            for c = col to num_vars do
              matrix.(r).(c) <- Rat.sub matrix.(r).(c) (Rat.mul factor matrix.(!pivot_row).(c))
            done
        done;

        incr pivot_row
      )
    )
  done;

  let pivots = List.rev !pivot_cols in

  let free_vars =
    List.init num_vars Fun.id
    |> List.filter (fun x -> not (List.mem x pivots))
  in

  (* recursive search for minimum integer solution *)
  let min_presses = ref None in
  let max_target = List.fold_left max 0 targets in

  (* we have at most max_target many presses *)
  let limit = if targets = [] then 0 else max_target in

  let rec search f_idx current_free_vals current_sum =
    match !min_presses with
    | Some m when current_sum >= m -> ()
    | _ ->
      if f_idx = List.length free_vars then (
        let calc_sum = ref current_sum in

        try
          List.iteri (fun r _col_idx ->
            let res = ref matrix.(r).(num_vars) in

            List.iter2 (fun fv_idx fv_val ->
              let coeff = matrix.(r).(fv_idx) in
              if not (Rat.is_zero coeff) then
                res := Rat.sub !res (Rat.mul coeff (Rat.of_int fv_val))
            ) free_vars current_free_vals;

            match Rat.to_int_opt !res with
            | Some n when n >= 0 -> calc_sum := !calc_sum + n
            | _ -> raise Exit (* invalid (negative or fractional) *)
          ) pivots;

          match !min_presses with
          | None -> min_presses := Some !calc_sum
          | Some m -> min_presses := Some (min m !calc_sum)
        with Exit -> ()
      ) else (
        for v = 0 to limit do
          search (f_idx + 1) (v :: current_free_vals) (current_sum + v)
        done
      )
  in
  search 0 [] 0;
  !min_presses



let () =
  data
  |> List.map (fun (_,(joltage,buttons)) ->
    match solve_machine buttons joltage with
        | Some presses -> presses
        | None -> 0
  )
  |> List.fold_left (+) 0
  |> dump_int 2
