open Z3.Arithmetic
open Z3.Boolean
open Z3.Solver
open Z3.Model
open Z3.Arithmetic.Integer
let ctx = Z3.mk_context []

type expr = Z3.Expr.expr
type model = Z3.Model.model

let mkInt name = Z3.Arithmetic.Integer.mk_const_s ctx name
let intConst i = Z3.Arithmetic.Integer.mk_numeral_i ctx i
let mkBool name = Z3.Boolean.mk_const_s ctx name
let boolConst b = if b then Z3.Boolean.mk_true ctx else Z3.Boolean.mk_false ctx
let eval_int m e = 
  eval m e true |> Option.get |> numeral_to_string |> int_of_string
let eval_bool m e = 
  eval m e true |> Option.get |> Z3.Boolean.is_true
let to_string e = Z3.Expr.to_string e
let string_of_expr = to_string

(* only needed explicitly for imperative style *)
let solver () = Z3.Solver.mk_solver ctx None
let optimizer () = Z3.Optimize.mk_opt ctx

(* functional style *)
let eval_model formels f = 
  let s = solver () in
  Z3.Solver.add s formels;
  let result = check s [] in
  match result with
  | SATISFIABLE ->
    let m = get_model s in
    (match m with
    | Some m -> f (Ok m)
    | _ -> f (Error "No model found")
    )
  | _ -> f (Error "No solution found")

let eval_opt formels obj f = 
  let o = optimizer () in
  Z3.Optimize.add o formels;
  let _h = Z3.Optimize.minimize o obj in
  let result = Z3.Optimize.check o in
  match result with
  | SATISFIABLE ->
    let m = Z3.Optimize.get_model o in
    (match m with
    | Some m -> f (Ok m)
    | _ -> f (Error "No model found")
    )
  | _ -> f (Error "No solution found")
  
let solve formels =
  eval_model formels (
    fun r ->
      match r with
      | Ok m -> m
      | Error s -> failwith s
  )

(* notations for convenience *)
let varInt name = mkInt name
let varBool name = mkBool name
let intVar = varInt
let boolVar = varBool
(* all constraints given in the list have to be fullfilled *)
let all xs = mk_and ctx xs
(* at least one constraint in the list has to be fullfilled *)
let any xs = mk_or ctx xs
let ite a b c = mk_ite ctx a b c
let ifthenelse a b c = mk_ite ctx a b c
(* module Mystery = struct *)
  (* nested un quote via Stdlib.(...) *)
  let (<=) a b = mk_le ctx a b
  let (>=) a b = mk_ge ctx a b
  let (<) a b = mk_lt ctx a b
  let (>) a b = mk_gt ctx a b
  let (=) a b = mk_eq ctx a b
  let (<>) a b = mk_not ctx (mk_eq ctx a b)
  (* to make sure nobody write wrong code on accident *)
  let (==) a b = mk_eq ctx a b
  let (!=) a b = mk_not ctx (mk_eq ctx a b)
  let (&&) a b = mk_and ctx [a; b]
  let (||) a b = mk_or ctx [a; b]
  let (+) a b = mk_add ctx [a; b]
  let (-) a b = mk_sub ctx [a; b]
  let (~-) a = mk_unary_minus ctx a
  let ( * ) a b = mk_mul ctx [a; b]
  let ( / ) a b = mk_div ctx a b
  let (mod) a b = mk_mod ctx a b
  let (!) a = mk_not ctx a
  let ite a b c = mk_ite ctx a b c
  let int i = intConst i
  let bool b = boolConst b
  (* unexpected precedence (stronger than && ) *)
  let (=>) a b = mk_implies ctx a b
(* end *)

type literal = Pos of string | Neg of string
let string_of_literal = function
  | Pos s -> s
  | Neg s -> "!" ^ s

let cnf_clauses formula =
  let t = Z3.Tactic.and_then ctx 
    (Z3.Tactic.mk_tactic ctx "tseitin-cnf")
    (Z3.Tactic.mk_tactic ctx "simplify")
    []
  in
  let goal = Z3.Goal.mk_goal ctx false false false in
  Z3.Goal.add goal [formula];
  let cnf = Z3.Tactic.apply t goal None in
  if Stdlib.(Z3.Tactic.ApplyResult.get_num_subgoals cnf != 1) then failwith "CNF conversion failed";
  let subgoal = Z3.Tactic.ApplyResult.get_subgoal cnf 0 in
  let var_name e = 
    if Z3.Expr.is_const e then Z3.Expr.to_string e
    else failwith "Unexpected expression in CNF (expected variable)"
  in
  let get_literal e = 
    if Z3.Boolean.is_not e then
      Neg (var_name (List.hd (Z3.Expr.get_args e)))
    else
      Pos (var_name e)
  in
  List.map (fun clause ->
    if Z3.Boolean.is_or clause then
      Z3.Expr.get_args clause |> List.map get_literal
    else
      [get_literal clause]
  ) (Z3.Goal.get_formulas subgoal)

let write_dimacs filename clauses =
  let variables = List.flatten clauses |> List.map (fun l -> match l with | Pos s | Neg s -> s) |> List.sort_uniq Stdlib.compare in

  let var_mapping_file = filename ^ ".var_mapping" in
  let var_mapping = List.mapi (fun i v -> Printf.sprintf "%s %d" v Stdlib.(i+1)) variables in
  let var_mapping_content = String.concat "\n" var_mapping in

  let oc = open_out var_mapping_file in
  Printf.fprintf oc "%s\n" var_mapping_content;
  close_out oc;

  let dimacs_file = filename ^ ".dimacs" in
  let oc = open_out dimacs_file in
  Printf.fprintf oc "p cnf %d %d\n" (List.length variables) (List.length clauses);
  let var_map = List.mapi (fun i v -> Stdlib.(v, i+1)) variables |> List.to_seq |> Hashtbl.of_seq in
  let get_var_num v = Hashtbl.find var_map v in
  List.iter (fun clause ->
    let clause_str = List.map Stdlib.(fun l -> match l with | Pos s -> get_var_num s | Neg s -> - (get_var_num s)) clause |> List.map string_of_int |> String.concat " " in
    Printf.fprintf oc "%s 0\n" clause_str
  ) clauses;
  close_out oc
