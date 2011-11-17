open LicsAst

module Tint = struct
  type t = int
  let compare = Pervasives.compare
end

module IMap = Map.Make(Tint) (* pour les environnements de variables *)

let eval_expr env = (* env contient les variables déjà définies *)
  let find x = env.(x) in function
    | Const c -> c
    | Unaire (Not,i) -> not (find i)
    | Binaire (Or, i, j) -> (find i) or (find j)
    | Binaire (And, i, j) -> (find i) && (find j)
    | Binaire (Nand, i, j) -> not ((find i) && (find j))
    | Binaire (Xor, i, j) ->
      ((find i) & not (find j)) or ((not (find i)) & (find j))
    | Ternaire (Mux, i, j, k) -> if find i then find j else find k

let rec eval_stmt env inputReg input =
  let add i x = env.(i) <- x ; env in
  let find x = env.(x) in
(* environnements des variables, valeurs envoyées par les registres, entrées *)
  let hd = List.hd and tl = List.tl in
  function
    | [] -> [] , []
    | h::t -> match h with
	| Assign (i,e) -> (* on rajoute une variable à l'environnement *)
	  eval_stmt (add i (eval_expr env e)) inputReg input t
	| Inputreg i -> (* idem mais provient des registres *)
	  eval_stmt (add i (hd inputReg)) (tl inputReg) input t
	| Input i -> (* idem mais provient des entrées *)
	  eval_stmt (add i (hd input)) inputReg (tl input) t
	| Outputreg i ->
	  (* change la valeur des registres pour le prochain cycle *)
	  let outputReg, output = eval_stmt env inputReg input t in
	  ((find i)::outputReg) , output
	| Output i ->
	  (* renvoie les sorties *)
	  let outputReg, output = eval_stmt env inputReg input t in
	  outputReg , ((find i)::output)

let rec simulate prog input regs n env =
  (* effectue un cycle *)
  (* l'environnement n'est pas vidé car on suppose que toutes les variables *)
  (* seront réaffectées avant d'être utiliser lors du cycle                 *)
  let new_regs, output = eval_stmt env regs input prog in
  (* affiche les sorties *)
  List.iter (function true -> print_int 1 | false -> print_int 0) output ;
  print_newline () ;
  (* lance les cycles suivant avec les nouvelles valeurs des registres *)
  if n > 1 then simulate prog input new_regs (n-1) env

let simulator circuit input n =
  let rec cree_liste = function
    | 0 -> []
    | n -> false::(cree_liste (n-1))
  in (* liste des valeurs initiales des registres *)
  let env = Array.make circuit.numero_var_max false in
  simulate circuit.programme input (cree_liste circuit.nb_reg) n env
