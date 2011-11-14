open LicsAst

module Tint = struct
  type t = int
  let compare = Pervasives.compare
end

module IMap = Map.Make(Tint) (* pour les environnements de variables *)

let eval_expr env = (* env contient les variables déjà définies *)
  let find x = IMap.find x env in function
    | Const c -> c
(*    | Unaire (Input, i) ->
      | Unaire (Inputreg, i) -> *)
    | Unaire (Not,i) -> not (find i)
    | Binaire (Or, i, j) -> (find i) or (find j)
    | Binaire (And, i, j) -> (find i) && (find j)
    | Binaire (Nand, i, j) -> not ((find i) && (find j))
    | Binaire (Xor, i, j) ->
      ((find i) & not (find j)) or ((not (find i)) & (find j))
    | Ternaire (Mux, i, j, k) -> if find i then find j else find k

let rec eval_stmt env inputReg input =
(* environnements des variables, valeurs envoyées par les registres, entrées *)
  let hd = List.hd and tl = List.tl in
  function
    | [] -> [] , []
    | h::t -> match h with
	| Assign (i,e) -> (* on rajoute une variable à l'environnement *)
	  eval_stmt (IMap.add i (eval_expr env e) env) inputReg input t
	| Inputreg i -> (* idem mais provient des registres *)
	  eval_stmt (IMap.add i (hd inputReg) env) (tl inputReg) input t
	| Input i -> (* idem mais provient des entrées *)
	  eval_stmt (IMap.add i (hd input) env) inputReg (tl input) t
	| Outputreg i ->
	  (* change la valeur des registres pour le prochain cycle *)
	  let outputReg, output = eval_stmt env inputReg input t in
	  ((IMap.find i env)::outputReg) , output
	| Output i ->
	  (* renvoie les sorties *)
	  let outputReg, output = eval_stmt env inputReg input t in
	  outputReg , ((IMap.find i env)::output)

let rec simulate prog input regs n =
  (* effectue un cycle *)
  let new_regs, output = eval_stmt IMap.empty regs input prog in
  (* affiche les sorties *)
  List.iter (function true -> print_int 1 | false -> print_int 0) output ;
  print_newline () ;
  (* lance les cycles suivant avec les nouvelles valeurs des registres *)
  if n > 1 then simulate prog input new_regs (n-1)

let simulator prog input n =
  let rec compte_registre = List.fold_left (function n -> function
	| (Inputreg _) -> n + 1
	| _ -> n
    ) 0 in
  let k = compte_registre prog in
  let rec cree_liste = function
    | 0 -> []
    | n -> false::(cree_liste (n-1))
  in (* liste des valeurs initiales des registres *)
  simulate prog input (cree_liste k) n

(* Tests *)
(*
let full_adder = [
  Input 1 ;
  Input 2 ;
  Assign (3, Binaire (Xor, 1, 2)) ;
  Assign (4, Binaire (And, 1, 2)) ;
  Output 3 ;
  Output 4 ]

let full_adder_series = [
  Input 1 ;
  Input 2 ;
  Inputreg 5 ;
  Assign (3, Binaire (Xor, 1, 2)) ;
  Assign (6, Binaire (Xor, 3, 5)) ;
  Assign (4, Binaire (And, 1, 2)) ;
  Assign (7, Binaire (Or, 1, 2)) ;
  Assign (8, Binaire (And, 7, 5)) ;
  Assign (9, Binaire (Or, 8, 4)) ;
  Output 6 ;
  Outputreg 9]

let compteur_mod4 = [
  Inputreg 1 ;
  Inputreg 2 ;
  Assign (3, Const true) ;
  Assign (4, Binaire (Xor, 1, 3)) ;
  Assign (5, Binaire (And, 1, 3)) ;
  Assign (6, Binaire (Xor, 5, 2)) ;
  Output 6 ;
  Output 4 ;
  Outputreg 4 ;
  Outputreg 6 ]

let _ = simulator compteur_mod4 [] 10
*)
