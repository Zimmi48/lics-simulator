open LicsAst

exception Inputs_missing
exception Lw_rom

let int_of_bool = function true -> 1 | false -> 0

let simulator
    { programme = prog ; numero_var_max = num_var_max ; nb_reg = nb_reg }
    input n print_outputs decimal rom horloge =

  let time_0 = Unix.time () in
  let last_time = ref time_0 in
  (* on pourrait s'en servir aussi pour l'initialisation *)

  (* env contient les variables déjà définies *)
  let env = Array.make (num_var_max + 1) false in
  let add i x = env.(i) <- x in
  let find x = env.(x) in

  let eval_expr = function
    | Const c -> c
    | Unaire (Not,i) -> not (find i)
    | Binaire (Or, i, j) -> (find i) or (find j)
    | Binaire (And, i, j) -> (find i) && (find j)
    | Binaire (Nand, i, j) -> not ((find i) && (find j))
    | Binaire (Xor, i, j) ->
      ((find i) & not (find j)) or ((not (find i)) & (find j))
    | Ternaire (Mux, i, j, k) -> if find i then find j else find k
  in

  let rec eval_stmt inputReg input =
    (* valeurs envoyées par les registres, entrées *)
    let hd = List.hd and tl = List.tl in
    function
      | [] -> [] , []
      | h::t -> match h with
	  | Assign (i,e) -> (* on rajoute une variable à l'environnement *)
            add i (eval_expr e);
	    eval_stmt inputReg input t
          | Lw (il, adresse) ->
            (* ajoute List.length il variables provenant de la rom *)
            let adresse = fst (List.fold_left (* calcule l'adresse *)
                                 (fun (acc,puiss2) bit ->
                                   acc + puiss2 * (int_of_bool (find bit)) ,
                                   puiss2 * 2)
                                 (0,1)
                                 adresse ) in
            (* on doit avoir List.length il <= List.length rom.(adresse) *)
            let _ = List.fold_left
              (fun rom_word i -> match rom_word with
                | [] -> raise Lw_rom
                | h :: t ->
                  add i h ;
                  t
              )
              rom.(adresse)
              il
            in
            eval_stmt inputReg input t
	  | Inputreg i -> (* ajoute une variable provenant des registres *)
            add i (hd inputReg);
	    eval_stmt (tl inputReg) input t
	  | Input i -> (* idem mais provient des entrées *)
            begin
              try
                add i (hd input);
	        eval_stmt inputReg (tl input) t
              with Failure s when s = "tl" or s = "hd" -> raise Inputs_missing
            end
	  | Outputreg i ->
	    (* change la valeur des registres pour le prochain cycle *)
	    let outputReg, output = eval_stmt inputReg input t in
	    ((find i)::outputReg) , output
	  | Output i ->
	    (* renvoie les sorties *)
	    let outputReg, output = eval_stmt inputReg input t in
	    outputReg , ((find i)::output)
          | _ -> failwith "Not implemented"
  in

  let rec simulate input regs n =
    let espace = 8 in
    if decimal then (
      Graphics.open_graph "" ;
      Graphics.set_text_size ( 50 * espace ) ;
    );
    (* effectue un cycle *)
    (* l'environnement n'est pas vidé car on suppose que toutes les variables
       seront réaffectées avant d'être utilisees lors du cycle suivant *)
    let new_regs, output = eval_stmt regs input prog in
    if print_outputs then (
      (* affiche les sorties *)
      List.iter (function b -> print_int (int_of_bool b)) output ;
      print_newline () ;
    );
    if decimal then (
      (* convertit en décimal les sorties (chaque chiffre sur 4 bits)
         les affiche en mode graphique *)
      let affiche_chiffre b0 b1 b2 b3 =
        let chiffre = b0 + 2 * b1 + 4 * b2 + 8 * b3 in
        Graphics.draw_string (string_of_int chiffre)
      in
      let rec affiche_chiffres = function
        | b0 :: b1 :: b2 :: b3 :: t ->
          Graphics.rmoveto espace 0 ;
          affiche_chiffre
            (int_of_bool b0) (int_of_bool b1)
            (int_of_bool b2) (int_of_bool b3);
          affiche_chiffres t
        | _ -> ()
      in
      Graphics.clear_graph ();
      affiche_chiffres output;
    );
    (* lance les cycles suivants avec les nouvelles valeurs des registres *)
    if n > 1 then begin
      if horloge then
        if Unix.time() -. !last_time >= 1. then begin
          last_time := Unix.time ();
          simulate [true] new_regs (n - 1)
        end
        else
          simulate [false] new_regs (n - 1)
      else
        simulate input new_regs (n-1);
    end
  in

  let rec cree_liste = function
    | 0 -> []
    | n -> false::(cree_liste (n-1))
  in (* liste des valeurs initiales des registres *)

  (* lance la simulation *)
  if horloge then
    simulate [false] (cree_liste nb_reg) n
  else
    simulate input (cree_liste nb_reg) n;

  if decimal then
    let _ = Graphics.read_key () in
    Graphics.close_graph()
