
(* types de description des actions à exécuter *)
type unaire = Not
type binaire = Or | And | Xor
type calcul = Unaire of int * unaire * int
	      | Binaire of int * binaire * int * int
type initialisation = Input of int | Inputreg of int
type definition = Output of int | Outputreg of int
type bloc = calcul list
type programme = initialisation list * bloc list * definition list

(*let eval prog =*)
