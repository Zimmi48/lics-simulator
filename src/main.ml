
open Printf

(* fichiers d'entrée, options par défaut *)
let ifile = ref ""
let cycles = ref 0
let no_outputs = ref false
let rom = ref ""
let decimal = ref false
let inputs = ref []

let arguments s =
let bool_of_string = function
  | "0" -> false
  | _ -> true
in
  if !ifile = "" then ifile := s
  else inputs := bool_of_string s :: !inputs
  

(* liste des options du simulateur *)
let options = [
  "-cycles", Arg.Set_int cycles, "Nombre de cycles a effectuer";
  "-no-outputs", Arg.Set no_outputs, "N'affiche pas les sorties du circuit";
  "-load-rom", Arg.Set_string rom , "Nom du fichier contenant les valeurs initiales de la ROM";
  "-decimal", Arg.Set decimal, "Convertir les sorties en decimal et les afficher en mode graphique."
]

let () =
  let usage = Printf.sprintf "Usage : %s <file.bin> <entrees separees par des espaces>  [options]" Sys.argv.(0) 
  in
  Arg.parse options arguments usage

let lengthArgv = Array.length Sys.argv

let () =
  let circuit =
    try LicsFileIO.read !ifile
    with _ -> (
      eprintf "Erreur a l'ouverture du fichier %s\n" !ifile ;
      exit 1
    )
  in
  try
    Simulator.simulator
      circuit (List.rev !inputs) !cycles (not !no_outputs) !decimal
  with
    | Simulator.Inputs_missing -> (
      eprintf "Inputs missing\n";
      exit 1
    )
    | Simulator.Lw_rom -> (
      eprintf "Utilisation incorrecte de Lw(\"rom\", _) : vous demandez un mot trop grand.";
      exit 1
    )
(*
    | _ -> (
      eprintf "Erreur probablement due a un fichier binaire illisible.\n
Utilisez ce simulateur avec une version de lics-compiler compatible.";
      exit 1
    )*)
      
