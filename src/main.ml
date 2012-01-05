
open Printf

(* fichiers d'entrée, options par défaut *)
let ifile = ref ""
let cycles = ref 0
let no_output = ref false
let rom = ref ""
let displays = ref 0
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
  "cycles", Arg.Set_int cycles, "Nombre de cycles à effectuer";
  "-no-outputs", Arg.Set no_output, "N'affiche pas les sorties du circuit";
  "-load-rom", Arg.Set_string rom , "Nom du fichier contenant les valeurs initiales de la ROM";
  "-displays", Arg.Set_int displays, "Nombre d'afficheurs 7 segments à utiliser"
]

let () =
  let usage = Printf.sprintf "Usage : %s [options] <file.bin> <entrees separees par des espaces>" Sys.argv.(0) 
  in
  Arg.parse options arguments usage

let lengthArgv = Array.length Sys.argv

let () =
  let circuit =
    try LicsFileIO.read !ifile
    with _ -> (
      eprintf "Erreur à l'ouverture du fichier %s\n" !ifile ;
      exit 1
    )
  in
  Simulator.simulator circuit (List.rev !inputs) !cycles
