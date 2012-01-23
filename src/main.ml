
open Printf

(* fichiers d'entrée, options par défaut *)
let ifile = ref ""
let cycles = ref 0
let no_outputs = ref false
let load_rom = ref ""
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
  "-load-rom", Arg.Set_string load_rom , "Nom du fichier contenant les valeurs initiales de la ROM";
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
  let rom = Array.make 65536 [] in
  (* adresses codées sur 16 bits (au plus) *)
  if !load_rom <> "" then begin
    try
      let f = open_in !load_rom in
      let rom_in : bool list array = input_value f in
      close_in f;
      Array.blit rom_in 0 rom 0 (Array.length rom_in)
    with
      | Invalid_argument "Array.blit" -> (
        eprintf "Rom chargee trop grosse\n" ;
        exit 1
      )
      | _ -> (
        eprintf "Erreur a l'ouverture du fichier %s\n" !load_rom ;
        exit 1
      )
  end;
  try
    Simulator.simulator
      circuit (List.rev !inputs) !cycles (not !no_outputs) !decimal rom
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
      
