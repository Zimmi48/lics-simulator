let mauvaise_utilisation () =
  failwith "Utilisation incorrecte : vous devez fournir au simulateur le nom du
fichier � interpr�ter, les entr�es s�par�es par des espaces et le nombre
de cycles � effectuer"

let lengthArgv = Array.length Sys.argv

let () = if lengthArgv < 2 then mauvaise_utilisation ()

let s = Sys.argv.(1)

let input =
  let bool_of_int = function
    | 0 -> false
    | _ -> true
  in
  let rec list_of_args n acc =
    if n = 1 then acc
    else list_of_args (n - 1) (bool_of_int (int_of_string Sys.argv.(n)) :: acc)
  in
  list_of_args (lengthArgv - 2) []

let n = int_of_string Sys.argv.(lengthArgv - 1)

let () =
  let circuit =
    try LicsFileIO.read s
    with _ -> failwith ("Erreur � l'ouverture du fichier " ^ s)
  in
  Simulator.simulator circuit input n
