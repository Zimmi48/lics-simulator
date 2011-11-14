let mauvaise_utilisation () =
  failwith "Utilisation incorrecte : vous devez fournir au simulateur le nom du
fichier à interpréter, les entrées séparées par des espaces ainsi que le nombre
de cycles à effectuer"

let lengthArgv = Array.length Sys.argv

let () = if lengthArgv < 3 then mauvaise_utilisation ()

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
  let prog =
    try LicsFileIO.read s
    with _ -> failwith ("Erreur à l'ouverture du fichier " ^ s)
  in
  Simulator.simulator prog input n
