open String;;
open Char;;
open Printf;;

(*Etat 4, le puit*)
let reconnaitRec_4 string = false ;;

(*Etat 3*)
let rec reconnaitRec_3 string =
  match string with
  | "" -> true
  | _ -> let c = get string 0 in
         if c >= '0' && c <='9'
         then reconnaitRec_3 (sub string 1 ((length string)-1))
         else reconnaitRec_4 string
;;

(*Etat 2*)
let rec reconnaitRec_2 string =
  match string with
  | "" -> reconnaitRec_4 string
  | _ -> let c = get string 0 in
         if c >= '0' && c <='9'
         then reconnaitRec_2 (sub string 1 ((length string)-1))
         else
           if c='.'
           then reconnaitRec_3 (sub string 1 ((length string)-1))
           else reconnaitRec_4 string
;;

(*Etat 1*)
let reconnaitRec_1 string =
  match string with
  | "" -> reconnaitRec_4 string
  | _ -> let c = get string 0 in
         if c >= '0' && c <='9'
         then reconnaitRec_2 (sub string 1 ((length string)-1))
         else reconnaitRec_4 string
;;

(*Etat 0*)
let reconnaitRec_0 string =
  match string with
  | "" -> reconnaitRec_4 string
  | _ -> let c = get string 0 in
         if c >= '0' && c <='9'
         then reconnaitRec_2 (sub string 1 ((length string)-1))
         else
           if c = '-' || c = '+'
           then reconnaitRec_1 (sub string 1 ((length string)-1))
           else reconnaitRec_4 string
;;

(*Fonction principale*)
let reconnaitReelRec string = reconnaitRec_0 string;;

(*Affiche le resultat*)
let printResultat string =
  printf "\nPour la valeur %s\n" string;
  if reconnaitReelRec string
  then printf "La valeur est un float\n"
  else print_endline "La valeur n'est pas un float\n"
;;

(*Les tests demandés*)
let test = ["123.";"123.45";"-123.";"+123.34";"-123.34";
            "123.34";"12A3.34";"123..33";"123.34";".34"];;

List.map (fun x -> printResultat x) test;;

(*Scanner des inputs*)
let analyse =
  while(true) do
    print_string "Prochaine valeur :"; printResultat (input_line stdin);
  done;
;;