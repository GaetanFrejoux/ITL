open String;;
open Char;;
open Printf;;

(*Etat 4, le puit*)
let reconnaitRec_4 string = false;;

(*Etat 3*)
let rec reconnaitRec_3 string = match string with
  | "" -> true
  | _ -> reconnaitRec_4 string
;;

(*Etat 2*)
let rec reconnaitRec_2 string = match string with
  | "" -> reconnaitRec_4 string
  | _ -> let c = get string 0 in
         let sub = (sub string 1 ((length string)-1)) in
         if c = 'c' then reconnaitRec_2 sub else
           if c = 'a' then reconnaitRec_3 sub else reconnaitRec_4 sub
;;

(*Etat 1*)
let rec reconnaitRec_1 string = match string with
  | "" -> false
  | _ -> let c = get string 0 and
             sub = (sub string 1 ((length string)-1)) and
             epsilon = reconnaitRec_2 string in
         if c = 'b' then reconnaitRec_1 sub else
           if c = 'a' then reconnaitRec_3 sub else reconnaitRec_4 sub || epsilon
;;

(*Etat 0*)
let reconnaitRec_0 string = match string with
  | "" -> false
  | _ -> let c = get string 0 and
             sub = (sub string 1 ((length string)-1)) in
         if c = 'a'
         then reconnaitRec_1 sub || reconnaitRec_2 sub
         else reconnaitRec_4 sub
;;

(*Fonction principale*)
let reconnaitRecL4 string = reconnaitRec_0 string;;

(*Affiche le resultat*)
let printResultat string =
  printf "Pour la valeur %s\n" string;
  if reconnaitRecL4 string
  then print_endline "La valeur est correct\n\nProchaine valeur :"
  else print_endline "La valeur n'est pas correct\n\nProchaine valeur :"
;;


(*Les tests demandés*)
let test = ["abbcca";"accca";"abbccccba"];;

List.map (fun x -> printResultat x) test;;

(*Scanner des inputs*)
let analyse = while(true) do printResultat (input_line stdin) done;;
