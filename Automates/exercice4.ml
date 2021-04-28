open String;;
open Char;;
open Printf;;

(*Etat 4, le puit*)
let reconnaitRec_4 string = false ;;

(*Etat 3*)
let rec reconnaitRec_3 string valeur negatif diviseur =
  match string with
  | "" -> true,valeur
  | _ -> let c = get string 0 and subString = (sub string 1 ((length string)-1)) in
         if c >= '0' && c <='9'
         then let valueAdded = (float_of_int (code c - code '0'))/.diviseur in
              if negatif
              then reconnaitRec_3 subString (valeur-.valueAdded) negatif (diviseur*.10.)
              else reconnaitRec_3 subString (valeur+.valueAdded) negatif (diviseur*.10.)
         else reconnaitRec_4 string,valeur
;;

(*Etat 2*)
let rec reconnaitRec_2 string valeur negatif =
  match string with
  | "" -> false,valeur
  | _ -> let c = get string 0 and subString = (sub string 1 ((length string)-1)) in
         if c >= '0' && c <='9'
         then let toFloat = float_of_int (code c - code '0') and
                  subString = (sub string 1 ((length string)-1)) in
              if negatif
              then reconnaitRec_2 subString (valeur*.10.-.toFloat) negatif
              else reconnaitRec_2 subString (valeur*.10.+.toFloat) negatif
         else
           if c='.'
           then reconnaitRec_3 subString valeur negatif 10.
           else reconnaitRec_4 string,valeur
;;

(*Etat 1*)
let reconnaitRec_1 string valeur negatif =
  match string with
  | "" -> false,valeur
  | _ -> let c = get string 0 and subString = (sub string 1 ((length string)-1)) in
         if c >= '0' && c <='9'
         then let toFloat = float_of_int (code c - code '0') in
              if negatif
              then reconnaitRec_2 subString (valeur*.10.-.toFloat) negatif
              else reconnaitRec_2 subString (valeur*.10.+.toFloat) negatif
         else reconnaitRec_4 string,valeur
;;

(*Etat 0*)
let reconnaitRec_0 string valeur =
  match string with
  | "" -> false,valeur
  | _ -> let c = get string 0 and subString = (sub string 1 ((length string)-1)) in
         if c >= '0' && c <='9'
         then let toFloat = float_of_int (code c - code '0') in
              reconnaitRec_2 subString (valeur*.10.+.toFloat) false
         else if c = '-' then reconnaitRec_1 subString valeur true
         else if c = '+' then reconnaitRec_1 subString valeur false
         else reconnaitRec_4 string,valeur
;;
(*Fonction principale*)
let evalueReelRec string = reconnaitRec_0 string 0.0;;

(*Affiche le resultat*)
let printResultat string =
  let isFloat,ans = evalueReelRec string in
  printf "\nPour la valeur %s\n" string;
  if isFloat
  then (printf "La valeur est un float, sa valeur : %f\n\n" ans)
  else (print_endline "La valeur n'est pas un float\n")
;;

(*Les tests demandés*)
let test = ["3.14";"002.24";"-3.14"; "1000.14";"123.";"123.453";"-123.";
            "+123.34";"-123.34";"12A3.34";"123..33";"123.34";".34"];;

List.map (fun x -> printResultat x) test;;

(*Scanner des inputs*)
let analyse =
  while(true) do
    print_string "Prochaine valeur :"; printResultat (input_line stdin);
  done;
;;
