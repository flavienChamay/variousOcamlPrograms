(*
(1,false) ;;
-: int * bool = (1,false)
(2., 1+4, 1=1, "salut") ;;
-: float * int * bool * string = (2., 1+4, 1=1, "salut")
(('a', 1), ('b', 2), ('c', 3)) ;;
-: (char * int) * (char * int) * (char * int) = (('a', 1), ('b', 2), ('c', 3))
(1, 2, 3) ;;
-: int * int * int = (1, 2, 3)
(1, (2, 3)) ;;
-: int * (int *int) = (1, (2, 3))
((1, 2), 3) ;;
-: (int * int) * int
(1, 2, 3) = (2, 1, 3) ;;
-: bool = false
(1, (2, 3)) = (1, 2, 3) ;;
This expression has type 'a * 'b * 'c
       but an expression was expected of type int * (int * int)
*)

let quadruplet = 
  (1 = 1, (23, 1), (fun x -> x +. 2.3), "bla");;

let imp a b =
  match (a,b) with
  | (true, false) -> false
  | _ ->true ;;

let xor a b =
  match (a,b) with
  | (true, true) | (false, false) -> false
  | _ -> true;;

let bissextile annee =
  annee mod 4 = 0 && annee mod 100 <> 0 || annee mod 400 = 0;;

let nbjour (moisInt, anneeInt) =
  if(bissextile anneeInt && moisInt = 2) then 
    29
  else match moisInt with 
    | 1 -> 31
    | 2 -> 28
    | 3 -> 31
    | 4 -> 30
    | 5 -> 31
    | 6 -> 30
    | 7 -> 31
    | 8 -> 31
    | 9 -> 30
    | 10 -> 31
    | 11 -> 30
    | 12 -> 31
    | _ -> 0;; 

let valide date =
  let (jour, mois, annee) = date  in
  let bissex = bissextile annee in
  annee >= 0 && mois >= 1 && mois <= 12 && jour >= 1 && jour <= 31
  && (imp bissex (jour <= 29)  && imp (not bissex && mois=2) (jour <= 28));;

let lendemain date =
  let (jour, mois, annee) = date in
  if (valide date = false) then
    failwith "argument pas une date"
  else
    let bissex = bissextile annee in
    let jourMax = nbjour (mois, annee) in
    if ((bissex && mois = 2 && jour = 29) || (not bissex && mois = 2 && jour = 28)) then
      (1,3,annee)
    else if (mois = 12 && jour = jourMax) then
      (1, 1, annee + 1)
    else if (jour = jourMax) then 
      (1, mois + 1, annee)
    else
      (jour + 1, mois, annee);;

let veille date =
  let (jour, mois, annee) = date in
  if (valide date = false) then
    failwith "argument pas une date"
  else
    let bissex = bissextile annee in
    if (bissex && mois = 3 && jour = 1) then
      (29, 2, annee)
    else if (not bissex && mois = 3 && jour = 1) then
      (28, 2, annee)
    else if (mois = 1 && jour = 1) then
      (31, 12, annee - 1)
    else if (jour = 1) then 
      (nbjour (mois - 1, annee), mois - 1, annee)
    else
      (jour - 1, mois, annee);;

let sec2hms ts =
  let min = ts / 60 in
  let h = ts / (60*60) in
  let min = min - (h*60) in
  let s = ts -(h*60*60+min*60) in 
  (h, min, s);;

let ex1 = sec2hms 3661;;

let sommeTemps hms1 hms2 =
  let (h1, m1, s1) = hms1 in
  let (h2, m2, s2) = hms2 in
  let totSec = (h1+h2)*60*60 + (m1+m2)*60 + s1 + s2 in
  sec2hms totSec;;

let ex2 = sommeTemps (1,31,7) (2,29,54);;
    
