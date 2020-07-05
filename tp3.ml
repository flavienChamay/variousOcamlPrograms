let estZero_v1 = function
  | 0 -> "zero" ;; 

let estZero_v2 = function
  |  0 -> "zero" 
  | _ -> "nonZero";; 

let voyelle = function
  | 'a'| 'e' | 'i' | 'o' | 'u'| 'y' -> true
  | _ -> false;;

let rang = function
  | "lundi" -> 1
  | "mardi" -> 2
  | "mercredi" -> 3
  | "jeudi" -> 4
  | "vendredi" -> 5
  | "samedi" -> 6
  | "dimanche" -> 7
  | _ -> 0;;

let inf jour1 jour2 =
  let rangJour1 = rang jour1 and rangJour2 = rang jour2 in
  let rangDiff = rangJour2 - rangJour1 in
  (rangDiff = 1 ) || (rangDiff = -6);; 

let jsem = function
  | 1 -> "lundi"
  | 2 -> "mardi"
  | 3 -> "mercredi"
  | 4 -> "jeudi"
  | 5 -> "vendredi"
  | 6 -> "samedi"
  | 7 -> "dimanche"
  | _ -> "jour inconnu";;

let jourSucc1 = function
  | "lundi" -> "mardi"
  | "mardi" -> "mercredi"
  | "mercredi" -> "jeudi"
  | "jeudi" -> "vendredi"
  | "vendredi" -> "samedi"
  | "samedi" -> "dimanche"
  | "dimanche" -> "lundi"
  | _ -> "jour inconnu";; 

let jourSucc2 j =
  let rangJour = rang j in
  if (rangJour <> 0) then
    let rangSucc = rangJour + 1 in 
    if (rangSucc = 8) then 
      "lundi" 
    else jsem rangSucc 
  else "jour inconnu";;

let jourSucc3 j = 
  let rangJour = rang j in
  if (rangJour <> 0) then
    let rangSucc = rangJour + 1 in
    if (rangSucc mod 8 = 0) then 
      "lundi" 
    else jsem rangSucc 
  else "jour inconnu";;

let jourPred1 = function
  | "lundi" -> "dimanche"
  | "mardi" -> "lundi"
  | "mercredi" -> "mardi"
  | "jeudi" -> "mercredi"
  | "vendredi" -> "jeudi"
  | "samedi" -> "vendredi"
  | "dimanche" -> "samedi"
  | _ -> "jour inconnu";;
  
let jourPred2 j =
  let rangJour = rang j in
  if (rangJour <> 0) then 
    let rangPred = rangJour - 1 in
    if (rangPred = 0) then 
      "dimanche" 
    else jsem rangPred 
  else "jour inconnu";;

let jourPred3 j =
  let rangJour = rang j in 
  if (rangJour <> 0) then
    let rangPred = rangJour - 1 in
    if (rangPred mod 7 = 0) then 
      "dimanche" 
    else jsem rangPred 
  else "jour inconnu";;


let bissextile annee = 
  (annee mod 400 = 0) || (annee mod 4 = 0 && annee mod 100 <> 0) ;;

let nbjour mois annee = 
  if (bissextile annee && mois = 2) then
    29
  else match mois with
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
    | _ -> failwith "mauvais mois";;
