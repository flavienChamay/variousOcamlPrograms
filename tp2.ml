type elem = Obstacle | Objet | Rien
type plateau = (int * int) -> elem
type etat_pince = Vide | Pleine
type prop = 
    CaseContenu of elem 
  | PinceVide
  | And of prop * prop
  | NotProp of prop;;

let contient elem = CaseContenu(elem);;

let pince_est_vide = PinceVide;;

let andp prop1 prop2 = And(prop1, prop2);;

let notp prop = NotProp(prop);;

let some_prop = andp (contient Objet) pince_est_vide;;

let it_prop fCaseContenu fPinceVide fAnd fNotProp prop=
  let rec compute = function
    | CaseContenu(elem) -> fCaseContenu elem
    | PinceVide -> fPinceVide
    | And(prop1, prop2) -> fAnd prop1 prop2 (compute prop1) (compute prop2)
    | NotProp(prop1) -> fNotProp prop1 (compute prop1)
  in compute prop;; 
  
type etat = 
    Etat of plateau * (int * int) * etat_pince * (int * int);;

let mk_etat plateau position etatPince vectDir = 
  Etat(plateau, position, etatPince, vectDir);;

let get_plateau = function Etat(plateau, _, _, _) -> plateau;;

let get_position = function Etat(_, position, _, _) -> position;;

let get_pince = function Etat(_, _, etatPince, _) -> etatPince;;

let get_direction = function Etat(_, _, _, vectDir) -> vectDir;;

let eval etat prop = 
  let fCaseContenu elem = ((get_plateau etat) (get_position etat)) = elem in
  let fPinceVide = (get_pince etat) = Vide  in
  let fAnd prop1 prop2 res1 res2 = res1 && res2  in
  let fNotProp prop1 res1 = not res1 in
  it_prop fCaseContenu fPinceVide fAnd fNotProp prop;;

let prendreCase = function
  | Objet -> Rien
  | _ -> failwith "Erreur de prendreCase";;


let prendreA plateau position = 
  if plateau position = Objet then
    fun pos -> if pos = position then Rien else plateau pos
  else 
    failwith "Erreur case vide ou obstacle";;

let etatPrendre = function
    Etat(plateau, position, etatPince, vectDir) -> 
      if etatPince = Pleine then
        failwith "Erreur pince pleine"
      else
        Etat(prendreA plateau position, position, Pleine, vectDir);;

let poserCase = function
  | Rien -> Objet
  | _ -> failwith "Erreur case occupee";; 

let poserA plateau position =
  if plateau position = Rien then
    fun pos -> if pos = position then Objet else plateau pos
  else 
    failwith "Erreur case pleine ou obstacle";;

let etatPoser = function
    Etat(plateau, position, etatPince, vectDir) -> 
      if etatPince = Vide then
        failwith "Erreur pince vide"
      else
        Etat(poserA plateau position, position, Vide, vectDir);; 
  
let etatTourner = function
    Etat(plateau, position, etatPince, vectDir) ->
      Etat(plateau, position, etatPince, 
           if vectDir = (1, 0) then 
             (0, 1)
           else if vectDir = (0, 1) then
             (-1, 0)
           else if vectDir = (-1, 0) then
             (0, -1)
           else
             (1, 0));;
  
let etatAvancer = function
    Etat(plateau, (xPos, yPos), etatPince, (xVect, yVect)) ->
      Etat(
        (if plateau (xPos, yPos) =  Obstacle then 
           failwith "Erreur obstacle" 
         else plateau) , 
        (xPos + xVect, yPos + yVect), etatPince, (xVect, yVect));;

type commande =
  | Prendre
  | Poser
  | Tourner
  | Avancer
  | Executer of commande * prop
  | ListeCommandes of commande list;;

let prendre = Prendre;;

let poser = Poser;;

let tourner = Tourner;;

let avancer = Avancer;;

let if_prop comm pro = Executer(comm, pro);;

let seq commandeListe = ListeCommandes(commandeListe);;

let rec executer etat  = function
  | Prendre -> etatPrendre etat
  | Poser -> etatPoser etat
  | Tourner -> etatTourner etat
  | Avancer -> etatAvancer etat
  | Executer(commande, prop) -> 
      if (eval etat prop) then
        executer etat commande
      else 
        etat
  | ListeCommandes([]) ->
      etat
  | ListeCommandes(head :: remain) -> 
      executer (executer etat head) (ListeCommandes remain) ;;

let rec executer2 etat  = function
  | Prendre -> etatPrendre etat
  | Poser -> etatPoser etat
  | Tourner -> etatTourner etat
  | Avancer -> etatAvancer etat
  | Executer(commande, prop) -> 
      if (eval etat prop) then
        executer etat commande
      else 
        etat
  | ListeCommandes(liste) ->
      List.fold_left (fun _ element -> executer etat element) etat liste;; 



