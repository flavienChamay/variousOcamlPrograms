(*Partie A*)

let rec inserer entier liste =
  match liste with
  | [] -> [entier]
  | tete :: reste -> 
      if entier < tete then 
        entier :: liste
      else 
        tete :: inserer entier reste;;

let rec triInsertion = function
  | [] -> []
  | tete :: reste -> inserer tete (triInsertion reste);;
(*
  bonne methode pour partage:
    let partage = function
      | [] -> ([], [])
      | x :: tail -> let l1, l2 = partage tail in 
          (x :: l2, l1);;
*)
let rec partage = function
  | [] -> ([], [])
  | [entier]  -> ([entier], [])          
  | tete1 :: tete2 :: reste -> let (l1, l2) = partage reste in (tete1 :: l1, tete2 :: l2);;

(**
On peut faire avec triInsertion le merge: on fusionne les liste puis TriInsertion
   *)
let rec merge liste1 liste2 =
  match liste1 with
  | [] -> liste2
  | tete :: reste -> merge reste (inserer tete liste2);;

let rec triFusion liste = 
  match liste with
  | [] -> []
  | [x] -> [x]
  | _ -> let (l1, l2) = partage liste in merge (triFusion l1) (triFusion l2);;
  
let rec filter predicat liste =
  match liste with
  | [] -> []
  | tete :: reste -> 
      if predicat tete then 
        tete :: filter predicat reste
      else
        filter predicat reste ;;

let rec forall predicat liste =
  match liste with
  | [] -> true
  | tete :: reste ->
      (predicat tete) && forall predicat reste;;

let rec exists predicat liste =
  match liste with
  | [] -> false
  | tete :: reste -> 
      predicat tete || exists predicat reste;;

let fst (one, two) =
  one;;

let snd (one, two) =
  two;;

let rec parcoursEstF liste element =
  match liste with
  | [] -> true
  | tete :: reste ->
      (tete <> element) && parcoursEstF reste element;;

let rec firstElem = function 
  | [] -> []
  | tete :: reste ->
      fst tete :: firstElem reste;;

let estFonction liste =
  let firstElemListe = firstElem liste in
  let rec calcul = function
    | [] -> true
    | tete :: reste -> (parcoursEstF reste tete) && calcul reste
  in calcul firstElemListe;;
  
let rec image element liste =
  match liste with
  | [] -> failwith "Erreur de domaine"
  | tete :: reste -> 
      if element = (fst tete) then
        snd tete
      else 
        image element reste;;

let rec imageEns l listeCouple =
  match l with
  | [] -> []
  | tete :: reste ->
      (image tete listeCouple) :: imageEns reste listeCouple;;

let rec parcoursEstI liste element =
  match liste with
  | [] -> true
  | tete :: reste ->
      ((snd tete) <> element) && parcoursEstI reste element;;

let rec estInjective = function
  | [] -> true
  | tete :: reste ->
      parcoursEstI reste (snd tete);; 

let rec surcharge l1 l2 = match l1 with
    [] -> l2
  | e::_l1 -> let (x1,_) = e in surcharge _l1
        (if forall (fun (x2,_) -> x2 <> x1) l2 then e::l2 else l2);;

let rec isDef x f =
  match f with
  | [] -> false
  | tete :: reste -> (fst tete) = x || isDef x reste;;

let rec composition l1 = function
    [] -> []
  | (x,y)::l2 -> if (imageEns [y] (l1@[(y,x)])) <> [x]
      then (x, image y l1)::(composition l1 l2)
      else composition l1 l2;;

let rec auxiliaire element liste =
  match liste with
  | [] -> []
  | tete :: reste -> 
      (element, tete) :: auxiliaire element reste;;

let rec fprod l1 l2 f =
  let rec _fprod _l1 _l2 = match (_l1,_l2) with
      (e1::__l1, e2::__l2) -> f e1 e2::_fprod _l1 __l2
    | ((_::__l1),[]) -> _fprod __l1 l2
    | ([],_) -> []
  in _fprod l1 l2;;
    
let produit l1 l2 = 
  fprod l1 l2 (fun (x1,y1) (x2,y2) -> ((x1,x2),(y1,y2)));;
    
(*Partie B*)

let rec hanoi n (src, tmp, dst) = 
  if n <= 0 then 
    [] 
  else
    hanoi (n - 1) (src, dst, tmp) @ (src, dst)::hanoi (n - 1) (tmp, src, dst);;
                                      
(*Partie C*)
let rec last = function
  | der :: [] -> der
  | tete :: reste -> last reste
  | _ -> failwith "Erreur liste vide";;

let rec sum = function
  | [] -> 0
  | tete :: reste ->
      tete + sum reste;;

let rec append liste1 liste2 =
  match liste1 with
  | [] -> liste2nn
  | tete :: reste ->
      tete :: (append reste liste2);;

let rec reverse = function
  | [] -> []
  | tete :: reste ->
      (reverse reste) @ [tete];;

let rec nbOcc element liste =
  match liste with
  | [] -> 0
  | tete :: reste -> 
      if tete = element then
        1 + nbOcc element reste
      else
        nbOcc element reste;;

let rec elimFirst element liste =
  match liste with
  | [] -> []
  | tete :: reste ->
      if tete = element then
        reste
      else
        tete :: elimFirst element reste;; 
(*
   Demander si correct
   *)
let elimLast1 element liste =
  let occurences = nbOcc element liste in
  let rec calcul element liste compteur =
    match liste with
    | [] -> []
    | tete :: reste -> 
        if tete = element then
          if compteur = occurences then
            calcul element reste compteur
          else tete :: calcul element reste (compteur+1)
        else 
          tete :: calcul element reste compteur
  in calcul element liste 1;; 

let elimLast2 element liste =
  reverse (elimFirst element (reverse liste));;
  
let rec elim element liste =
  match liste with
  | [] -> []
  | tete :: reste ->
      if tete = element then
        elim element reste
      else
        tete :: elim element reste;;

let rec substitute x y liste =
  match liste with
  | [] -> []
  | tete :: reste -> 
      if tete = x then
        y :: substitute x y reste
      else 
        tete :: substitute x y reste;;

let rec substitute2 p element liste =
  match liste with
  | [] -> []
  | tete :: reste ->
      if p tete then
        element :: substitute2 p element reste
      else
        tete :: substitute2 p element reste;; 

(*Partie C.2*)

let l1 = 10 :: 20 :: 30 :: [];;
let l2 = [10;20;30];;

let head = function
  | [] -> failwith "ERREUR liste vide"
  | tete :: reste -> tete;;

let tail = function
  | [] -> failwith "ERREUR liste vide"
  | tete :: reste -> reste;;

let res1 = head (tail (tail [1;2;3;4]));;
let res2 = tail [(1,2);(3,4);(5,6)];;
(*peut pas acceder a 3e element de [], de [1;2]*)
let res3 = tail [[1]; [2;3;4];[]];;
let res4 = tail [[1,2];[3,4];[5,6]];;

(*Partie C.3*)
(*
1) []
-: 'a list = []

2) [1;2;true]
erreur type
3) [1;(2,true)]
erreur type
4) [1,2,3]
-: (int * int * int) list = [(1,2,3)]
5) [[1,2];[3,4]]
-: (int*int) list list = [[(1,2)];[(3,4)]]
6) [[1,2];[3,4,5]]
erreur type
7) [1;2;3]
-: int list = [1;2;3]
8) [(1,true,5.0);(2,false,6.4);(3,true,7.9)]
-: (int * bool * float) list = [(1,true,5.0);(2,false,6.4);(3,true,7.9)]
9) ([1;2;3],[[];[true,false]])
-: int list * (bool* bool) list list  = ([1;2;3],[[];[true,false]])
*)

(*Partie C.4*)
let f1 = function
  | un :: deux :: trois -> deux
  | _ -> failwith "Erreur domaine";;

let f2 = function
  | un :: deux :: trois :: reste -> deux
  | _ -> failwith "Erreur domaine";;

let f3 = function
  | ([un :: deux :: reste], liste2) -> deux
  | _ -> failwith "Erreur domaine";;

let f4 = function
  | [(unU, deuxU);(unD, deuxD)] -> deuxU
  | _ -> failwith "Erreur domaine";;

(*Partie C5*)
let rec consCpleDouble n =
  if n <= 0 then
    []
  else
    match n with
    | 1 -> [(1, 2)]
    | n -> (n, 2 * n) :: consCpleDouble (n-1);;

let rec consCpleF f n =
  if n <= 0 then 
    []
  else
    match n with
    | 1 -> [(1, f 1)]
    | n -> (n, f n) :: consCpleF f (n-1);;


