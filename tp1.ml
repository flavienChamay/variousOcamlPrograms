let hd = function 
  |  tete :: reste -> tete
  | _ -> failwith "Erreur liste vide";;

let rec tl = function
  | tete :: reste -> reste
  | _ -> failwith "Erreur liste vide";;

let rec tete = function
  | [] -> []
  | head :: remain -> hd head :: tete remain;;

let rec reste = function
  | [] -> []
  | head :: remain  -> tl head :: reste remain;;

let rec trans = function
  | [] | [] :: _ -> []
  | matrice -> tete matrice :: trans (reste matrice);;

let rec map fonc = function
  | [] -> []
  | head :: remain -> fonc head :: map fonc remain;; 
(*('a -> 'b) -> 'a list -> 'b list*)

let tete2 = map hd;;

let reste2 = map tl;;

let rec ligneZero = function
  | 0 -> [] 
  | n -> 0 :: ligneZero (n-1);;

let zero n = 
  map (fun _ -> ligneZero n) (ligneZero n);;

let rec compteur = function
  | 1 -> [1]
  | n -> compteur (n-1) @ [n];; 

let rec unite = function
  | 0 -> []
  | n -> (1 :: ligneZero (n-1)) :: map (fun x -> (0 :: x)) (unite (n-1));;

let rec map2 fonction liste1 liste2 = 
  match liste1, liste2 with
  | [], [] -> []
  | head1 :: remain1, head2 :: remain2 ->
      fonction head1 head2 :: map2 fonction remain1 remain2
  | _ -> failwith "Erreur: listes de tailles differentes";;
    
let somlig = map2 (+) ;;
      
let add = map2 somlig ;;

let rec prodligcol liste1 liste2 =
  match liste1, liste2 with
  | [], [] -> 0
  | head1 :: remain1, head2 :: remain2 ->
      head1 * head2 + prodligcol remain1 remain2
  | _ -> failwith "Erreur: listes de tailles differentes";;
  
let prodligtmat ligne =
  map (prodligcol ligne);;  

let rec prodmat matrice1 matrice2 =
  match matrice1 with
  | [] -> []
  | head :: remain ->
      prodligtmat head matrice2 :: prodmat remain matrice2;; 

(* ou
  let prodmat m1 m2 = map (fun l -> prodligmat l (trans m2)) m1;;
*)
let rec create f = function
  | 0 -> []
  | 1 -> [f 1]
  | n -> create f (n-1) @ [f n];;

let couples n = create (fun i -> create (fun j -> (i,j)) n) n;;

let zero2 n = 
  let ligne = create (fun _ -> 0) n in
  map (fun _ -> ligne) (ligne);;

let unite2 n = 
  create (fun i -> create (fun j -> if i = j then 1 else 0) n) n;;
