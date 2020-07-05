let estMembre element liste =
  List.exists (fun elemList -> elemList = element) liste;;

let estInclus liste1 liste2 =
  List.fold_left (fun r e -> (estMembre e liste2) && r) true liste1;;

estInclus [1;2;3] [2;0;-3;1;0;3];;

let diffEns liste1 liste2 =
  List.fold_left (fun r e -> if not (estMembre e liste2) then e :: r else r) [] liste1;;

diffEns [2;0;-3;1;0;3] [1;2;3];;
diffEns [1;2;3] [1;2;3];;

let interEns liste1 liste2 =
  List.fold_left (fun r e -> if estMembre e liste2 then e :: r else r) [] liste1;;

interEns [1;2;3] [2;0;-3;1;0;3];;
interEns [1;2;3] [1;2;3];;

let union liste1 liste2 =
  interEns liste1 liste2 @ diffEns liste1 liste2 @ diffEns liste2 liste1;;

union [1;2;3] [1;2;4];;

let propListEmpty p =
  List.fold_left 
    (fun r (prop, list) -> if list = [] then (prop, list) :: r else r)
    []
    p;;

let prog = [('p', ['t';'m';'s']);('p', ['q';'r']);('q', []);('r', ['s';'t']);('s', []);('t', [])];;
let propTrue = propListEmpty prog;;

let rec elimListEmpty = function
  | [] -> []
  | (prop, []) :: remain ->
      prop :: elimListEmpty remain
  | _ -> failwith "Erreur liste non vide";;

elimListEmpty propTrue;;

let demontrable p b =
  let listPropTrueEmpty = propListEmpty p in
  let progRestant = diffEns listPropTrueEmpty p in
  let lTrue = elimListEmpty listPropTrueEmpty in
  let rec calcul listTrue prog switch =
    match switch with
    | 0 -> listTrue
    | _ -> 
        let listTrueBis = 
          List.fold_left 
            (fun r (prop, list) -> if estInclus list listTrue then union r [prop] else r) 
            listTrue  
            prog 
        in
        if listTrueBis <> listTrue then calcul listTrueBis prog 1
        else calcul listTrue progRestant 0
  in 
  let listTrueFinal = calcul lTrue p 1 in
  estInclus b listTrueFinal;; 

demontrable prog ['q';'r';'t'];;

let propositionsDemontrees p =
  List.fold_left 
    (fun r (prop, list) -> prop :: r) 
    [] 
    (List.filter (fun (prop, list) -> list = []) p);;

let propTrueEmpty = propositionsDemontrees prog;;

let reduitProg p ldd = 
  List.map 
    (fun (prop, list) -> 
       (prop, List.fold_left 
          (fun r e -> if estMembre e ldd then r else e :: r)
          []
          list)
    ) 
    p;;

reduitProg prog propTrueEmpty;;

let demontrableBis p b =
  let rec interne prog listPropTrue =
    let propTrue = propositionsDemontrees prog in
    let progItere = reduitProg prog propTrue in
    if prog <> progItere then
      interne progItere propTrue
    else 
      propTrue
  in estInclus b (interne p b);;

demontrableBis prog ['q';'r';'t'];;
demontrableBis prog ['a'; 'b'; 'c'];;
demontrableBis prog ['q'; 'r'; 'p'; 't'];;

(*
La premiere version est trop complique a comprendre
le seconde est plus simple
*)

let demontrableTri p b =
  let ldd = propositionsDemontrees p in
  let rec interne listPropTrue = function
    | [] -> []
    | (prop, liste) :: remain ->
        if estInclus liste listPropTrue then
          interne (prop :: listPropTrue) remain
        else
          interne listPropTrue remain
  in interne ldd p

