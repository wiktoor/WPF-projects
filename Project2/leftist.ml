(*autor: Wiktor Chmielewski grupa 5*)
(*review: Igor Kamiński grupa 2*)
(*przechowujemy kolejno: lewego syna, prawego syna, wartość (priorytet), npl (null path length)*)
(*npl to odległość danego wierzchołka od nulla*)
type 'a queue = Node of ('a queue)*('a queue)*('a)*int | Null;;

(*funkcje pomocnicze*)
let npl q = 
  match q with
  | Null -> -1
  | Node (Null,_,_,_) -> 0
  | Node (_,Null,_,_) -> 0
  | Node (_,_,_,a) -> a
;;

let empty = Null;;

let rec join queue1 queue2 = 
  match (queue1, queue2) with
  | (Null, Null) -> Null
  | (Null,_) -> queue2
  | (_,Null) -> queue1
  | (Node (left1,right1,value1,npl1), Node (left2,right2,value2,npl2)) ->
    if (value1<=value2) then 
      let d = join right1 queue2 in 
      if (npl left1)<=(npl d) then Node (d,left1,value1,(npl left1)+1)
      else Node (left1,d,value1,(npl d)+1)
    else join queue2 queue1
;;

let add e q = join (Node (Null,Null,e,0)) q;;

let is_empty q =
  match q with
  | Null -> true
  | _ -> false
;;

exception Empty;;
let delete_min q =
  match q with
  | Null -> raise (Empty)
  | Node (left,right,value,npl) -> (value,(join left right))
;;