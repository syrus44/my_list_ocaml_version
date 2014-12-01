(* OCaml Project 01 : my_list *)
(* author : Jordan Bettin <bettin_j@epitech.net> *)

(* ------------------------------------------------------------- *)

(* Declaration du type generique my_list *)

type 'a my_list = 
  | Item of ('a * 'a my_list)
  | Empty
;;

(* ------------------------------------------------------------- *)

(* Fonction length *)

let rec length my_list =
  match my_list with
    | Empty -> 0
    | Item (hd, tl) -> 1 + length (tl)
;;

(* ------------------------------------------------------------- *)

(* Fonction hd *)

let hd my_list =
  match my_list with
  | Item (hd, tl) -> hd
  | Empty -> raise (Failure "hd")
;;

(* ------------------------------------------------------------- *)

(* Fonction tl *)

let tl my_list = 
  match my_list with
  | Item (hd, tl) -> tl
  | Empty -> raise (Failure "tl")
;;

(* ------------------------------------------------------------- *)

(* Fonction nth *)

let rec nth my_list nb = 
  let l = length my_list in
  if nb < 0 then raise (Failure "nth")
  else if nb > l then raise (Failure "nth : too much elements")
  else
    match nb with
      | 0 -> (hd my_list)
      | _ -> nth (tl my_list) (nb - 1)
;;

(* ------------------------------------------------------------- *)

(* Fonction rev *)

let rev my_list =
  let rec rev2 acc = function
      | Empty -> acc
      | Item (hd, tl) -> rev2 (Item (hd, acc)) tl
  in rev2 Empty my_list
;;

(* ------------------------------------------------------------- *)

(* Fonction append *)

let append my_list my_list2 =
  let rec append2 acc = function
      | Empty -> acc
      | Item (hd, tl) -> append2 (Item (hd, acc)) tl
  in append2 my_list2 (rev my_list)
;;

(* ------------------------------------------------------------- *)

(* Fonction rev_append *)

let rec rev_append my_list my_list2 =
  match my_list with
    | Empty -> my_list2
    | Item (hd, tl) -> rev_append tl (Item (hd, my_list2))
;;

(* ------------------------------------------------------------- *)

(* Fonction flatten *)

let flatten my_list = 
  let rec flatten2 acc = function
    | Empty -> rev acc
    | Item (hd, tl) -> flatten2 (rev_append hd acc) tl
  in flatten2 Empty my_list
;;

(* ------------------------------------------------------------- *)

(* Fonction mem *)

let rec mem elem my_list = 
  match my_list with
    | Empty -> false
    | Item (hd, tl) -> if elem = hd then true else mem elem tl
;;

(* ------------------------------------------------------------- *)

(* Fonction memq *)

let rec memq elem my_list = 
  match my_list with
    | Empty -> false
    | Item (hd, tl) -> if elem == hd then true else memq elem tl
;;

(* ------------------------------------------------------------- *)

(* Fonction iter *)

let rec iter func my_list =
  match my_list with
    | Empty -> ()
    | Item (hd, tl) -> func hd; iter func tl
;;

(* ------------------------------------------------------------- *)

(* Fonction map *)

let rec map func = function
    | Empty -> Empty
    | Item (hd, tl) -> let aux = func hd in Item (aux, (map func tl))
;;

(* ------------------------------------------------------------- *)

(* Fonction fold_left *)

let rec fold_left func n my_list =
  match my_list with
    | Empty -> n
    | Item (hd, tl) -> (fold_left func (func n hd) tl)
;;

(* ------------------------------------------------------------- *)

(* Fonction for_all *)

let rec for_all func my_list =
  match my_list with
    | Empty -> true
    | Item (hd, tl) -> func hd && for_all func tl
;;

(* ------------------------------------------------------------- *)

(* Fonction exists *)

let rec exists func my_list =
  match my_list with
  | Empty -> false
  | Item (hd, tl) -> func hd || exists func tl
;;

(* ------------------------------------------------------------- *)

(* Fonction filter *)

let filter elem =
  let rec find acc = function
    | Empty -> rev acc
    | Item (hd, tl) -> if elem hd
      then find (Item (hd, acc)) tl
      else find acc tl
  in find Empty
;;

(* ------------------------------------------------------------- *)

(* Fonction assoc *)

let rec assoc elem = function
  | Empty -> raise Not_found
  | Item ((hd, tl), tl2) -> if hd = elem then tl 
    else assoc elem tl2
;;

(* ------------------------------------------------------------- *)

(* Fonction mem_assoc *)

let rec mem_assoc elem = function
  | Empty -> false
  | Item ((hd, tl), tl2) -> if hd = elem then true
    else mem_assoc elem tl2
;;

(* ------------------------------------------------------------- *)

(* Fonction remove_assoc *)

let rec remove_assoc elem = function
  | Empty -> raise Not_found
  | Item ((hd, tl), tl2) -> if hd = elem then tl2
    else remove_assoc elem tl2
;;

(* ------------------------------------------------------------- *)

(* Fonction split *)

let rec split = function
  | Empty -> (Empty, Empty)
  | Item ((hd, tl), tl2) -> 
    let (x, y) = split tl2 in (Item (hd, x), Item (tl, y))
;;

(* ------------------------------------------------------------- *)

(* Fonctions bonus *)

(* Fonction fold_right *)

let rec fold_right func my_list acc =
  match my_list with
    | Empty -> acc
    | Item (hd, tl) -> func hd (fold_right func tl acc)
;;

(* ------------------------------------------------------------- *)

(* Fonction mem_assq *)

let rec mem_assq elem = function
  | Empty -> false
  | Item ((hd, tl), tl2) -> if hd == elem then true
    else mem_assq elem tl2
;;

(* ------------------------------------------------------------- *)

(* Fonction remove_assq *)

let rec remove_assq elem = function
  | Empty -> raise Not_found
  | Item ((hd, tl), tl2) -> if hd == elem then tl2
    else remove_assq elem tl2
;;

(* ------------------------------------------------------------- *)

(* Fonction iter2 *)

(* let rec iter2 func my_list1 my_list2 = function *)
(*   | Empty -> (function  *)
(*       | Empty -> () *)
(*       | _ -> raise (Failure "iter2")) *)
(*   | Item (hd, tl) -> (function *)
(*       | Empty -> () *)
(*       | Item (hd1, tl1)-> func hd hd1; iter2 func tl tl1) *)
(* ;; *)

(* ------------------------------------------------------------- *)

(* Fonction map2 *)

(* ------------------------------------------------------------- *)

(* Fonction partition *)

(* ------------------------------------------------------------- *)

(* Fonction remove_all_assoc *)

(* let rec remove_all_assoc elem = function *)
(*   | Empty -> raise Not_found *)
(*   | Item (hd, tl) -> if hd = elem *)
(*     then Empty *)
(*     else remove_all_assoc elem tl *)
(* ;; *)

(* ------------------------------------------------------------- *)

(* Fonction sort *)

(* ------------------------------------------------------------- *)

(* Fonction stable_sort *)

(* ------------------------------------------------------------- *)

(* Fonction merge *)
