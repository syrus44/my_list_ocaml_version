my_list_ocaml_version
=====================

MyList Project in OCaml : projet d'Ocaml à Epitech. Émulation d'un type générique my_list et liste des commandes utilisables avec
ce type.

Création d'un type my_list dans l'interpréteur Ocaml et définition d'une my_list : 
type 'a my_list = 
  | Item of ('a * 'a my_list)
  | Empty
;;

let list_test = Item("toto", Item("titi", Item("tata", Item("tutu", Empty))))
;;

Langages : OCaml

NB : Je ne suis pas responsable des -42 obtenus par les étudiants d'Epitech qui tomberont par hasard sur ce dépôt.
Ce dépôt est à titre informatif uniquement.
