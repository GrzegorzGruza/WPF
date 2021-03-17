(** Autor: Grzegorz Gruza
	Reviewer: Witold Drzewakowski, gr 6*)

(** Typ złączalnej kolejki priorytetowej zaimplementowanej za pomoca 
	drzewa lewicowego, w ktorym Node to wezel drzewa, a Leaf to lisc drzewa
	Node: wartosc w wezle, lewe poddrzewo, prawe poddrzewo, prawa wysokosc *)
type 'a queue = 
	| Node of 'a * 'a queue * 'a queue * int
	| Leaf
	
(** Pusta kolejka priorytetowa *)
let empty = Leaf

(** Wyjątek podnoszony przez [delete_min] gdy kolejka jest pusta *)
exception Empty

(** Zwraca [true] jeśli dana kolejka jest pusta. W przeciwnym razie [false] *)
let is_empty q = match q with
	| Leaf -> true
	| _ -> false

(** Zwraca dlugosc skrajnie prawej sciezki w danym drzewie lewicowym *)
let get_path_length q = match q with
	| Leaf -> 0
	| Node (_, _, _, p) -> p

	
(** [join q1 q2] zwraca złączenie kolejek [q1] i [q2] *)
let rec join q1 q2 = match q1, q2 with
	| Leaf, q -> q
	| q, Leaf -> q
	| (Node (q1_v, q1_l, q1_r, _)), (Node (q2_v, q2_l, q2_r, _)) ->
		if q1_v < q2_v then 
			let q3 = join q1_r q2 in
			if get_path_length q1_l < get_path_length q3 then 
				Node (q1_v, q3, q1_l, get_path_length q1_l + 1)
			else Node (q1_v, q1_l, q3, get_path_length q3 + 1)
		else 
			let q3 = join q2_r q1 in
			if get_path_length q2_l < get_path_length q3 then 
				Node (q2_v, q3, q2_l, get_path_length q2_l + 1)
			else Node (q2_v, q2_l, q3, get_path_length q3 + 1)

(** Dla niepustej kolejki [q], [delete_min q] zwraca parę [(e,q')] gdzie [e]
    jest elementem minimalnym kolejki [q] a [q'] to [q] bez elementu [e].
    Jeśli [q] jest puste podnosi wyjątek [Empty]. *)
let delete_min q = match q with
	| Leaf -> raise Empty
	| Node (v, l, r, _) -> (v, join l r)
	
(** [add e q] zwraca kolejkę powstałą z dołączenia elementu [e] 
    do kolejki [q] *)
let add e q = join q (Node (e, Leaf, Leaf, 1))