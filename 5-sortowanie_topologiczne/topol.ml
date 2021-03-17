(** Sortowanie topologiczne
    autor: Grzegorz Gruza
    code reviewer: Grzegorz Kopania *)
		
open PMap

(** wyjatek rzucany przez [topol] gdy zaleznosci sa cykliczne *)
exception Cykliczne

(** funkcja przyjmujaca reprezentacje grafu w postaci listy sasiedztwa
		i zwracajaca parę: 
		- graf reprezentowany za pomoca mapy, gdzie kluczem jest nazwa
		wierzcholka, a wartoscia lista jego sasiedztwa
		- liste wierzcholkow o niezerowym stopniu (byc moze z powtorzeniami) *)
let change_representation start_list =
	let update (neighbors, nodes) (w, old_list) = 
		let new_list = try (find w neighbors) with Not_found -> [] in
		add w (old_list @ new_list) neighbors, w :: nodes
	in
	List.fold_left update (empty, []) start_list

(** funkcja przyjmujaca mape sasiedztwa oraz wierzcholek i zwracajaca:
		0 jesli wierzcholek nie zostal nigdy odwiedzony
		1 jesli wierzcholek zostal odwiedzony, ale nie przetworzony 
		2 jesli wierzcholek zostal odwiedzony i przetworzony *)
let position visit w =
	try (find w visit) with Not_found -> 0

(** Dla danej listy [(a_1,[a_11;...;a_1n]); ...; (a_m,[a_m1;...;a_mk])] 
    zwraca liste, na ktorej kazdy z elementow a_i oraz a_ij wystepuje
    dokladnie raz i ktora jest uporzadkowana w taki sposob, ze kazdy
    element a_i jest przed kazdym z elementow a_i1 ... a_il *)
let topol l =
	let neighbors, nodes = change_representation l in
		let rec dfs (old_list, visit) w =
			let p = position visit w in
			if p = 2 then (old_list, visit) 
			else if p = 1 then raise Cykliczne 
			else
				let neighbors_w = try (find w neighbors) with Not_found -> [] in
				let visit = add w 1 visit in
				let old_list, visit = List.fold_left dfs (old_list, visit) neighbors_w in
				let visit = add w 2 visit in
				(w::old_list, visit) 
		in
		let (end_lst, _) = List.fold_left dfs ([], empty) nodes in
		end_lst
	
