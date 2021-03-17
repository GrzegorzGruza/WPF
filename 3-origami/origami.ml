(** 
		Autor: Grzegorz Gruza
		Code rewiever: Igor Witkowski
*)


(** Punkt na płaszczyźnie *)
type point = float * float

(** Zlozona kartka: ile razy kartkę przebije szpilka wbita w danym punkcie *)
type kartka = point -> int

(** [odbij p1 p2 p] zwraca wpolrzedne punktu [p] po odbiciu wzgledem prostej 
		przechodzacej przez punkty [p1] i [p2] *)
let odbij (p1_x, p1_y) (p2_x, p2_y) (x, y) = 
	let a = p1_y -. p2_y in 
	let b = p2_x -. p1_x in
	let c =  p1_y *. (-. b) +. p1_x *. (-. a)  in
	(((b ** 2. -. a ** 2.) *. x -. (2. *. a *. b *. y) -. (2. *. a *. c)) /.
		(a ** 2. +. b ** 2.),
		((a ** 2. -. b ** 2.) *. y -. (2. *. a *. b *. x) -. (2. *. b *. c)) /.
		(a ** 2. +. b ** 2.))

(** [iloczyn p1 p2 p] zwraca iloczyn wektorowy wektorow [p1 - p2] [p1 - p] *)
let iloczyn (p1_x, p1_y) (p2_x, p2_y) (x, y) =
	(p2_x -. p1_x) *. (y -. p1_y) -. (p2_y -. p1_y) *. (x -. p1_x)  
 
(** [prostokat p1 p2] zwraca kartkę, reprezentującą domknięty prostokąt
		o bokach równoległych do osi układu współrzędnych i lewym dolnym rogu [p1]
		a prawym górnym [p2]. Punkt [p1] musi więc być nieostro na lewo i w dół
		od punktu [p2]. Gdy w kartkę tę wbije się szpilkę wewnątrz
		(lub na krawędziach) prostokąta, kartka zostanie przebita 1 raz,
		w pozostałych przypadkach 0 razy *)
let prostokat (p1_x, p1_y) (p2_x, p2_y) = fun (x, y) ->
	if p1_x <= x && x <= p2_x && p1_y <= y && y <= p2_y then 1 else 0

(** [kolko p r] zwraca kartkę, reprezentującą kółko domknięte o środku 
		w punkcie [p] i promieniu [r] *)
let kolko (p_x, p_y) r = fun (x, y) ->
	if (p_x -. x) ** 2. +. (p_y -. y) ** 2. <=  r ** 2. then 1 else 0

(** [zloz p1 p2 k] składa kartkę [k] wzdłuż prostej przechodzącej przez
		punkty [p1] i [p2] (muszą to być różne punkty). Papier jest składany
		w ten sposób, że z prawej strony prostej (patrząc w kierunku od [p1] 
		do [p2]) jest przekładany na lewą. Wynikiem funkcji jest złożona kartka.
		Jej przebicie po prawej stronie prostej zwraca 0. Przebicie dokładnie na 
		prostej zwrócić tyle samo, co przebicie kartki przed złożeniem. 
		Po stronie lewej - tyle co przed złożeniem plus przebicie rozłożonej 
		kartki w punkcie, który nałożył się na punkt przebicia. *)
let zloz p1 p2 k = fun (x, y) ->
	let ilo = iloczyn p1 p2 (x, y) in
	if ilo  < 0. then 0 
	else if ilo > 0. then  k (x, y) + k (odbij p1 p2 (x, y))
	else k (x, y)

(** [skladaj [(p1_1,p2_1);...;(p1_n,p2_n)] k = 
		zloz p1_n p2_n (zloz ... (zloz p1_1 p2_1 k)...)]
		czyli wynikiem jest złożenie kartki [k] kolejno wzdłuż wszystkich prostych
		z listy *)
let skladaj l k = 
	List.fold_left (fun k (p1, p2) -> zloz p1 p2 k) k l   

