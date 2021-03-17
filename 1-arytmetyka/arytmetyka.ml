(*Autor: Grzegorz Gruza
Reviewer: Przemyslaw Kusiak & Michal Leszczynski*)

(*jezeli 3 argument to true, to jest to przedzial od 1. do 2. argumentu, w przeciwnym razie*
jest to suma przedzilow od -infinity do 1. oraz od 2. argumentu do +infinity *)
type wartosc = Inter of float*float*bool;; 

let wartosc_dokladnosc x p = Inter (min(x -. (p /. 100.0 *. x)) (x +. (p /. 100.0 *. x)), 
	max(x -. (p /. 100.0 *. x)) (x +. (p /. 100.0 *. x)), true)
let wartosc_od_do x y = Inter (x, y, true)
let wartosc_dokladna x = Inter (x, x, true)

(*odpowidnio max a b oraz min a b, tylko jesli dla argumentu nan i nie_nan zwracany jest nie_nan*)
let maxi a b = if classify_float a = FP_nan then b else if classify_float b = FP_nan then a else max a b
let mini a b = if classify_float a = FP_nan then b else if classify_float b = FP_nan then a else min a b

(*spradza czy x to Inter(nan, nan, true)*)
let is_nan x = match x with Inter(a, b, p) -> (classify_float a = FP_nan || classify_float b = FP_nan) 

let in_wartosc y x = match y with
	|Inter (a, b, true) -> if a <= x &&  x <= b then true else false
	|Inter (a, b, false) -> if a < x &&  x < b then false else true

let min_wartosc x = match x with 
	|Inter (a, b, true) -> a
	|Inter (a, b, false) -> neg_infinity
	
let max_wartosc x = match x with 
	|Inter (a, b, true) -> b
	|Inter (a, b, false) -> infinity

let sr_wartosc x = 
	if classify_float (min_wartosc x) = FP_infinite 
	&& classify_float (max_wartosc x) = FP_infinite then nan
	else (min_wartosc x +. max_wartosc x) /. 2.

(*Funkcja sumująca dwa przedziały postaci Inter(a, b, true), gdzie a=-inf v b=_inf v (a=b=0)*)
let sum_inter x y = match (x, y) with
	|(Inter(0., 0., true), Inter(0., 0., true) ) -> Inter(0., 0., true)
	|(Inter(a, b, p), Inter(c, d, q)) -> 
		if (a=neg_infinity && c=neg_infinity) then Inter(neg_infinity, max b d, true)
		else if (b=infinity && d=infinity) then Inter(min a c, infinity, true)
		else if (a<=c && c<=b || c<=a && a<=d) then Inter(neg_infinity, infinity, true) 
		else Inter(mini b d, maxi a c, false)
	
(*Rozważamy 4 przypadki w zaleznosci od tego czy kazdy z przedzialow ma 3 wartosc rowna true czy false*)
let plus x y = if is_nan x || is_nan y then Inter(nan, nan, true) else match (x, y) with
	|(Inter (a, b, true), Inter (c, d, true)) -> Inter (a+.c, b+.d, true)
	|(Inter (a, b, false), Inter (c, d, false)) -> Inter(neg_infinity, infinity, true)
	|(Inter (a, b, true), Inter (c, d, false)) -> if c +. b >= a +. d then Inter(neg_infinity, infinity, true) 
		else Inter( c +. b, a +. d, false)
	|(Inter (c, d, false), Inter (a, b, true)) -> if c +. b >= a +. d then Inter(neg_infinity, infinity, true) 
		else Inter(c +. b, a +. d, false)

(*Odejmowanie A B jest zdefiniowane jako dodawanie przedzialu A i [-B]*)	
let minus x y = match (x, y) with
	|(Inter (a, b, p), Inter(c, d, q)) -> plus (Inter(a, b, p)) (Inter(-.d, -.c, q))
	
(*Rozważamy 4 przypadki w zaleznosci od tego czy kazdy z przedzialow ma 3 wartosc rowna true czy false*)
let rec razy x y = if is_nan x || is_nan y then Inter(nan, nan, true) else match (x, y) with
	|(Inter(a, b, true), Inter(c, d, true)) -> 
		if (a = 0. && b = 0. )||(c = 0. && d = 0.) then Inter(0., 0., true)
		else Inter ((mini (mini (a *. c) (a *. d)) (mini (b *. c) (b *. d))), 
		maxi (maxi (a *. c) (a *. d)) (maxi (b *. c) (b *. d)), true)
	|(Inter(a, b, false), Inter(c, d, true)) -> 
		sum_inter (razy (Inter(neg_infinity, a, true)) (Inter(c, d, true))) 
		(razy (Inter(b, infinity, true)) (Inter(c, d, true))) 
	|(Inter(c, d, true), Inter(a, b, false)) -> razy (Inter(a, b, false)) (Inter(c, d, true))
	|(Inter(a, b, false), Inter(c, d, false)) -> 
		if 0. <= a || b <= 0. || 0. <= c || d <= 0. then Inter(neg_infinity, infinity, true) 
		else Inter(maxi (a *. d) (b *. c), mini (a *. c) (b *. d), false);;

(*Dzielenie A B jest zdefiniowane jako mnozenie przedzialu A i [1/B]*)
let podzielic x y = if is_nan x || is_nan y then Inter(nan, nan, true) else match (x, y) with
	|(Inter(a, b, _), Inter(c, d, _)) -> if (c=0. && d=0.) then Inter(nan, nan, true) else match y with
	|Inter(a, b, true) -> 
		if classify_float a = FP_infinite && classify_float b = FP_infinite then razy x (Inter(neg_infinity, infinity, true))
		else if 0.<=a || b<=0.
		then razy x (Inter((if b = 0. then neg_infinity else 1. /. b), (if a = 0. then infinity else 1. /. a), true))
		else razy x (Inter((if a = 0. then neg_infinity else 1. /. a), (if b = 0. then infinity else 1. /. b), false))
	|Inter(a, b, false) -> if a <= 0. && 0. <= b
		then razy x (Inter((if a = 0. then neg_infinity else 1. /. a), (if b = 0. then infinity else 1. /. b), true))
		else razy x (Inter((if b = 0. then neg_infinity else 1. /. b), (if a = 0. then infinity else 1. /. a), false));;
