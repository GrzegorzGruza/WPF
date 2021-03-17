(*
 * ISet - Interval sets
 * Copyright (C) 1996-2003 Xavier Leroy, Nicolas Cannasse, 
 * Markus Mottl, Jacek Chrzaszcz, Grzegorz Gruza
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(** Interval Set.

    This is an interval set, i.e. a set of integers, where large
    intervals can be stored as single elements. Intervals stored in the
    set are disjoint. 

*)

(* 
    autor: Grzegorz Gruza
		reviewer: Konrad Skublicki
*)

open List;;

(*typ zbioru trzymanego na drzewie*)
(*Node : lewy syn, korzen, prawy syn, wysokosc, liczba elementow*)
type t =
  | Empty
  | Node of t * (int*int) * t * int * int

(*pusty zbior*)
let empty = Empty

(*Zwraca true jesli zbior jest pusty, false jesli nie jest*)
(*dziala w czasie stalym*)
let is_empty =  function
  | Empty -> true
  | _ -> false

(*zwraca wysokosc drzewa*)
(*dziala w czasie stalym*)
let height = function
  | Node (_, _, _, h, _) -> h
  | Empty -> 0

(*zwraca liczbe elementow w przedziale [x, y] 
  lub max_int jesli liczba elementow jest wieksza niz max_int*)
(*zaklada x <= y*)
(*dziala w czasie stalym*)
let distance x y =
  if x = min_int then 
		if -2 <= y then max_int else (y + max_int) + 2
  else let dis = 0 - x in
    if (y + dis) = max_int || (y + dis) < 0 then max_int 
    else y + dis + 1	

(*sumuje dwie liczby, 
  jesli ich suma przekracza max_int, to zwraca max_int*)
(*dziala w czasie stalym*)
let sum x y = 
  if x + y = max_int || x + y <=0 then max_int 
  else ((distance (-x) y) - 1)

(*zwraca liczbe elementow zbiorze*)
let dis_tree = function
  | Empty -> 0
  | Node(_, _, _, _, d) -> d 

(*tworzy wezel, gdzie lewe poddrzewo to l, prawe r, a korzen k*)
(*zaklada, ze l r sa zbalansowane*)
(*dla zbalansowanych l r zwraca zbalansowane drzewo*)
let make l k r = 
  let (k1, k2) = k in
  Node (l, k, r, max (height l) (height r) + 1, 
        sum (sum (dis_tree l) (distance k1 k2) ) (dis_tree r) )

(*zwraca 0 jesli x jest w przedziale [st, nd], 
  -1 jesli jest <, 1 jesli jest >*)
(*zaklada ze st <= nd*)
(*dziala w czasie stalym*)
let cmp x (st, nd) = 
	if x < st then -1 
	else if x <= nd then 0 
	else 1

(*zwraca true jesli x istnieje w set, false w przeciwnym razie*)
(*dla drzewa zbalansowanego dziala w czasie O(logn)*)
let mem x set =
  let rec loop = function
    | Node (l, k, r, _, _) ->
      let c = cmp x k in
      c = 0 || loop (if c < 0 then l else r)
    | Empty -> false in
  loop set

(*balansuje Node (l, k, r, _, _), 
  tak ze roznica ich wysokosci jest w przedziale [-2, 2]*)
(*dziala w czasie stalym*)
let bal l k r =
  let hl = height l in
  let hr = height r in
  if hl > hr + 2 then
    match l with
    | Node (ll, lk, lr, _, _) ->
      if height ll >= height lr then make ll lk (make lr k r)
      else
        (match lr with
         | Node (lrl, lrk, lrr, _, _) ->
           make (make ll lk lrl) lrk (make lrr k r)
         | Empty -> assert false)
    | Empty -> assert false
  else if hr > hl + 2 then
    match r with
    | Node (rl, rk, rr, _, _) ->
      if height rr >= height rl then make (make l k rl) rk rr
      else
        (match rl with
         | Node (rll, rlk, rlr, _, _) ->
           make (make l k rll) rlk (make rlr rk rr)
         | Empty -> assert false)
    | Empty -> assert false
  else (make l k r)

(*dodaje do s przedzial [add_st, add_nd]*)
(*zaklada ze s jest rozlaczne i niesasiednie z tym przedzialem*)
(*zaklada ze add_st <= add_nd*)
(*dziala w czasie O(logn)*)
let rec add_one (add_st, add_nd) s = 
  match s with
  | Node(l, (k_st, k_nd), r, _, _) ->
    if add_nd < k_st then 
      let nl = add_one (add_st, add_nd) l in 
      bal nl (k_st, k_nd) r
    else 
      let nr = add_one (add_st, add_nd) r in 
      bal l (k_st, k_nd) nr
  | Empty -> 
    Node (Empty, (add_st, add_nd), Empty, 1, distance add_st add_nd)

(*laczy dwa dowolne drzewa zbalansowane w jedno zbalansowane*)
(*dziala w czasie O(logn)*)
let rec join l v r =
  match (l, r) with
  | (Empty, _) -> add_one v r
  | (_, Empty) -> add_one v l
  | (Node(ll, lv, lr, lh, _), Node(rl, rv, rr, rh, _)) ->
    if lh > rh + 2 then bal ll lv (join lr v r) else
    if rh > lh + 2 then bal (join l v rl) rv rr else
      make l v r	

(*zwraca trzy elementy:
  zbior elementow set mniejszych od x,
  true jesli x jest w set, false w przeciwnym wypadku
  zbior elementow set wiekszych od x*)
(*dziala w czasie O(logn)*)
let split x set =
  let rec loop x = function
    | Empty -> (Empty, false, Empty)
    | Node (l, (v_st, v_nd), r, _, _) ->
      let c = cmp x (v_st, v_nd) in
      if c = 0 then 
        let nl = if x = v_st then l else add_one (v_st, x - 1) l in
        let nr = if x = v_nd then r else add_one (x + 1, v_nd) r in
        (nl, true, nr)
      else if c < 0 then
        let (ll, pres, rl) = loop x l in (ll, pres, join rl (v_st, v_nd) r)
      else
        let (lr, pres, rr) = loop x r in (join l (v_st, v_nd) lr, pres, rr) in 
  let setl, pres, setr = loop x set in
  setl, pres, setr

(*zwraca spojny przedzial drzewa o najmniejszych elementach
  lub (0,0) jesli drzewo jest puste*)
(*dziala w czasie O(logn)*)
let rec min_elt = function
  | Node (Empty, k, _, _, _) -> k
  | Node (l, _, _, _, _) -> min_elt l
  | Empty -> (0, 0)

(*zwraca drzewo bez elementu minimalnego*)
(*dziala w czasie O(logn)*)
let rec remove_min_elt = function
  | Node (Empty, _, r, _, _) -> r
  | Node (l, k, r, _, _) -> bal (remove_min_elt l) k r
  | Empty -> invalid_arg "PSet.remove_min_elt"

(*zwraca spojny przedzial drzewa o najwiekszych elementach
  lub (0,0) jesli drzewo jest puste*)
(*dziala w czasie O(logn)*)
let rec max_elt = function
  | Node (_, k, Empty, _, _) -> k
  | Node (_, _, r, _, _) -> max_elt r
  | Empty -> (0, 0) (*wyjatek*)

(*zwraca drzewo bez elementu maksymalnego *)
(*dziala w czasie O(logn)*)
let rec remove_max_elt = function
  | Node (l, _, Empty, _, _) -> l
  | Node (l, k, r, _, _) -> bal l k (remove_max_elt r)
  | Empty -> invalid_arg "PSet.remove_min_elt"

(*usuwa przedzial [st, nd] z set*)
(*zaklada ze st <= nd*)
(*dziala w czasie O(logn)*)
let remove (st, nd) set =
  let (l, _, _) = split st set in
  let (_, _, r) = split nd set in
  match l, r with 
  | l, Empty -> l
  |	Empty, r -> r
  | l, r -> 
    let min = min_elt r in
    let nr = remove_min_elt r in
    let max = max_elt l in
    let nl = remove_max_elt l in
    join (add_one max nl) min nr 

(*zwraca niemalejaca liste spojnych przedzialow zbioru s*)
(*dziala w czasie O(n)*)	
let elements s =
  let rec loop s a =
    match s with
    | Empty -> a
    | Node(r, e, l, _, _) -> (loop l (e :: loop r a) )
  in rev (loop s []);;

(*zwraca (f xn (f xn-1 (... ( f(x1 a))))) *)
(*dziala w czasie O(n)*O(f), o ile s jest zbalansowane*)
let fold f s a = 
  let rec loop f s a =
    match s with
    | Empty -> a
    | Node(r, e, l, _, _) -> (loop f l (f e (loop f r a) ) )
  in loop f s a;;

(*[iter f s] stosuje [f] do wszystkich ciaglych przedzialow w [s].
  w rosnacej kolejnosci*)
(*dziala w czasie O(n)*)
let iter f set =
  let rec loop = function
    | Empty -> ()
    | Node (l, k, r, _, _) -> loop l; f k; loop r in
  loop set	

(*dodaje dowolny przedzial [st, nd] do zbioru set*)
(*zaklada ze st <= nd*)
(*dziala w czasie O(logn), o ile drzewo jest zbalansowane*)
let rec add (st, nd) set = match set with
  | Empty -> Node(Empty, (st, nd) , Empty, 1, distance st nd)
  | Node(l, (k_st, k_nd), r, _, _) ->
    if nd + 1 < k_st && nd <> max_int then 
      join (add (st, nd) l) (k_st, k_nd) r
    else if k_nd < st - 1 && st <> min_int then 
      join l (k_st, k_nd) (add (st, nd) r)
    else
      let nl = if k_st <= st then l else remove (st, k_st) l in 
      let nr = if nd <= k_nd then r else remove (k_nd, nd) r in
      let nk = (min k_st st, max k_nd nd) 

      in
      (*laczy przedzialy postaci [p, q] [q+1, r] *)
      let disambiguation set = match set with
        | Empty -> Empty
        | Node(l, (k_st, k_nd), r, _, _) ->
          let (ml_st, ml_nd) = max_elt l in
          let (mr_st, mr_nd) = min_elt r in
          let new_set = 
            if ml_nd + 1 < k_st || l = Empty then set 
            else add (ml_st, ml_nd) (remove (ml_st, ml_nd) set) in 
          let new_set = 
            if k_nd + 1 < mr_st || r = Empty then new_set
            else add (mr_st, mr_nd) (remove (mr_st, mr_nd) new_set)in 
          new_set 
      in 
      match (disambiguation (make nl nk nr)) with 
      | Node(l, k, r, _, _) -> join l k r 
      | Empty -> Empty

(*zwraca liczbe elementow zbioru s rowna lub mniejsza niz x
  lub max_int jesli jest ich wiecej niz max_int*)	
(*dziala w czasie O(logn)*)
let below x set = match set with
  | Empty -> 0
  | _ -> 
    let l, b, _ = split x set in
    match l with
    | Node (_ ,_ ,_ ,_ , c) -> if b then sum 1 c else c
    | Empty -> if b then 1 else 0