(* Zadanie: przelewanka
   Autor: Grzegorz Gruza
   Code review: Adrian Górecki, grupa 1 *)


(*	[fill input volume glass] zwraca tablice stanu wypelnienia szklanek [input]
zmodyfikowana w taki sposob, ze szklanka [glass] zostala napelniona
[volume] to tablica trzymajaca objetosci szklanek *)
let fill input volume glass =
  let output = Array.copy input in
  output.(glass) <- volume.(glass);
  output

(*	[empty input glass] zwraca tablice stanu wypelnienia szklanek [input]
zmodyfikowana w taki sposob, ze szklanka [glass] zostala oprozniona*)
let empty input glass =
  let output = Array.copy input in
  output.(glass) <- 0;
  output

(*	[transfer input volume glass1 glass2] zwraca tablice stanu wypelnienia
szklanek [input] zmodyfikowana w taki sposob, ze przelano maksymalnie duzo wody
z [glass1] do [glass2] *)
let transfer input volume glass1 glass2 =
	let output = Array.copy input in
	output.(glass2) <- min volume.(glass2) (input.(glass1) + input.(glass2));
	output.(glass1) <- input.(glass1) - (output.(glass2) - input.(glass2));
	output

(*  Oblicza NWD (a, b) *)
let rec gcd a b =
  if b = 0 then a
  else gcd b (a mod b)

(* zwraca true wtedy i tylko wtedy, gdy NWD wszystkich liczb [volume]
    dzieli kazda liczbe z [final] *)
let check_gcd volume final =
  let d = Array.fold_left gcd 0 volume in
  d = 0 || (Array.for_all (fun g -> g mod d = 0) final)

(*	[przelewanka a] zwraca minimalna liczbe czynnosci
[fill] [empty] [transfer] potrzebna do uzyskania sytuacji opisanej w [a]
Jeżeli jej uzyskanie nie jest możliwe, to zwraca -1 *)
let przelewanka a =
	if a = [||] then 0
	else
		let n = Array.length a in
		let volume = Array.map (fun (x, _) -> x) a in
		let final = Array.map (fun (_, x) -> x) a in
		if not (check_gcd volume final) then -1
		else if Array.for_all (fun (v, f) -> (f > 0 && f < v)) a then -1
		else
			let result = ref (-1) in
			let q = Queue.create () in
			let top = Array.make n 0 in
			let visited = Hashtbl.create 1000 in
			Hashtbl.add visited top ();
			Queue.add (top, 0) q;
			let process new_add d =
				if not (Hashtbl.mem visited new_add) then begin
                    Hashtbl.add visited new_add ();
                    Queue.add (new_add, d + 1) q
					end in
			while not (Queue.is_empty q) && !result = (-1) do
				let top, d = Queue.take q in
				if top = final then result := d
				else
					for glass = 0 to n - 1 do
						if top.(glass) < volume.(glass)
                            then process (fill top volume glass) d;
						if top.(glass) > 0 then
                            process (empty top glass) d;
						for glass2 = 0 to n - 1 do
							if glass <> glass2 && top.(glass) <> 0 && top.(glass2) < volume.(glass2) then
                                process (transfer top volume glass glass2) d;
						done
					done
			done;
			!result
