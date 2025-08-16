(* Maze module body *)
(* LAP (AMD 2023) *)

(* 
Student: 62111 Patricia Costa

Comment:

?????????????????????????
?????????????????????????
?????????????????????????
?????????????????????????
?????????????????????????
?????????????????????????

*)

(*
0123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789
	100 columns
*)


(* COMPILATION - How Mooshak builds this module:
		ocamlc -c Maze.mli Maze.ml
*)



(* AUXILIARY GENERAL FUNCTIONS - you can add more *)

(* Sorted lists with no repetitions *)
(* precondition for all the list arguments:
		isCanonical l && isCanonical l1 && isCanonical l2 *)
(* postcondition for all the list results: isCanonical result *)

let rec removeDups z = (* pre: z sorted *)
	match z with
	| [] -> []
	| [x] -> [x]
	| x::y::xs -> (if x = y then [] else [x])@ removeDups (y::xs)
;;

let canonize z = (* sort and remove duplicates *)
	removeDups (List.sort compare z)
;;

let isCanonical z = (* check if sorted and with no duplicates *)
	z = (canonize z)
;;

let belongs v l =
	List.mem v l
;;

let length =
	List.length
;;

let filter =
	List.filter
;;

let exists =
	List.exists
;;

let for_all =
	List.for_all
;;

let partition =
	List.partition
;;

let contained l1 l2 =
	for_all (fun x -> belongs x l2) l1
;;

let union l1 l2 =
	canonize (l1@l2)

let inter l1 l2 =
	filter (fun x -> belongs x l2) l1
;;

let diff l1 l2 =
	filter (fun a -> not (belongs a l2)) l1
;;

let map f l =
	canonize (List.map f l)
;;

let merge l =
	canonize (List.flatten l)
;;

let flatMap f l =
	merge (List.map f l)
;;

let showi l =
	let li = List.map string_of_int l in
	let body = String.concat "," li in
		Printf.printf "[%s]\n" body
;;

let showp l =
	let li = List.map (fun (a,b) -> Printf.sprintf "(%d,%d)" a b) l in
	let body = String.concat "," li in
		Printf.printf "[%s]\n" body
;;

(* TYPES & CONSTANTS *)

type room = int
type rooms = room list

type path = room list
type island = room list

type passage = room * room
type passages = passage list

type maze = {
    rooms: rooms;
	entrances: rooms;
    exits: rooms;
    passages: passages
}

let _NO_PATH = []

let setPassages m ps =
	{rooms = m.rooms; entrances = m.entrances; exits = m.exits; passages = ps}
;;

let setEntrances m es =
	{rooms = m.rooms; entrances = es; exits = m.exits; passages = m.passages}
;;

let setRooms m rs =
	{rooms = rs; entrances = m.entrances; exits = m.exits; passages = m.passages}
;;


(* SOME EXAMPLES - you can add more *)

let myMaze = {
    rooms = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13];
	entrances = [1;4;11];
    exits = [6;12];
    passages = [(1,2);(1,4);(1,5);(2,5);(3,6);(4,5);(4,7);
				(5,6);(5,8);(6,9);(7,8);(8,9);(10,11);(11,12)]
};;

let myMaze4 = {
    rooms = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13];
	entrances = [1;4;11];
    exits = [6;12];
    passages = [(1,8);(2,5);(3,6);(4,5);(4,7);
				(5,6);(5,8);(6,9);(7,8);(10,11);(11,12)]
};;

let myMaze3 = {
    rooms = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14];
	entrances = [1;4;11];
    exits = [6;12];
    passages = [(1,2);(1,4);(1,5);(2,5);(3,6);(4,5);(4,7);
				(5,6);(5,8);(6,9);(7,8);(8,9);(10,11);(11,12);(14,9)]
};;

let loopMaze = {
    rooms = [1;2;3;4];
	entrances = [1];
    exits = [4];
    passages = [(1,2);(2,3);(3,4);(4,1)]
};;

let myLoopMaze = {
    rooms = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13];
	entrances = [1;4;11];
    exits = [6;12];
    passages = [(1,2);(1,4);(1,5);(2,5);(3,6);(4,5);(4,7);
				(5,6);(5,8);(6,9);(7,8);(8,9);(10,11);(11,12);(12,13);(13,10)]
};;

let lilMaze = {
    rooms = [1];
	entrances = [1];
    exits = [];
    passages = []
};;

let lilLoopMaze = {
    rooms = [1;2];
	entrances = [1];
    exits = [2];
    passages = [(1,2);(2,1)]
};;

let lilLoopMaze2 = {
    rooms = [1];
	entrances = [1];
    exits = [];
    passages = [(1,1)]
};;

let myMaze2 = {
    rooms = [1;2;3;4;5];
	entrances = [1];
    exits = [4];
    passages = [(1,2);(2,3);(3,4)]
};;

let notValidMaze = {
    rooms = [1;2;3;4];
	entrances = [1];
    exits = [4];
    passages = [(1,2);(2,3);(2,4);(3,4)]
};;

let solMaze = {
    rooms = [1;2;3;4];
	entrances = [1];
    exits = [];
    passages = []
};;

(* FUNCTION isValid *)
(* pre: none *)
(* post: none *)
let isValid m =
	if m.rooms = [] || m.entrances = [] 
		then false
	else if exists (fun x -> x < 0) m.rooms
		then false
	else if not (contained m.entrances m.rooms) || not (contained m.exits m.rooms) 
		then false
	else if exists ((fun rs (x,y) -> not (belongs x rs) || not (belongs y rs)) m.rooms) m.passages 
		then false
	else if exists (fun (x,y) -> x = y) m.passages 
		then false
	else if not ((inter m.entrances m.exits) = [])
		then false
	else (isCanonical m.rooms) && (isCanonical m.entrances) && 
		(isCanonical m.exits) && (isCanonical m.passages)
;;

(* FUNCTION makeLineMaze *)
(* pre: a < b *)
(* post: isValid result *)
let rec makeLineMaze a b = {
		rooms = 
			(if a > b then [] 
			else a::(makeLineMaze (a+1) b).rooms);
		entrances = [a];
		exits = [b];
		passages = 
			(if a >= b then [] 
			else (a,a+1)::(makeLineMaze (a+1) b).passages)
};;


(* FUNCTION combine *)
(* pre: isValid m1, isValid m2 *)
(* post: isValid result *)
let combine m1 m2 = {
		rooms = union m1.rooms m2.rooms; 
		entrances = union m1.entrances m2.entrances;
		exits = diff (union m1.exits m2.exits) (union m1.entrances m2.entrances);
		passages = union m1.passages m2.passages
};;

(* FUNCTION next *)
(* pre: isValid m *)
(* post: isCanonical result *)
let rec next m r = 
	match m.passages with
	| [] -> []
	| (x,y)::xs -> if x = r 
  		then y::(next (setPassages m xs) r) 
  		else next (setPassages m xs) r
;;

(* FUNCTION next2 *)
(* pre: isValid m *)
(* post: isCanonical result *)
let next2 m rs = 
	flatMap (next m) rs
;;

(* FUNCTION prev *)
(* pre: isValid m *)
(* post: isCanonical result *)
let rec prev m r = 
	match m.passages with
	| [] -> []
	| (x,y)::xs -> if y = r 
  		then x::(prev (setPassages m xs) r) 
  		else prev (setPassages m xs) r
;;

(* FUNCTION adjacent *)
(* pre: isValid m *)
(* post: isCanonical result *)
let adjacent m r = 
	union (next m r ) (prev m r )
;;

(* FUNCTION reachable *)
(* pre: isValid m, not (hasLoop m) *)
(* post: isCanonical result *)
let rec reachable m = 
	match m.entrances with
	| [] -> []
	| xs ->  union (xs) (reachable (setEntrances m (next2 m (xs))))
;;

(* FUNCTION solitary *)
(* pre: isValid m *)
(* post: isCanonical result *)
let rec solitary m = 
	match m.rooms with
	| [] -> []
	| x::xs -> if adjacent m x = [] 
							then x::(solitary (setRooms m xs))
							else solitary (setRooms m xs)
;;

(*returns all the rooms reachable from the room r, including r*)
let rec nextOfNexts m r =
	let rs = next m r in
		match rs with
		| [] -> [r]
		| x -> r::(lnextOfNexts m x) 
and lnextOfNexts m xs =
	let rss = next2 m xs in
		match rss with
		| [] -> xs
		| x -> canonize (x@xs@(lnextOfNexts m rss))
;;

(*unite all the lists from a list of lists into a single list*)
let rec unionOfLists ls =
	match ls with
	| [] -> []
	| x::xs -> union x (unionOfLists xs)
;;

(*Unify all the lists with common elements*)
let rec unify l =
	match l with
	| [] -> []
	| x::xs ->  let (a,b) = partition ((fun l1 l2 -> inter l1 l2 = []) x) (unify xs) in 
											(union x (unionOfLists b))::a										
;;

(* FUNCTION islands *)
(* pre: isValid m, not (hasLoop m) *)
(* post: isCanonical result *)
let islands m = 
	unify (map (nextOfNexts m) m.rooms)
;;

(*checks if the last element of a list l ends with one of the elements of the list es*)
let rec doesEndWith es l =
	match l with
	| [] -> false
	| [x] -> belongs x es
	| x::xs -> doesEndWith es xs
;;

(*gets the list with the shorter length from a list of lists*)
(*pre: ls <> []*)
let rec minLengthList ls =
	match ls with
	| [x] -> x
	| x::xs ->  if List.compare_lengths x (minLengthList xs) <= 0
							then x
							else minLengthList xs
;;

let rec pathsStartingAt m e =
	let rs = next m e in
		match rs with
		| [] -> [[e]]
		| [r] -> if belongs e m.exits then 
							[e]::(map ((fun x l -> x::l) e) (pathsStartingAt m r))
							else map ((fun x l -> x::l) e) (pathsStartingAt m r)
		| r::rs -> if belongs e m.exits then 
								[e]::(map ((fun x l -> x::l) e) (union (pathsStartingAt m r) (lpaths m rs)))
								else map ((fun x l -> x::l) e) (union (pathsStartingAt m r) (lpaths m rs))
and lpaths m s =
	match s with
	| [] -> failwith "lpaths"
	| [r] -> pathsStartingAt m r
	| r::rs -> union (pathsStartingAt m r) (lpaths m rs)
;;

(* FUNCTION paths *)
(* pre: isValid m, not (hasLoop m) *)
(* post: none *)
let paths m = 
	flatMap (pathsStartingAt m) m.entrances
;;

(* FUNCTION shortest *)
(* pre: isValid m, not (hasLoop m) *)
(* post: none *)
let shortest m = 
	if m.exits = [] then _NO_PATH
	else let ps = filter (doesEndWith m.exits) (paths m) in
		match ps with
		| [] -> _NO_PATH
		| x -> minLengthList x
;;

let rec isInLoop m v r =
	if belongs r v then true
	else let rs = next m r in
	let visited = r::v in
		match rs with
		| [] -> false
		| x::xs -> (isInLoop m visited x) || (lisInLoop m visited xs)
and lisInLoop m v xs =
	match xs with
	| [] -> false
	| r::rs -> (isInLoop m v r) || (lisInLoop m v rs)
;;

(* FUNCTION hasLoop *)
(* pre: isValid m *)
(* post: none *)
let rec hasLoop m =
match m.rooms with
| [] -> false
| x::xs -> (isInLoop m [] x) || (hasLoop (setRooms m xs))
;;


let rec pathsWithLoop m v e =
	let rs = diff (next m e) v in
	let visited = union v rs in
		match rs with
		| [] -> [[e]]
		| [r] -> if belongs e m.exits then 
				[e]::(map ((fun x l -> x::l) e) (pathsWithLoop m visited r))
				else map ((fun x l -> x::l) e) (pathsWithLoop m visited r)
		| r::rs -> if belongs e m.exits then 
				[e]::(map ((fun x l -> x::l) e) (union (pathsWithLoop m visited r) (lpathsWithLoop m visited rs)))
				else map ((fun x l -> x::l) e) (union (pathsWithLoop m visited r) (lpathsWithLoop m visited rs))
and lpathsWithLoop m v s =
	match s with
	| [] -> failwith "lpathsWithLoop"
	| [r] -> pathsWithLoop m v r
	| r::rs -> union (pathsWithLoop m v r) (lpathsWithLoop m v rs)
;;

(* FUNCTION shortest2 *)
(* pre: isValid m *)
(* post: none *)
let shortest2 m = 
	if m.exits = [] then _NO_PATH
	else let ps = filter (doesEndWith m.exits) (flatMap (pathsWithLoop m []) m.entrances) in
		match ps with
		| [] -> _NO_PATH
		| x -> minLengthList x
;;
































