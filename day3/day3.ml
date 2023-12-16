open Batteries
open BatList
open Aoc2023.Utils

(* Should I have some names here? *)
type gear = int
type num = (int * int * int) (* start stop val *)
type line = (gear list * num list)
type problem = line list
type tuple = line * line * line

let mkline (s : string) =
  let rec go (gs : gear list) (ns : num list) (p : int) (cs : char list) =
    let open Char in
    let front = List.take_while is_digit cs in
    let back = List.drop_while is_digit cs in
    let len = length front in
    if len > 0 
      then let v = int_of_string (String.implode front) in 
           go gs ((p,p+len,v)::ns) (p + len) back
      else match cs with
      | []        -> (gs, ns)
      | '.' :: cs -> go gs ns (p+1) cs
      | c :: cs   -> go (p :: gs) ns (p+1) cs
  in go [] [] 0 (String.to_list s)

let tuples (prob : line list) : (line * line * line) list =
  let rec go ls prev = match ls, prev with
  | a :: b :: rest, prev -> (prev, a, b) :: go (b :: rest) a
  | [a], prev -> (prev, a, ([],[])) :: go [] a
  | [], _ -> []
  in
  go prob ([],[])

let is_adj : int -> num -> bool = 
  fun g (start, stop, _) -> g >= start - 1 && g <= stop

let rec ispart  (gs : gear list) (l : num) : bool = 
  exists (fun g -> is_adj g l) gs

let part1 (prob : line list) =
  let open List in
  let get_parts (a,b,c) = filter (ispart (flatten [fst a; fst b; fst c])) (snd b)
  in
  tuples prob 
    |> map get_parts 
    |> flatten
    |> fold (fun a (_,_,v) -> a + v) 0

let part2_tuple : tuple -> 'a = fun (a,b,c) ->
  let gears = fst b in
  let nums = flatten [snd a; snd b; snd c] in
  map (fun g -> filter (is_adj g) nums) gears
      |> map (function | [(_,_,a);(_,_,b)] -> a*b | _ -> 0)
      |> sum
  
let part2 (prob : line list) =
  tuples prob
    |> map part2_tuple
    |> sum

let process fn =
  let lines = read_lines fn in
  let prob = List.map mkline lines in
  print_endline fn;
  print_endline ("part1 " ^ string_of_int (part1 prob));
  print_endline ("part2 " ^ string_of_int (part2 prob))

let () = 
  process "day3/eg.txt";
  process "day3/input.txt"
  
