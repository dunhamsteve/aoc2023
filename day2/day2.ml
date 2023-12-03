open Batteries
open BatList
open Aoc2023.Utils

type draw = (int * int * int)
[@@deriving show]

type game = (int * draw list)
[@@deriving show]

let max_draw (x : draw) (y : draw) : draw =
    let open Int in
    let (a,b,c) = x and (d,e,f) = y in
    (max a d, max b e, max c f)

let lte_draw (x : draw) (y : draw) : bool =
      let open Int in
      let (a,b,c) = x and (d,e,f) = y in
      a <= d && b <= e && c <= f

let foo = List.fold
let parse_part (s : string): draw =
  let open String in
  match split s ~by:" " with
    | (n, "red")   -> (int_of_string n, 0, 0)
    | (n, "green") -> (0,int_of_string n, 0)
    | (n, "blue")  -> (0,0,int_of_string n)
    | _            -> (0, 0, 0)

let parse_draw (s : string): draw =
  let open String in
  String.split_on_string s ~by:", "
  |> List.map parse_part
  |> fold max_draw (0,0,0)

let parseGame (s : string) : game = 
  let open String in
  let (a,b) = split s ~by:": " in
  let (_,n) = split a ~by:" " in 
  let parts = 
      split_on_string ~by:"; " b
      |>  List.map parse_draw in
  (int_of_string n, parts)

let get_max = fold max_draw (0,0,0)
let sum : int enumerable -> int = fold (fun x y -> x + y) 0

let process fn =
  let content = read_string fn in
  let games = String.split_on_string ~by:"\n" content
    |> List.filter (fun s -> String.length s > 0)
    |> map parseGame
  in
  let part1 : int =
    map (fun (n,draws) -> if lte_draw (get_max draws) (12,13,14) then n else 0) games |> sum
  in
  let part2 : int = map (fun (n, draws) -> (let a,b,c = (get_max draws) in a * b * c)) games |> sum 
  in
  (* map (fun g -> print_endline (show_game g)) games; *)
  print_endline (fn ^ " p1 " ^ string_of_int part1);
  print_endline (fn ^ " p2 " ^ string_of_int part2);
  ()
  
(* eg2 crashes digits1 *)
let () = 
  process "day2/eg.txt";
  process "day2/input.txt"
  