open Batteries
open BatList
open Aoc2023.Utils

let digits1 x =
  map (fun x -> int_of_char x - 48) x
  |> filter (fun x -> x >=0 && x < 10)

let rec digits2 (x : char enumerable) =
  match x with
  | 'o' :: 'n' :: 'e' :: _               -> 1 :: digits2 (tl x)
  | 't' :: 'w' :: 'o' :: _               -> 2 :: digits2 (tl x)
  | 't' :: 'h' :: 'r' :: 'e' :: 'e' :: _ -> 3 :: digits2 (tl x)
  | 'f' :: 'o' :: 'u' :: 'r' :: _        -> 4 :: digits2 (tl x)
  | 'f' :: 'i' :: 'v' :: 'e' :: _        -> 5 :: digits2 (tl x)
  | 's' :: 'i' :: 'x' :: _ -> 6 :: digits2 (tl x)
  | 's' :: 'e' :: 'v' :: 'e' :: 'n' :: _ -> 7 :: digits2 (tl x)
  | 'e' :: 'i' :: 'g' :: 'h' :: 't' :: _ -> 8 :: digits2 (tl x)
  | 'n' :: 'i' :: 'n' :: 'e' :: _        -> 9 :: digits2 (tl x)
  | x :: xs ->
      let x = int_of_char x in
      if x > 47 && x < 58 then (x - 48) :: digits2 xs else digits2 xs
  | [] -> []

let debug x =
  print_endline (string_of_int x);
  x

let process fn digits =
  let data = read_chars fn in
  let lines =
    nsplit (fun x -> x = '\n') data |> filter (fun l -> length l > 0)
  in
  let p1 =
    lines |> map digits
    |> map (fun line -> (hd line * 10) + last line)
    |> sum
  in
  
  print_endline (fn ^ ": " ^ string_of_int p1)

(* eg2 crashes digits1 *)
let () =
  process "day1/eg.txt" digits1;
  process "day1/input.txt" digits1;
  process "day1/eg2.txt" digits2;
  process "day1/input.txt" digits2
