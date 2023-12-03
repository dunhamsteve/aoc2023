open Batteries

let read_bytes (fn : string) =
  let ch = open_in fn in
  let rec read ch =
    try
      let c = input_byte ch in
      c :: read ch
    with _ -> []
  in
  read ch


let read_chars (fn : string) =
  let ch = open_in fn in
  let rec read ch =
    try
      let c = input_char ch in
      c :: read ch
    with _ -> []
  in
  read ch

let read_string (fn : string) = String.implode (read_chars fn)

let debug x =
  print_endline (string_of_int x);
  x