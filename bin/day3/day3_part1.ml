let file = "input/day3/input.txt"

let c_to_int (c: char) : int = 
  match c with
    '1' -> 1
  | '2' -> 2
  | '3' -> 3
  | '4' -> 4
  | '5' -> 5
  | '6' -> 6
  | '7' -> 7
  | '8' -> 8
  | '9' -> 9
  | '0' -> 0
  | _ -> 0

let int_to_c (i: int) : char =
  match i with
    1 -> '1'
  | 2 -> '2'
  | 3 -> '3'
  | 4 -> '4'
  | 5 -> '5'
  | 6 -> '6'
  | 7 -> '7'
  | 8 -> '8'
  | 9 -> '9'
  | 0 -> '0'
  | _ -> '0'

let best_in_string s =
  let len = String.length s in
  let (best_i, best) = List.fold_left (fun (best_i, best) i ->
    let c = String.get s i in
    let value = c_to_int c in
    if value > best then
      (i, value)
    else
      (best_i, best)
    ) (0, 0) (List.init len (fun i -> i)) in
  (best_i, best)

let best_in_line line =
  let sub1 = String.sub line 0 (String.length line - 2) in
  let (best1_i, best1) = best_in_string sub1 in
  let sub2 = String.sub line (best1_i + 1) (String.length line - best1_i - 2) in
  let (_, best2) = best_in_string sub2 in
  let c1 = int_to_c best1 in
  let c2 = int_to_c best2 in
  let pair_str = Printf.sprintf "%c%c" c1 c2 in
  Printf.printf "Pair: %s\n" pair_str;
  int_of_string pair_str

let read_data file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents

let () =
  let lines = read_data file in
  let total = List.fold_left (fun (acc: int) line ->
    acc + best_in_line line
  ) 0 lines in
  Printf.printf "Total: %d\n" total