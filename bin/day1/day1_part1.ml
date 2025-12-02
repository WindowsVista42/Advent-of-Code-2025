let file = "input.txt"

type state = {
  mutable nzero : int;
  mutable value : int;
};;

let read_lines file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents

let () =
  let s: state = { nzero = 0; value = 50 } in
  let lines = read_lines file in
  List.iter (fun line ->
    let first_char = String.get line 0 in
    let rest_num = String.sub line 1 (String.length line - 1) in
    let rest_value = int_of_string (String.trim rest_num) in

    if first_char = 'L' then
      s.value <- s.value - rest_value
    else if first_char = 'R' then
      s.value <- s.value + rest_value;

    s.value <- s.value mod 100;
    if s.value == 0 then
      s.nzero <- s.nzero + 1;
  ) lines;

  Printf.printf "Final zero count: %d\n" s.nzero