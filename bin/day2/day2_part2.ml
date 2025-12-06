let file = "input/day2/input.txt"

type range = {
  min : int;
  max : int;
};;

let is_valid s =
  List.fold_left (fun acc v ->
    if v = 0 then
      acc
    else if String.length s mod v <> 0 then
      acc
    else
      let all_subs = List.init ((String.length s) / v) (fun i ->
        String.sub s (i * v) v
      ) in
      let all_eq = List.fold_left (fun a sub ->
        a && (sub = List.hd all_subs)
      ) true all_subs in
      if all_eq then
        false
      else
        acc
  ) true (List.init (String.length s / 2) (fun i -> i + 1))

let read_data file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  let values = String.split_on_char ',' contents in
  List.map (fun v ->
    let dash_index = String.index v '-' in
    let min_str = String.sub v 0 dash_index in
    let max_str = String.sub v (dash_index + 1) (String.length v - dash_index - 1) in
    { min = int_of_string (String.trim min_str); max = int_of_string (String.trim max_str) }
  ) values

let () =
  let ranges = read_data file in
  let total = List.fold_left (fun (acc: int) (r: range) ->
    let list = List.init (r.max - r.min + 1) (fun i -> r.min + i) in
    acc + List.fold_left (fun a v ->
      let int_str = string_of_int v in
      if is_valid int_str then
        a
      else begin
        a + v
      end
    ) 0 list;
  ) 0 ranges in
  Printf.printf "Total: %d\n" total