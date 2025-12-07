let file = "input/day6/input.txt"

let rec ( -- ) i j = if i > j then [] else i :: i + 1 -- j

let read_data file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  let lines = String.split_on_char '\n' contents in

  let ops = lines |> List.rev |> List.hd in
  let chars = List.init (String.length ops) (String.get ops) in

  let char_i = List.mapi (fun i c -> (c, i)) chars in
  let positions = (List.fold_left (fun acc_list (c, i) ->
    if c <> ' ' then acc_list @ [i] else acc_list
  ) [] char_i)
  @ [List.length chars + 1] in

  let starts = positions |> List.rev |> List.tl |> List.rev in
  let ends = positions |> List.tl in

  let ranges = List.map2 (fun start_i end_i ->
    (start_i, end_i)
  ) starts ends in

  let quads = ranges
  |> List.map (fun (start_i, end_i) -> 
    List.fold_left (fun acc line ->
      let word = String.sub line start_i (end_i - start_i - 1) in
      acc @ [word]
    ) [] lines
  ) in

  quads

let char_to_int c =
  int_of_char c - int_of_char '0'

let apply_op n0 n1 op =
  match op with
  | "+" -> n0 + n1
  | "*" -> n0 * n1
  | _ -> failwith (Printf.sprintf "unknown op %s" op)

let () =
  let quads = read_data file in

  quads
  |> List.map (fun q ->
    let len = q |> List.hd |> String.length in
    let op = q |> List.rev |> List.hd |> String.trim in
    let vals = q |> List.rev |> List.tl in
    let start_val = if op = "+" then 0 else 1 in

    (* Iterate columns *)
    0 -- (len - 1)
    |> List.fold_left (fun total col ->
      (* Iterate rows *)
      let (number, _) = 0 -- (List.length vals - 1)
      |> List.fold_left (fun (num, power) row ->
        (* Construct number from row *)
        let line = List.nth vals row in
        let c = String.get line col in

        if c = ' ' then
          (num, power)
        else
          (num + char_to_int c * power, power * 10)
      ) (0, 1) in

      apply_op total number op
    ) start_val
  )
  |> List.fold_left (+) 0
  |> Printf.printf "Result: %d\n";
