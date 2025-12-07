let file = "input/day6/input.txt"

let rec ( -- ) i j = if i > j then [] else i :: i + 1 -- j

let read_data file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  let lines = String.split_on_char '\n' contents in
  List.map (fun line ->
    String.trim line
    |> String.split_on_char ' '
    |> List.filter (fun word ->
      word <> " " && word <> ""
    )
  ) lines

let apply_op n0 n1 op =
  match op with
  | "+" -> n0 + n1
  | "*" -> n0 * n1
  | _ -> failwith (Printf.sprintf "unknown op %s" op)

let () =
  let lines = read_data file in

  let ops = List.hd (List.rev lines) in
  let val_lines = List.tl (List.rev lines) in

  0 -- ((List.length ops) - 1)
  |> List.map (fun i ->
    let op = List.nth ops i in
    let start_val = if op = "+" then 0 else 1 in

    List.fold_left (fun acc line ->
      let word = List.nth line i in
      let num = int_of_string word in

      apply_op acc num op
    ) start_val val_lines
  )
  |> List.fold_left (+) 0
  |> Printf.printf "Result: %d\n";
