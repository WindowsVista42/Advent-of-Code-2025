let file = "input/day7/input.txt"

type sim_cell = Empty | Start | Splitter | Beam

let sim_cell_of_char c =
  match c with
  | '.' -> Empty
  | 'S' -> Start
  | '^' -> Splitter
  | '|' -> Beam
  | _ -> failwith (Printf.sprintf "unknown cell %s" (String.make 1 c))

(* let string_of_sim_cell c =
  match c with
  | Empty -> "."
  | Start -> "S"
  | Splitter -> "^"
  | Beam -> "|" *)

let read_data file: sim_cell list list =
  let lines = In_channel.with_open_text file In_channel.input_lines in

  lines
  |> List.map String.trim
  |> List.map (fun line ->
    line
    |> String.to_seq
    |> Seq.map sim_cell_of_char
    |> List.of_seq
  )

let rec windows3 = function
  | a :: b :: c :: _ as lst ->
    (a, b, c) :: windows3 (List.tl lst)
  | _ -> []

let () =
  let grid = read_data file in

  (* Pad left/right (makes later part easier) *)
  let grid = List.map (fun row ->
    [Empty] @ row @ [Empty]
  ) grid in

  let split_count = ref 0 in

  let lines = List.tl grid in
  let prev_line = ref (List.hd grid) in

  List.iter (fun line ->
    let new_line = ref [] in

    let windows = windows3 line in
    let prev_windows = windows3 !prev_line in

    windows
    |> List.to_seq
    |> Seq.zip (List.to_seq prev_windows)
    |> Seq.iter (fun ((p_1, p0, p1), (c_1, c0, c1)) ->
      (match (p_1, p0, p1, c_1, c0, c1) with
      | (_, Start, _, _, Empty, _) ->
        new_line := !new_line @ [Beam]
      | (_, Beam, _, _, Empty, _) ->
        new_line := !new_line @ [Beam]
      | (_, _, Beam, _, Empty, Splitter) ->
        new_line := !new_line @ [Beam]
      | (Beam, _, _, Splitter, Empty, _) ->
        new_line := !new_line @ [Beam]
      | (_, Beam, _, _, Splitter, _) ->
        new_line := !new_line @ [Splitter];
        incr split_count
      | (_, _, _, _, c0, _) ->
        new_line := !new_line @ [c0]
      );
    );

    (* Padding left/right *)
    prev_line := [Empty] @ !new_line @ [Empty]
  ) lines;

  Printf.printf "Total splits: %d\n" (!split_count)
