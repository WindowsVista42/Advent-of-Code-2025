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

let windows6x6 line prev_line =
  let windows = windows3 line in
  let prev_windows = windows3 prev_line in

  windows
  |> List.to_seq
  |> Seq.zip (List.to_seq prev_windows)

let () =
  let grid = read_data file in

  (* Pad left/right (makes later part easier) *)
  let grid = List.map (fun row ->
    [Empty] @ row @ [Empty]
  ) grid in

  let lines = List.tl grid in
  let prev_line = ref (List.hd grid) in
  let first_line = !prev_line in

  (* Simulate the grid *)
  let simulated_grid = List.map (fun line ->
    let new_line = ref [] in

    windows6x6 line !prev_line
    |> Seq.iter (fun ((p_1, p0, p1), (c_1, c0, c1)) ->
      (* Special rules for simulating the beam paths *)
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
      | (_, _, _, _, c0, _) ->
        new_line := !new_line @ [c0]
      );
    );

    (* Padding left/right *)
    prev_line := [Empty] @ !new_line @ [Empty];
    !prev_line
  ) lines in

  let simulated_grid = [first_line] @ simulated_grid in

  let sim_lines = List.tl simulated_grid in
  let sim_prev_lines = simulated_grid |> List.rev |> List.tl |> List.rev in

  let counts = ref (List.init (List.length sim_lines + 2) (fun _ -> 0)) in

  (* Use the simulated grid to count paths. *)
  Seq.zip (List.to_seq sim_prev_lines) (List.to_seq sim_lines)
  |> Seq.iter (fun (prev_line, line) ->
    let new_counts = ref [] in

    let line_windows = windows3 line |> List.to_seq in
    let prev_line_windows = windows3 prev_line |> List.to_seq in
    let count_windows = windows3 !counts |> List.to_seq in

    Seq.zip count_windows (Seq.zip prev_line_windows line_windows)
    |> Seq.iter (fun ((count_1, count0, count1), ((p_1, p0, p1), (c_1, c0, c1))) ->
      (* Special rules for counting paths *)
      (match (p_1, p0, p1, c_1, c0, c1) with
      | (_, Start, _, _, Beam, _) ->
        new_counts := !new_counts @ [1];
      | (_, _, _, _, Splitter, _) ->
        new_counts := !new_counts @ [0]
      | (Beam, _, Beam, Splitter, Beam, Splitter) ->
        new_counts := !new_counts @ [count_1 + count0 + count1]
      | (_, _, Beam, _, Beam, Splitter) ->
        new_counts := !new_counts @ [count0 + count1]
      | (Beam, _, _, Splitter, Beam, _) ->
        new_counts := !new_counts @ [count0 + count_1]
      | (_, Beam, _, _, Beam, _) ->
        new_counts := !new_counts @ [count0]
      | (_, _, _, _, Empty, _) ->
        new_counts := !new_counts @ [0]
      | (_, _, _, _, _, _) ->
        new_counts := !new_counts @ [0];
      );
    );

    counts := [0] @ !new_counts @ [0];
  );

  let path_count = List.fold_left (+) 0 !counts in

  Printf.printf "Total paths: %d\n" path_count
