open Y2016

let p1_list = [Day01.p1; Day02.p1; Day03.p1; Day04.p1; Day05.p1; Day06.p1; Day07.p1; Day08.p1; Day09.p1; Day10.p1; Day11.p1; Day12.p1; Day13.p1; Day14.p1; Day15.p1; Day16.p1; Day17.p1; Day18.p1; Day19.p1; Day20.p1; Day21.p1; Day22.p1; Day23.p1; Day24.p1; Day25.p1]
let p2_list = [Day01.p2; Day02.p2; Day03.p2; Day04.p2; Day05.p2; Day06.p2; Day07.p2; Day08.p2; Day09.p2; Day10.p2; Day11.p2; Day12.p2; Day13.p2; Day14.p2; Day15.p2; Day16.p2; Day17.p2; Day18.p2; Day19.p2; Day20.p2; Day21.p2; Day22.p2; Day23.p2; Day24.p2; Day25.p2]

let int_to_string n =
  Printf.sprintf "%02d" n

let test_day day_nb =
  let open Misc in
  let f_name = "day" ^ int_to_string day_nb ^ ".txt" in
  let p1 = List.nth p1_list (day_nb - 1) in
  let p2 = List.nth p2_list (day_nb - 1) in
  Printf.printf "Solution of day %02d part 1 is " day_nb;
  let time, _ = Utils.time_it (fun _ -> p1 (Misc.Read_file.read_lines_string f_name)) in
  Printf.printf " (%.2f)\n" time;
  Printf.printf "                   part 2 is ";
  let time, _ = Utils.time_it (fun _ -> p2 (Misc.Read_file.read_lines_string f_name)) in
  Printf.printf " (%.2f)\n" time

let _ =
  let day_nb = 3 in
  (* List.length p1_list in *)
  for i = day_nb to day_nb do
    test_day i
  done;
  print_endline ""