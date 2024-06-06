let test_day day_nb p1 p2 =
  let open Misc in
  let f_name = "../txt/day" ^ Printf.sprintf "%02d" day_nb ^ ".txt" in
  Printf.printf "Solution of day %02d part 1 is " day_nb;
  let time, _ = Utils.time_it (fun _ -> p1 (Misc.Read_file.read_lines_string f_name)) in
  Printf.printf " (%.2f)\n" time;
  Printf.printf "                   part 2 is ";
  let time, _ = Utils.time_it (fun _ -> p2 (Misc.Read_file.read_lines_string f_name)) in
  Printf.printf " (%.2f)\n" time