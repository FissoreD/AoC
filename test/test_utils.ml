let test_day y d p1 p2 =
  let f_name = "../txt/day" ^ Printf.sprintf "%02d" d ^ ".txt" in
  let t1, s1 = Utils.time_it (fun _ -> p1 (Read_file.read_lines_string f_name)) in
  let t2, s2 = Utils.time_it (fun _ -> p2 (Read_file.read_lines_string f_name)) in
  Printf.printf "(%.2fs) Solution of y%d/%02d part 1 is %s\n" t1 y d s1;
  Printf.printf "(%.2fs) Solution of y%d/%02d part 2 is %s\n" t2 y d s2