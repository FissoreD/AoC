let read_lines_fun f filename =
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := f (input_line chan) :: !lines
    done;
    !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines

let read_line_fun f filename =
  match read_lines_fun f filename with
  | [hd] -> hd
  | _ -> invalid_arg "Read_line error: file should have only one line"

let read_line_string = read_line_fun Fun.id
let read_lines_string = read_lines_fun Fun.id
let read_line_char_list = read_line_fun
