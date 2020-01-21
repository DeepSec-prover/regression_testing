type colour =
  | Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White

type decoration =
  | Bold
  | Underline

let colour_to_int = function
  | Black -> 0 | Red     -> 1 | Green -> 2 | Yellow -> 3
  | Blue  -> 4 | Magenta -> 5 | Cyan  -> 6 | White  -> 7


let coloured_terminal_text colour deco text =
  let deco_str =
    List.fold_left (fun acc decoration -> match decoration with
      | Bold -> "\027[1m"^acc
      | Underline -> "\027[4m"^acc
    ) "" deco
  in

  Printf.sprintf "%s\027[3%dm%s\027[0m" deco_str (colour_to_int colour) text


type info =
  | Good of bool * string
  | Error

let rec get_info_v1 l = match l with
  | [] -> []
  | "true"::time::q ->
      if not (String.contains time 's' || String.contains time 'm' || String.contains time 'h')
      then [Error]
      else Good (true,time) :: get_info_v1 q
  | "false"::time::q ->
      if not (String.contains time 's' || String.contains time 'm' || String.contains time 'h')
      then [Error]
      else Good(false,time) :: get_info_v1 q
  | _ -> [Error]

let rec get_info_v2 l = match l with
  | [] -> []
  | "true"::time::mem::q ->
      if not (String.contains time 's' || String.contains time 'm' || String.contains time 'h')
      then [Error]
      else
        if not (String.contains mem 'B')
        then [Error]
        else Good (true,time) :: get_info_v2 q
  | "false"::time::mem::q ->
      if not (String.contains time 's' || String.contains time 'm' || String.contains time 'h')
      then [Error]
      else
        if not (String.contains mem 'B')
        then [Error]
        else Good(false,time) :: get_info_v2 q
  | _ -> [Error]

let are_info_equal l1 l2 =
  if List.length l1 <> List.length l2
  then false
  else
    List.for_all2 (fun i1 i2 -> match i1,i2 with
      | Error, Error -> true
      | Good(b1,_), Good(b2,_) -> b1 = b2
      | _ -> false
    ) l1 l2

let problematic_files = ref []

let recorded_time = ref []

let rec compare_two_by_two title v_l (s_l:string list) i_l = match i_l with
  | [] | [_] -> true
  | t1::t2::q ->
      let v_l' = List.tl v_l in
      let s_l' = List.tl s_l in
      let eq_info =
        if not (are_info_equal t1 t2)
        then
          begin
            problematic_files := (title,List.hd v_l,List.hd v_l',List.hd s_l,List.hd s_l') :: !problematic_files;
            false
          end
        else true
      in
      eq_info && compare_two_by_two title v_l' s_l' (t2::q)

let rec record_time i title i_l_l =
  if List.hd i_l_l = []
  then ()
  else
    begin
      let (time_l,above_min,q_i_l_l) =
        List.fold_right (fun i_l (acc_t,acc_above,acc_q) -> match List.hd i_l with
          | Error -> (acc_t,acc_above,(List.tl i_l)::acc_q)
          | Good(_,t) ->
              if String.contains t 'm' || String.contains t 'm'
              then (t::acc_t,true,(List.tl i_l)::acc_q)
              else (t::acc_t,acc_above,(List.tl i_l)::acc_q)
        ) i_l_l ([],false,[])
      in

      if above_min
      then recorded_time := (String.concat ";" (title :: string_of_int i :: time_l)) :: !recorded_time;

      record_time (i+1) title q_i_l_l
    end

let compare_one_line_in_files version_list line_str line_list =

  let title = List.hd (List.hd line_list) in
  if List.exists (fun l -> title <> List.hd l) line_list
  then failwith "Unexpected mismatch of title";

  let main_line_list = List.map (fun l -> List.tl l) line_list in

  let info_lists =
    List.map2 (fun version line ->
      if version = "1.02"
      then get_info_v1 line
      else get_info_v2 line
    ) version_list main_line_list
  in

  if compare_two_by_two title version_list line_str info_lists
  then record_time 1 title info_lists

let rec compare_all_files version_list line_str_list =
  if List.hd line_str_list = []
  then ()
  else
    begin
      let line_str_q = List.map (fun l -> List.tl l) line_str_list in
      let line_str = List.map (fun l -> List.hd l) line_str_list in
      let line_list = List.map (fun l -> String.split_on_char ';' (List.hd l)) line_str_list in
      compare_one_line_in_files version_list line_str line_list;
      compare_all_files version_list line_str_q
    end

let rec display_problematic_files version_list problem_files = match version_list with
  | [] | [_] -> ()
  | v :: v' :: q ->
      let (files,rest) = List.partition (fun (_,v1,_,_,_) -> v = v1) problem_files in
      if files = []
      then Printf.printf "Check from %s to %s: %s\n" v v' (coloured_terminal_text Green [Bold] "Same results")
      else
        begin
          Printf.printf "Check from %s to %s: %s\n" v v' (coloured_terminal_text Red [Bold] (Printf.sprintf "Found %d differences !" (List.length files)));
          List.iter (fun (title,v1,v2,t1,t2) ->
            Printf.printf "    - File %s:\n" title;
            Printf.printf "       -> In %s: %s\n" v1 t1;
            Printf.printf "       -> In %s: %s\n" v2 t2
          ) files;
        end;

      display_problematic_files (v'::q) rest

let display_record_time version_list =
  if !recorded_time = []
  then print_string ((coloured_terminal_text Blue [Bold] "No queries took more than a minute to verified")^"\n")
  else
    begin
      print_string ((coloured_terminal_text Blue [Bold] "Time comparaison of queries:")^"\n");
      Printf.printf "File;Query;%s\n" (String.concat ";" version_list);
      List.iter (fun str -> Printf.printf "%s\n" str) !recorded_time
    end

let versions = ["1.02";"2.0.0-rc";"v2.0.0-beta"]
let type_files = ["fast";"medium";"slow";"above"]

let get_files v t =
  let in_ch = open_in (Printf.sprintf "formatted_outputs/%s_%s.txt" t v) in
  let _ = input_line in_ch in

  let lines = ref [] in

  begin
    try
      while true do
        let l = input_line in_ch in
        lines := l::!lines
      done
    with End_of_file -> ()
  end;

  List.rev !lines

let get_all_files_one_version exclude_above v t =
  if t = "all"
  then
    List.fold_right (fun t' acc ->
      if exclude_above && t' = "above"
      then acc
      else (get_files v t') @ acc
    ) type_files []
  else get_files v t

let rec extract inside v1 v2 = function
  | [] -> failwith "Error"
  | v::q when v = v1 -> v1 :: (extract true v1 v2 q)
  | v::q when v = v2 -> [v2]
  | v::q ->
      if inside
      then v::(extract true v1 v2 q)
      else extract false v1 v2 q

let get_all_files_and_version v1 v2 type_file =
  let version_list = extract false v1 v2 versions in
  let exclude_above = v1 = "1.02" in

  let files =
    List.map (fun v ->
      get_all_files_one_version exclude_above v type_file
    ) version_list
  in
  version_list,files

let apply_comparaison v1 v2 type_file =
  let (v_list,f_list) = get_all_files_and_version v1 v2 type_file in
  compare_all_files v_list f_list;

  display_problematic_files v_list !problematic_files;
  print_string "\n";
  display_record_time v_list


let _ = match Array.length Sys.argv with
  | 2 ->
    (* All *)
    apply_comparaison (List.hd versions) (List.hd (List.rev versions)) Sys.argv.(1)
  | 3 ->
    (* From arg to end *)
    apply_comparaison Sys.argv.(2) (List.hd (List.rev versions)) Sys.argv.(1)
  | 4 ->
    (* From first arg to second arg *)
    apply_comparaison Sys.argv.(2) Sys.argv.(3) Sys.argv.(1)
  | _ -> Printf.printf "Usage : compare type [ <v1> [ <v2> ] ]\n"
