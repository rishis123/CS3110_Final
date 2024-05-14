type node =
  | NonTerminal of {
      value : char;
      children : node array;
      mutable is_terminal : bool;
    }
  | Empty

type t = node array

let idx_of_char chr =
  let code = Char.code chr in
  if 65 <= code && code <= 90 then code - 65
  else if 97 <= code && code <= 122 then code - 97
  else if 48 <= code && code <= 57 then
    code - 48 + 26 (* offset numbers to after letters *)
  else 36
  (* else failwith "Invalid character" *)

let make () = Array.make 37 Empty

let rec _insert ?(str_start_idx = 0) str trie =
  if String.length str = str_start_idx then ()
  else
    let head_chr = str.[str_start_idx] in
    let array_idx = idx_of_char head_chr in
    match trie.(array_idx) with
    | NonTerminal nt ->
        let subtrie = nt.children in
        _insert str subtrie ~str_start_idx:(str_start_idx + 1);
        if String.length str - 1 = str_start_idx then nt.is_terminal <- true;
    | Empty ->
        let subtrie = make () in
        _insert str subtrie ~str_start_idx:(str_start_idx + 1);
        trie.(array_idx) <-
          NonTerminal
            {
              value = head_chr;
              children = subtrie;
              is_terminal = String.length str - 1 = str_start_idx;
            }

let insert str trie = _insert str trie

let to_string trie =
  trie
  |> Array.to_list
  |> List.filter (function
       | NonTerminal _ -> true
       | Empty -> false)
  |> List.map (fun node ->
         Tree.to_string
           ~get_name:(function
             | NonTerminal nt ->
                 BatString.of_char nt.value ^ if nt.is_terminal then "!" else ""
             | Empty -> ".")
           ~get_children:(function
             | NonTerminal nt ->
                 nt.children
                 |> Array.to_list
                 |> List.filter (function
                      | NonTerminal _ -> true
                      | Empty -> false)
             | Empty -> [])
           node)
  |> List.filter (fun l -> String.length l > 0)
  |> String.concat ""

let of_list lst =
  let trie = make () in
  List.iter (fun str -> insert str trie) lst;
  trie

let of_file path =
  let trie = make () in
  BatFile.lines_of path |> BatEnum.iter (fun str -> insert str trie);
  trie

let rec _mem ?(str_start_idx = 0) str trie =
  let array_idx = idx_of_char str.[str_start_idx] in
  match trie.(array_idx) with
  | NonTerminal nt ->
      let next_str_idx = str_start_idx + 1 in
      if String.length str = next_str_idx then nt.is_terminal
      else _mem ~str_start_idx:next_str_idx str nt.children
  | Empty -> false

let mem str trie = _mem str trie

let rec to_list_list ?(str_start_idx = 0) trie =
  let folder acc = function
    | NonTerminal nt ->
        let subtrie_str_lists =
          to_list_list ~str_start_idx:(str_start_idx + 1) nt.children
          |> List.map (fun str_l -> nt.value :: str_l)
        in
        let seen_str_lists = subtrie_str_lists @ acc in
        if nt.is_terminal then [ nt.value ] :: seen_str_lists
        else seen_str_lists
    | Empty -> acc
  in
  Array.fold_left folder [] trie

let to_list trie = to_list_list trie |> List.map Base.String.of_char_list
