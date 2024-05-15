type node = {
  value : char;
  children : node OrderedMultiset.t;
  mutable is_terminal : bool;
}

type t = node OrderedMultiset.t

let child_with_char chr trie =
  trie |> OrderedMultiset.find_opt (fun node -> node.value = chr)

let make () = OrderedMultiset.make 1

let rec _insert ?(str_start_idx = 0) str trie =
  if String.length str = str_start_idx then ()
  else
    let head_chr = str.[str_start_idx] in
    let child = child_with_char head_chr trie in
    match child with
    | Some child ->
        let subtrie = child.children in
        _insert str subtrie ~str_start_idx:(str_start_idx + 1);
        if String.length str - 1 = str_start_idx then child.is_terminal <- true
    | None ->
        let subtrie = make () in
        _insert str subtrie ~str_start_idx:(str_start_idx + 1);
        trie
        |> OrderedMultiset.insert
             {
               value = head_chr;
               children = subtrie;
               is_terminal = String.length str - 1 = str_start_idx;
             }

let insert str trie = _insert (String.lowercase_ascii str) trie

let of_list lst =
  let trie = make () in
  List.iter (fun str -> insert str trie) lst;
  trie

let of_file path =
  let trie = make () in
  BatFile.lines_of path |> BatEnum.iter (fun str -> insert str trie);
  trie

let rec _mem ?(str_start_idx = 0) str trie =
  if str = "" then false
  else
    let child = child_with_char str.[str_start_idx] trie in
    match child with
    | Some child ->
        let next_str_idx = str_start_idx + 1 in
        if String.length str = next_str_idx then child.is_terminal
        else _mem ~str_start_idx:next_str_idx str child.children
    | None -> false

let mem str trie = _mem (String.lowercase_ascii str) trie

let rec to_list_list ?(str_start_idx = 0) trie =
  let folder acc node =
    let subtrie_str_lists =
      to_list_list ~str_start_idx:(str_start_idx + 1) node.children
      |> List.map (fun str_l -> node.value :: str_l)
    in
    let seen_str_lists = subtrie_str_lists @ acc in
    if node.is_terminal then [ node.value ] :: seen_str_lists
    else seen_str_lists
  in
  OrderedMultiset.fold_left folder [] trie

let to_list trie = to_list_list trie |> List.map Base.String.of_char_list
