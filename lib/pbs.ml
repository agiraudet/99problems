let print_opt = function
  | None -> print_endline "None"
  | Some x -> print_endline x

let range lo hi =
  let rec aux hi acc = if hi < lo then acc else aux (hi - 1) (hi :: acc) in
  aux (hi - 1) []

let rec tail_of_a_list = function
  | [] -> None
  | [x] -> Some x
  | _ :: t -> tail_of_a_list t

let rec two_elements_of_a_list = function
  | [] | [_] -> None
  | [a; b] -> Some (a, b)
  | _ :: t -> two_elements_of_a_list t

let rec nth_element_of_a_list lst n =
  match lst with
  | [] -> failwith "nth"
  | h :: t -> if n = 0 then h else nth_element_of_a_list t (n - 1)

let length_of_a_list lst =
  let rec aux acc = function [] -> acc | _ :: t -> aux (acc + 1) t in
  aux 0 lst

let reverse_a_list lst =
  let rec aux acc = function [] -> acc | h :: t -> aux (h :: acc) t in
  aux [] lst

let palindrome lst = if reverse_a_list lst = lst then true else false

type 'a node = One of 'a | Many of 'a node list

let flatten_a_list lst =
  let rec aux acc = function
    | [] -> acc
    | h :: t -> (
      match h with
      | One x -> aux (x :: acc) t
      | Many x -> aux (aux [] x @ acc) t )
  in
  reverse_a_list @@ aux [] lst

let flatten_a_list2 lst =
  let rec aux acc = function
    | [] -> acc
    | One x :: t -> aux (x :: acc) t
    | Many m :: t -> aux (aux acc m) t
  in
  reverse_a_list @@ aux [] lst

let eliminate_duplicate lst =
  let is_in x = function [] -> false | h :: _ -> x = h in
  let rec aux acc = function
    | [] -> acc
    | h :: t -> aux (if is_in h acc then acc else h :: acc) t
  in
  reverse_a_list @@ aux [] lst

let pack_consecutive_duplicate lst =
  let rec pack acc lst =
    match lst with
    | [] -> (acc, lst)
    | h :: t as l -> (
      match acc with
      | [] -> pack (h :: acc) t
      | u :: _ -> if h = u then pack (h :: acc) t else (acc, l) )
  in
  let rec aux acc = function
    | [] -> acc
    | l ->
        let a, lt = pack [] l in
        aux (a :: acc) lt
  in
  reverse_a_list @@ aux [] lst

let run_length_encoding lst =
  let rec aux acc n cur = function
    | [] -> (n, cur) :: acc
    | h :: t ->
        if h = cur then aux acc (n + 1) h t else aux ((n, cur) :: acc) 1 h t
  in
  match lst with [] -> [] | h :: t -> reverse_a_list @@ aux [] 1 h t

type 'a rle = One of 'a | Many of int * 'a

let modified_run_length_encoding lst =
  let make_rle n cur = if n > 1 then Many (n, cur) else One cur in
  let rec aux acc n cur = function
    | [] -> make_rle n cur :: acc
    | h :: t ->
        if h = cur then aux acc (n + 1) h t
        else aux (make_rle n cur :: acc) 1 h t
  in
  match lst with [] -> [] | h :: t -> reverse_a_list @@ aux [] 1 h t

let decode_a_run_lenght_encoded_list lst =
  let make_rle n cur = if n > 1 then Many (n, cur) else One cur in
  let rec aux acc = function
    | [] -> acc
    | One x :: t -> aux (x :: acc) t
    | Many (n, x) :: t -> aux (x :: acc) (make_rle (n - 1) x :: t)
  in
  reverse_a_list @@ aux [] lst
