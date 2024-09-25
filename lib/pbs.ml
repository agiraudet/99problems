(* HELPERS *)
let print_opt = function
  | None -> print_endline "None"
  | Some x -> print_endline x

(* 99 Problems *)

(* Tail of a List *)
let rec last = function [] -> None | [ x ] -> Some x | _ :: t -> last t

(* Last Two Elements of a List *)
let rec last_two = function
  | [] | [ _ ] -> None
  | [ a; b ] -> Some (a, b)
  | _ :: t -> last_two t

(* N'th Element of a List *)
let rec nth lst n =
  match lst with
  | [] -> failwith "nth"
  | h :: t -> if n = 0 then h else nth t (n - 1)

(*  Length of a List *)
let length lst =
  let rec aux acc = function [] -> acc | _ :: t -> aux (acc + 1) t in
  aux 0 lst

(* Reverse a List *)
let rev lst =
  let rec aux acc = function [] -> acc | h :: t -> aux (h :: acc) t in
  aux [] lst

(* Palindrome *)
let is_palindrome lst = if rev lst = lst then true else false

(*  Flatten a List *)
type 'a node = One of 'a | Many of 'a node list

let flatten lst =
  let rec aux acc = function
    | [] -> acc
    | h :: t -> (
        match h with
        | One x -> aux (x :: acc) t
        | Many x -> aux (aux [] x @ acc) t)
  in
  rev @@ aux [] lst

let flatten2 lst =
  let rec aux acc = function
    | [] -> acc
    | One x :: t -> aux (x :: acc) t
    | Many m :: t -> aux (aux acc m) t
  in
  rev @@ aux [] lst

(*  Eliminate Duplicates *)
let compress lst =
  let is_in x = function [] -> false | h :: _ -> x = h in
  let rec aux acc = function
    | [] -> acc
    | h :: t -> aux (if is_in h acc then acc else h :: acc) t
  in
  rev @@ aux [] lst

(* Pack Consecutive Duplicates *)
let pack lst =
  let rec inpack acc lst =
    match lst with
    | [] -> (acc, lst)
    | h :: t as l -> (
        match acc with
        | [] -> inpack (h :: acc) t
        | u :: _ -> if h = u then inpack (h :: acc) t else (acc, l))
  in
  let rec aux acc = function
    | [] -> acc
    | l ->
        let a, lt = inpack [] l in
        aux (a :: acc) lt
  in
  rev @@ aux [] lst

(* Run-Length Encoding *)
let encode lst =
  let rec aux acc n cur = function
    | [] -> (n, cur) :: acc
    | h :: t ->
        if h = cur then aux acc (n + 1) h t else aux ((n, cur) :: acc) 1 h t
  in
  match lst with [] -> [] | h :: t -> rev @@ aux [] 1 h t

(* Modified Run-Length Encoding *)
type 'a rle = One of 'a | Many of int * 'a

let mod_encode lst =
  let make_rle n cur = if n > 1 then Many (n, cur) else One cur in
  let rec aux acc n cur = function
    | [] -> make_rle n cur :: acc
    | h :: t ->
        if h = cur then aux acc (n + 1) h t
        else aux (make_rle n cur :: acc) 1 h t
  in
  match lst with [] -> [] | h :: t -> rev @@ aux [] 1 h t

(* Decode a Run-Length Encoded List *)
let decode lst =
  let make_rle n cur = if n > 1 then Many (n, cur) else One cur in
  let rec aux acc = function
    | [] -> acc
    | One x :: t -> aux (x :: acc) t
    | Many (n, x) :: t -> aux (x :: acc) (make_rle (n - 1) x :: t)
  in
  rev @@ aux [] lst

(* Duplicate the Elements of a List *)
let duplicate lst =
  let rec aux acc = function [] -> acc | h :: t -> aux (h :: h :: acc) t in
  rev @@ aux [] lst

(* Replicate the Elements of a List a Given Number of Times *)
let replicate lst n =
  let rec dup acc e = function 0 -> acc | x -> dup (e :: acc) e (x - 1) in
  let rec aux acc = function [] -> acc | h :: t -> aux (dup [] h n @ acc) t in
  rev @@ aux [] lst

(* Drop Every N'th Element From a List *)
let drop lst n =
  let rec aux acc i = function
    | [] -> acc
    | h :: t -> if i = n then aux acc 1 t else aux (h :: acc) (i + 1) t
  in
  rev @@ aux [] 1 lst

(* Split a List Into Two Parts; The Length of the First Part Is Given *)
let split lst len_a =
  let rec aux acc n = function
    | [] -> (rev acc, [])
    | h :: t -> if n < len_a then aux (h :: acc) (n + 1) t else (rev acc, h :: t)
  in
  aux [] 0 lst

(* Extract a Slice From a List *)
let slice lst i k =
  let rec aux acc n = function
    | [] -> acc
    | h :: t ->
        if n > k then acc
        else if n >= i then aux (h :: acc) (n + 1) t
        else aux acc (n + 1) t
  in
  rev @@ aux [] 0 lst

(* Rotate a List N Places to the Left *)
let rotate lst n =
  let l, r = split lst n in
  r @ l

(* Remove the K'th Element From a List *)
let remove_at k lst =
  let rec aux acc n = function
    | [] -> List.rev acc
    | h :: t -> if n = k then List.rev acc @ t else aux (h :: acc) (n + 1) t
  in
  aux [] 0 lst

(* Insert an Element at a Given Position Into a List *)
let insert_at e p = function
  | [] -> [ e ]
  | lst ->
      let l, r = split lst p in
      l @ (e :: r)

(* Create a List Containing All Integers Within a Given Range *)
let range lo hi =
  let rec aux hi acc = if hi < lo then acc else aux (hi - 1) (hi :: acc) in
  aux hi []
