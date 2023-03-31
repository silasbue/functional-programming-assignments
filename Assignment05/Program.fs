// Exercise 5.1
let sum m n =
    let rec aux acc m n =
        match n with
        | 0 -> acc + m
        | n -> aux (acc + (m + n)) m (n-1)
    aux 0 m n

// Exercise 5.2
let length lst =
    let rec aux acc lst =
        match lst with
        | [] -> acc
        | _::xs -> aux (acc + 1) xs
    aux 0 lst
    
// Exercise 5.3
let foldBack f lst acc =
    let rec aux f lst acc c =
        match lst with
        | [] -> c acc
        | x::xs -> aux f xs acc (fun r -> c (f x r))
    aux f lst acc id
    
// Exercise 5.4
let factA x =
    let rec aux acc =
        function
        | 0 -> acc
        | x -> aux (x * acc) (x - 1)
    aux 1 x

let factC x =
    let rec aux x c =
        match x with
        | 0 -> c 1
        | x -> aux (x-1) (fun r -> c (x * r))
    aux x id
    
//todo:
// factA is faster than factC because because everything in factA can be evaluated before the recursive call happens.
// This means that we don't have to store much in the memory. In factC the continuation will have to be stored until
// the last recursive call has happened. 

// Exercise 5.5
let fibW x =
    let mutable res1 = 0
    let mutable res2 = 1
    let mutable i = 1
    while (i <= x) do
        let temp = res1
        res1 <- res2
        res2 <- temp + res2
        i <- i + 1
    res1

let fibA x =
    let rec aux i acc1 acc2 =
        match i with
        | i when i = x -> acc1
        | _ -> aux (i+1) acc2 (acc1 + acc2)
    aux 0 0 1
    
let fibA2 x =
    let rec aux x acc1 acc2 =
        match x with
        | 0 -> acc1
        | x -> aux (x-1) acc2 (acc1 + acc2)
    aux x 0 1

let fibC x: int =
    let rec aux x (c: int -> 'a) =
        match x with
        | 0 -> c 0
        | x -> aux (x-1) (fun acc1 -> c (acc1 + x))
    aux x id
    
// Exercise 5.6
let rec bigListK c =
    function
    | 0 -> c []
    | n -> bigListK (fun res -> 1 :: c res) (n - 1) 
// bigListK is not an iterative function since inside the lambda expression it is appending the continuation
// call to an integer, meaning it is not a tail call. This means that the integer will need to be
// stored n times in memory before the continuations can be evaluated

// Exercise 5.7
let (|??|) a b =
    match a with
    | Some a -> a
    | None -> b

type word = (char * int) list
type aExp =
    | N of int
    | V of string
    | WL
    | PV of aExp
    | Add of aExp * aExp
    | Sub of aExp * aExp
    | Mul of aExp * aExp
    | CharToInt of cExp
and cExp =
    | C  of char
    | CV of aExp
    | ToUpper of cExp
    | ToLower of cExp
    | IntToChar of aExp

let rec arithEvalSimple a (w: word) s =
    match a with
    | N n -> n
    | V v -> (Map.tryFind v s) |??| 0
    | WL -> w.Length
    | PV p -> snd w.[arithEvalSimple p w s]
    | Add (x, y) -> arithEvalSimple x w s + arithEvalSimple y w s
    | Sub (x, y) -> arithEvalSimple x w s - arithEvalSimple y w s
    | Mul (x, y) -> arithEvalSimple x w s * arithEvalSimple y w s
    | CharToInt c -> int (charEvalSimple c w s)
    | _ -> failwith "todo"
    
and charEvalSimple c (w: word) s =
    match c with
    | C c -> c
    | ToLower c -> charEvalSimple c w s |> System.Char.ToLower
    | ToUpper c -> charEvalSimple c w s |> System.Char.ToUpper
    | CV n -> fst w.[arithEvalSimple n w s]
    | IntToChar n -> char (arithEvalSimple n w s)
    | _ -> failwith "todo"
    
// let rec arithEvalTail a (w: word) s (con: char -> 'a) =
//     match a with
//     | N n -> n
//     | V v -> (Map.tryFind v s) |??| 0
//     | WL -> w.Length
//     | PV p -> snd w.[arithEvalTail p w s con]
//     | Add (x, y) -> arithEvalTail x w s con + arithEvalTail y w s con
//     | Sub (x, y) -> arithEvalTail x w s con - arithEvalTail y w s con
//     | Mul (x, y) -> arithEvalTail x w s con * arithEvalTail y w s con
//     | CharToInt c -> con (charEvalTail c w s (fun r -> int r))
//     | _ -> failwith "todo"
//     
// and charEvalTail c (w: word) s (con2: int -> 'b): char =
//     match c with
//     | C c -> c
//     | IntToChar n -> con2 (arithEvalTail n w s (fun r -> con2 r))
//     | _ -> failwith "todo"
//
//
//
// let arithEval a w s = arithEvalTail a w s id
// let charEval c w s  = charEvalTail c w s id