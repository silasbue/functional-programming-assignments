// Exercise 2.1
let downto1 n = if n > 0 then [n .. -1 .. 1] else []

let downto2 = function
  | n when n > 0 -> [n .. -1 .. 1]
  | _ -> []

// Exercise 2.2
let rec removeOddIdx = function
  | x1::_::xs -> x1::removeOddIdx(xs)
  | x1::_ -> [x1]
  | _ -> []

// Exercise 2.3
let rec combinePair = function
  | x1::x2::xs -> (x1, x2)::combinePair(xs)
  | _ -> []

// Exercise 2.4
type complex = {
  a: float;
  b: float;
}

let mkComplex x y = {a = x; b = y}

let complexToPair c = (c.a, c.b)

let ( |+|) (c1: complex) (c2: complex) = mkComplex (c1.a + c2.a) (c1.b + c2.b)

let ( |*| ) (c1: complex) (c2: complex) = mkComplex (c1.a*c2.a - c1.b*c2.b) (c1.b*c2.a + c1.a*c2.b)

let ( |-| ) (c1: complex) (c2: complex) = c1 |+| mkComplex (-c2.a) (-c2.b)

let ( |/| ) (c1: complex) (c2: complex) = c1 |*| mkComplex (c2.a / (c2.a**2.0+c2.b**2.0)) (-c2.b / (c2.a**2.0+c2.b**2.0))


// Exercise 2.5
let explode1 (s: string) = List.ofArray (s.ToCharArray())

let rec explode2 = function
  | (s: string) when s.Length > 0 -> s.[0]::explode2 (s.Remove(0, 1))
  | _ -> []

// Exercise 2.6
let implode (cs: char list) = List.foldBack (fun c s -> c.ToString() + s) cs ""

let implodeRev (cs: char list) = List.fold (fun s c -> c.ToString() + s) "" cs

// Exercise 2.7
let toUpper s = s |> explode1 |> List.map (fun c -> System.Char.ToUpper c) |> implode

let toUpper2 = explode1 >> List.map System.Char.ToUpper >> implode

// Exercise 2.8
let rec ack = function
  | (0, n) when n > 0 -> n + 1
  | (m, 0) when m > 0 -> ack (m-1, 1)
  | (m, n) when m > 0 && n > 0 -> ack(m-1, ack(m, n-1))
  | _ -> 1

// YELLOW
// Exercise 2.9
let time f =
  let start = System.DateTime.Now
  let res = f ()
  let finish = System.DateTime.Now
  (res, finish - start)

let timeArg1 f a = (fun _ -> f a) |> time

// Exercise 2.10
let rec downto3 f n = function
  | e when n > 0 -> downto3 f (n-1) (f n e)
  | e -> e

let fac n = downto3 (fun n e -> n * e) n 1

let range (g: int -> 'a) = function
  | (n: int) -> downto3 (fun x -> fun xs -> g(x)::xs) n []


// Assignment 2.11
type word = (char * int) list
let hello : word = [('H', 4); ('E', 1); ('L', 1); ('L', 1); ('O', 1);]

// Assignment 2.12
type squareFun = word -> int -> int -> int

let letterScore d (w: word) pos acc = match w.[pos] with (_, s) -> acc + s * d

let singleLetterScore : squareFun = letterScore 1
let doubleLetterScore : squareFun = letterScore 2
let tripleLetterScore : squareFun = letterScore 3

// Assignment 2.13
let wordScore d _ _ acc = acc * d

let doubleWordScore : squareFun = wordScore 2
let tripleWordScore : squareFun = wordScore 3

// Assignment 2.14
let isConsonant = function
  | 'a'|'e'|'i'|'o'|'u'|'y'|'æ'|'ø'|'å' -> false
  | _ -> true

let oddConsonants (w: word) _ (acc: int) =
  match w with
    | w when (w |> List.filter (fun (x, _) -> isConsonant x)).Length % 2 = 1 -> -acc
    | _ -> acc
// using if-else-then
let oddConsonants1 (w: word) (pos: int) (acc: int) =
  if ((w |> List.filter (fun (x, _) -> isConsonant x)).Length % 2 = 1) then -acc else acc

// RED
// Assignment 2.15
type square = (int * squareFun) list

let SLS : square = [(0, singleLetterScore)];;
let DLS : square = [(0, doubleLetterScore)];;
let TLS : square = [(0, tripleLetterScore)];;
let DWS : square = SLS @ [(1, doubleWordScore)];;
let TWS : square = SLS @ [(1, tripleWordScore)];;

let calculatePoints (squares: square list) (w: word) =
  squares |>
  List.mapi (fun (i: int) (s: square) ->
  List.map (fun (y:int, s: square) -> (([x, (f w 0)])) |>
  List.fold (fun xs1 x1 -> x1 |> List.fold (fun xs2 x2 -> x2::xs2) xs1) [] |>
  List.sortBy (fun (x, _) -> x) |>
  List.map (fun (x, y) -> y) |>
  List.fold (fun xs x -> x >> xs) (fun x -> x) <| 0