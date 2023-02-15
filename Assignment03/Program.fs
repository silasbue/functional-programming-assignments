// Assignments 3.1
type aExp =
| N of int            // Integer value
| V of string         // Variable
| WL                  // Length of the word
| PV of aExp          // Point value of character at specific word index
| Add of aExp * aExp  // Addition
| Sub of aExp * aExp  // Subtraction
| Mul of aExp * aExp  // Multiplication
  
let (.+.) a b = Add (a, b)
let (.-.) a b = Sub (a, b)
let (.*.) a b = Mul (a, b)

let rec arithEvalSimple =
    function
    | N x -> x
    | Add (x, y) -> arithEvalSimple x + arithEvalSimple y
    | Sub (x, y) -> arithEvalSimple x - arithEvalSimple y
    | Mul (x, y) -> arithEvalSimple x * arithEvalSimple y
    | _ -> failwith "todo"

// Assignment 3.2
let (|??|) a b =
    match a with
    | Some a -> a
    | None -> b

let rec arithEvalState a s =
    match a with
    | N n -> n
    | V v -> (Map.tryFind v s) |??| 0
    | Add (x, y) -> arithEvalState x s + arithEvalState y s
    | Sub (x, y) -> arithEvalState x s - arithEvalState y s
    | Mul (x, y) -> arithEvalState x s * arithEvalState y s
    | _ -> failwith "todo"

// Assignment 3.3
let arithSingleLetterScore = PV (V "_pos_") .+. (V "_acc_")
let arithDoubleLetterScore = ((N 2) .*. PV (V "_pos_")) .+. (V "_acc_")
let arithTripleLetterScore = ((N 3) .*. PV (V "_pos_")) .+. (V "_acc_")
let arithDoubleWordScore = N 2 .*. V "_acc_"
let arithTripleWordScore = N 3 .*. V "_acc_"

type word = (char * int) list
let hello : word = [('H', 4); ('E', 1); ('L', 1); ('L', 1); ('O', 1);]

let rec arithEval a (w: word) s =
    match a with
    | N n -> n
    | V v -> (Map.tryFind v s) |??| 0
    | WL -> w.Length
    | PV p -> snd w.[arithEval p w s]
    | Add (x, y) -> arithEval x w s + arithEval y w s
    | Sub (x, y) -> arithEval x w s - arithEval y w s
    | Mul (x, y) -> arithEval x w s * arithEval y w s

// Assignment 3.4
type cExp =
| C  of char      (* Character value *)
| ToUpper of cExp (* Converts lower case to upper case character, non-letters are unchanged *)
| ToLower of cExp (* Converts upper case to lower case character, non-letters are unchanged *)
| CV of aExp      (* Character lookup at word index *)

let rec charEval c (w: word) s =
    match c with
    | C c -> c
    | ToLower c -> charEval c w s |> System.Char.ToLower
    | ToUpper c -> charEval c w s |> System.Char.ToUpper
    | CV n -> fst w.[arithEval n w s]
    
// Assignment 3.5
type bExp =
| TT                    (* true *)
| FF                    (* false *)
| AEq of aExp * aExp    (* numeric equality *)
| ALt of aExp * aExp    (* numeric less than *)
| Not of bExp           (* boolean not *)
| Conj of bExp * bExp   (* boolean conjunction *)
| IsDigit of cExp       (* check for digit *)
| IsLetter of cExp      (* check for letter *)
| IsVowel of cExp       (* check for vowel *)

let (~~) b = Not b
let (.&&.) b1 b2 = Conj (b1, b2)
let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)

let (.=.) a b = AEq (a, b)
let (.<.) a b = ALt (a, b)
let (.<>.) a b = ~~(a .=. b)                (* numeric inequality *)
let (.<=.) a b = a .<. b .||. ~~(a .<>. b)  (* numeric less than or equal to *)
let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)

let isVowel =
    function
    | 'a' | 'e' | 'i' | 'o' | 'u' | 'æ' | 'ø' | 'å' -> true
    | _ -> false

let rec boolEval b (w: word) s =
    match b with
    | TT -> true
    | FF -> false
    | AEq (a, b) -> arithEval a w s = arithEval b w s
    | ALt (a, b) -> arithEval a w s < arithEval b w s
    | Not b -> not (boolEval b w s)
    | Conj (a, b) -> boolEval a w s && boolEval b w s
    | IsDigit c -> charEval c w s |> System.Char.IsDigit
    | IsLetter c -> charEval c w s |> System.Char.IsLetter
    | IsVowel c -> charEval c w s |> isVowel