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
    | _ -> 0

// Assignment 3.2
let (|??|) a b =
    match a with
    | Some a -> a
    | None -> b
    
let rec arithEvalState a s =
    match a with
    | N x -> x
    | V x -> (Map.tryFind x s) |??| 0
    | Add (x, y) -> arithEvalState x s + arithEvalState y s
    | Sub (x, y) -> arithEvalState x s - arithEvalState y s
    | Mul (x, y) -> arithEvalState x s * arithEvalState y s
    | _ -> 0

// Assignment 3.3
type word = (char * int) list