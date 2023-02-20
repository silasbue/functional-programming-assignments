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
| C of char       (* Character value *)
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
| IsConsonantY of cExp

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
    System.Char.ToLower >>
    function
    | 'a' | 'e' | 'i' | 'o' | 'u' | 'y'| 'æ' | 'ø' | 'å' -> true
    | _ -> false

// for 3.10 (y is a consonant)
let isConsonantY =
    System.Char.ToLower >>
    function
    | 'a' | 'e' | 'i' | 'o' | 'u' | 'æ' | 'ø' | 'å' -> false
    | _ -> true

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
    | IsConsonantY c -> charEval c w s |> isConsonantY

// Assignment 3.6
let isConsonant c = ~~(IsVowel c)
let isConsonant2 c = Not (IsVowel c)
let isConsonant3 = IsVowel >> (~~)

// Assignment 3.7
type stmnt =
| Skip                        (* does nothing *)
| Ass of string * aExp        (* variable assignment *)
| Seq of stmnt * stmnt        (* sequential composition *)
| ITE of bExp * stmnt * stmnt (* if-then-else statement *)
| While of bExp * stmnt       (* while statement *)

let rec evalStmnt stm w s =
    match stm with
    | Skip -> s
    | Ass (x, a) -> s |> Map.add x (arithEval a w s)
    | Seq (stm1, stm2) -> evalStmnt stm1 w s |> evalStmnt stm2 w
    | ITE (guard, stm1, stm2) ->
        match boolEval guard w s with
        | true  -> evalStmnt stm1 w s
        | false -> evalStmnt stm2 w s
    | While (guard, stm) ->
        match boolEval guard w s with
        | true  -> evalStmnt stm w s |> evalStmnt (While (guard, stm)) w
        | false -> s

// Assignment 3.8
type squareFun = word -> int -> int -> int

let stmntToSquareFun (stm: stmnt): squareFun =
    (fun (w: word) pos acc
      -> evalStmnt stm w (Map.ofList [("_pos_", pos); ("_acc_", acc)])
         |> Map.find "_result_")

let singleLetterScore = stmntToSquareFun (Ass ("_result_", arithSingleLetterScore))
let doubleLetterScore = stmntToSquareFun (Ass ("_result_", arithDoubleLetterScore))
let tripleLetterScore = stmntToSquareFun (Ass ("_result_", arithTripleLetterScore))
let doubleWordScore = stmntToSquareFun (Ass ("_result_", arithDoubleWordScore))
let tripleWordScore = stmntToSquareFun (Ass ("_result_", arithTripleWordScore))
let containsNumbers =
  stmntToSquareFun
    (Seq (Ass ("_result_", V "_acc_"),
          While (V "i" .<. WL,
                 ITE (IsDigit (CV (V "i")),
                      Seq (
                           Ass ("_result_", V "_result_" .*. N -1),
                           Ass ("i", WL)),
                      Ass ("i", V "i" .+. N 1)))))

let oddConsonants =
        (Seq (Ass ("_result_", V "_acc_"),
             While (V "i" .<. WL,
                    ITE (IsConsonantY (CV (V "i")),
                         Seq (
                             Ass ("_result_", V "_result_" .*. N -1),
                             Ass ("i", V "i" .+. N 1)),
                         Ass ("i", V "i" .+. N 1)))))

// Assignment 3.10
type square = (int * squareFun) list
type square2 = (int * stmnt) list

let SLS = [(0, Ass ("_result_", arithSingleLetterScore))]
let DLS = [(0, Ass ("_result_", arithDoubleLetterScore))]
let TLS = [(0, Ass ("_result_", arithTripleLetterScore))]
let DWS = [(1, Ass ("_result_", arithDoubleWordScore))] @ SLS
let TWS = [(1, Ass ("_result_", arithTripleWordScore))] @ SLS

let calculatePoints (squares: square list) (w: word) =
    squares
    |> List.mapi (fun i s -> (i, s))
    |> List.map (fun (i, s) ->
          match s with
          | (a, b)::(x, f)::_ -> [(a, b w i); (x, f w i)]
          | (x, f)::_ -> [(x, f w i)]
          | _ -> failwith "todo")
    |> List.fold (fun xs x -> x @ xs) []
    |> List.sortBy fst
    |> List.map snd
    |> List.fold (fun x xs -> x >> xs) id
    <| 0

let calculatePoints2 (squares: square2 list) =
    squares
    |> List.fold (fun xs s -> xs @ [(List.map (fun (x, stm) -> (x, stmntToSquareFun stm)) s)]) []
    |> calculatePoints
