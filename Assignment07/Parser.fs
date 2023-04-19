module ImpParser

    open System
    open Eval
    open Types

    (*

    The interfaces for JParsec and FParsecLight are identical and the implementations should always produce the same output
    for successful parses although running times and error messages will differ. Please report any inconsistencies.

    *)

    open JParsec.TextParser             // Example parser combinator library. Use for CodeJudge.
    // open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.
    
    let pIntToChar  = pstring "intToChar"
    let pPointValue = pstring "pointValue"

    let pCharToInt  = pstring "charToInt"
    let pToUpper    = pstring "toUpper"
    let pToLower    = pstring "toLower"
    let pCharValue  = pstring "charValue"

    let pTrue       = pstring "true"
    let pFalse      = pstring "false"
    let pIsDigit    = pstring "isDigit"
    let pIsLetter   = pstring "isLetter"
    let pIsVowel   = pstring "isVowel"

    let pif       = pstring "if"
    let pthen     = pstring "then"
    let pelse     = pstring "else"
    let pwhile    = pstring "while"
    let pdo       = pstring "do"
    let pdeclare  = pstring "declare"

    let whitespaceChar = satisfy System.Char.IsWhiteSpace <?> "whitespace"
    let pletter        = satisfy System.Char.IsLetter <?> "letter"
    let palphanumeric  = satisfy System.Char.IsLetterOrDigit <?> "alphanumeric"
    let spaces         = many (satisfy System.Char.IsWhiteSpace) <?> "spaces"
    let spaces1        = many1 (satisfy System.Char.IsWhiteSpace) <?> "space1"

    let (.>*>.) p1 p2 = (p1 .>> spaces) .>>. p2
    let (.>*>) p1 p2  = (p1 .>> spaces) .>> p2
    let (>*>.) p1 p2  = (p1 .>> spaces) >>. p2

    let parenthesise (p: Parser<'a>) = ((pstring "(") .>>. spaces) >>. p .>> (spaces .>>. pstring(")"))

    let charsToString (c: char list) = List.fold (fun s c -> s + System.Char.ToString c) "" c
    let pid = ((pchar '_') <|> pletter) .>>. (many (palphanumeric <|> pletter)) |>> (fun (a, b) -> System.Char.ToString a + charsToString b)

    
    let unop op a = (op >>. spaces) >>. a
    let binop op p1 p2 = ((p1 .>> spaces) .>> op) .>>. (spaces >>. p2)

    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()
    let CharParse, cref = createParserForwardedToRef<cExp>()

    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"
    do tref := choice [AddParse; SubParse; ProdParse]
    

    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"
    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    do pref := choice [MulParse; ModParse; DivParse; AtomParse]

    let NParse   = pint32 |>> N <?> "Int"
    let VParse   = pid |>> V <?> "Var"
    let ParParse = parenthesise TermParse
    let NegParse = unop (pchar '-') (ProdParse |>> (fun a -> (.*.) (N -1) a))
    let PVParse  = unop pPointValue AtomParse |>> PV <?> "PV"
    let CharToIntParse = unop pCharToInt CharParse |>> CharToInt <?> "CharToInt"
    do aref := choice [CharToIntParse; NegParse; ParParse; PVParse; VParse; NParse]

    let apostrophesis (p: Parser<'a>) = pchar ''' >>. p .>> pchar '''
    
    let AexpParse = TermParse
    
    let CParse         = apostrophesis (pletter <|> whitespaceChar) |>> C <?> "Char"
    let CVParse        = unop pCharValue AtomParse |>> CV <?> "CV"
    let CParParse      = parenthesise CharParse
    let ToUpperParse   = unop pToUpper CharParse |>> ToUpper <?> "ToUpper"
    let ToLowerParse   = unop pToLower CharParse  |>> ToLower <?> "ToLower"
    let IntToCharParse = unop pIntToChar AexpParse |>> IntToChar <?> "IntToChar"
    do cref := choice [CParParse; IntToCharParse; ToUpperParse; ToLowerParse; CVParse; CParse;]

    let CexpParse = CharParse

    let BexpParse = pstring "not implemented"

    let stmntParse = pstring "not implemented"


    let parseSquareProg _ = failwith "not implemented"

    let parseBoardProg _ = failwith "not implemented"

    let mkBoard (bp : boardProg) : board = failwith "not implemented"

