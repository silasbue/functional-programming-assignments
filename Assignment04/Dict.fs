module Dictionary

type Dictionary =
    | Leaf of bool
    | Node of bool * Map<char, Dictionary>

let (|??|) a b =
    match a with
    | Some a -> a
    | None -> b

let empty () = Leaf false

let rec insert (s: string) (d: Dictionary)=
    match d with
    | Leaf _ when s = "" -> Leaf true
    | Leaf b when b = false -> Node (false, Map.empty |> Map.add (s.Chars(0)) (insert (s.Substring(1, s.Length-1)) (empty ())))
    | Node (b, m) when m.ContainsKey(s.Chars(0)) -> Node (b, m.Add (s.Chars(0), insert (s.Substring(1, s.Length-1)) (Map.find (s.Chars(0)) (m))))
    | Node (b, m) -> Node (b, m.Add (s.Chars(0), insert (s.Substring(1, s.Length-1)) (empty ())))
    
    
    // | Node (b, m) when b = false -> Node (b, m.Add ((s.Chars(0)), (insert (s.Substring(1, s.Length-1)) (Node (b, Map.empty)))))
    // | _ -> failwith "todo"