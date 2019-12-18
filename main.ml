open Printf

let isNumber = function '0' .. '9' -> true | _ -> false
let isOp = function '*' | '+' | '-' | '=' | '^' -> true | _ -> false

let parseError str nd err =
    print_string "\027[31mSyntax Error:\027[0m ";
    print_endline (match (str.[nd], isNumber str.[nd], err) with
        | ('(', _, "") | (')', _, "") -> "bracket might be missmatched"
        | ('+', _, "") | ('-', _, "") | ('=', _, "") | ('*', _, "") | ('^', _, "") -> "unexpected token"
        | (' ', false, "") -> "two space in a row"
        | (_, false, "") -> "unexpected character"
        | (_, true, "") -> "unexpected token"
        | _ -> err);
    print_endline str;
    let rec loop n =
        if n < 0 then ""
        else " " ^ loop (n - 1)
    in
    print_endline ((loop (nd - 1)) ^ "^");
    exit 1

(* PARSING *)

let typeMe c =
    if isNumber c = true then 1
    else if c = '.' then 1
    else if isOp c = true then 2
    else if c = ' ' then 3
    else if c = '(' || c = ')' then 5
    else if c = 'x' || c = 'X' then 6
    else 4

let catchSyntaxError str =
    let rec loop n n1 op nb x =
        if n = String.length str && op = true then parseError str (n - 1) ""
        else if n = String.length str then ()
        else if n1 = '.' && isNumber str.[n] = false then parseError str n ""
        else if str.[n] = n1 && typeMe str.[n] <> 5 && isNumber str.[n] = false then parseError str n ""
        else match typeMe str.[n] with
        | 6 -> if x = true then parseError str n "" else loop (n + 1) str.[n] false true true
        | 5 -> (if str.[n] = ')' && op = true then parseError str n ""
                else if str.[n] = '(' && op = false && n <> 0 then parseError str n ""
                else loop (n + 1) str.[n] op nb x)
        | 4 -> parseError str n ""
        | 3 -> loop (n + 1) str.[n] op nb x
        | 2 -> if op = true then parseError str n "" else loop (n + 1) str.[n] true false false
        | 1 -> if nb = true && typeMe n1 <> 1 then parseError str n "" else loop (n + 1) str.[n] false true false
        | _ -> parseError str n ""
    in loop 0 'W' false false false

let catchEq str =
    let rec loop n neq els =
        if n = String.length str then ()
        else match (str.[n], neq, els) with
            | ('=', false, true) -> loop (n + 1) true els
            | ('=', false, false) ->  parseError str n ""
            | ('=', true, _) -> parseError str n "multiple equal"
            | _ -> loop (n + 1) neq true
    in loop 0 false false

let catchPower str =
    let rec loop n pow br =
        if n = String.length str then ()
        else match str.[n] with
            | '^' -> if pow = true then parseError str n "multiple power" else loop (n + 1) true br
            | 'X' | 'x' -> if pow = true then parseError str n "X in expodential" else loop (n + 1) pow br
            | '*' | '+' | '-' | '=' -> if br > 0 then loop (n + 1) pow br else loop (n + 1) false br
            | '(' -> loop (n + 1) pow (br + 1)
            | ')' -> loop (n + 1) pow (br - 1)
            | _ -> loop (n + 1) pow br
    in loop 0 false 0

let catchX str =
    let rec loop n =
        if n = String.length str then (print_endline ("\027[31mCalc Error:\027[0m no X founded\n" ^ str); exit 1)
        else match str.[n] with
            | 'X' | 'x' -> ()
            | _ -> loop (n + 1)
    in loop 0

let catchBracket str =
    let rec loop n br =
        if br < 0 then parseError str (n - 1) ""
        else if n >= String.length str && br > 0 then parseError str (n - 1) "Non terminated bracket"
        else if n >= String.length str then ()
        else match str.[n] with
        | '(' -> loop (n + 1) (br + 1)
        | ')' -> loop (n + 1) (br - 1)
        | '=' -> if br > 0 then parseError str (n - 1) ""
        | _ -> loop (n + 1) br
    in loop 0 0

let catchError str =
    catchSyntaxError str;
    catchEq str;
    catchPower str;
    catchBracket str;
    catchX str


let nblen str ns =
    let rec loop n = 
        if n = String.length str then n - ns
        else match (isNumber str.[n], str.[n]) with
        | (true, _) -> loop (n + 1)
        | (false, '.') -> loop (n + 1)
        | (false, 'x') | (false, 'X') -> loop (n + 1)
        | (false, _) -> n - ns
    in loop ns

let createNb str ns =
    let rec loop n =
        if n = String.length str then new Token.token (float_of_string (String.sub str ns (n - ns))) 0 'X'
        else match (isNumber str.[n], str.[n]) with
        | (true, _) -> loop (n + 1)
        | (false, '.') -> loop (n + 1)
        | (false, 'x') | (false, 'X') ->
            if n = ns then new Token.token 1. 1 'X'
            else new Token.token (float_of_string (String.sub str ns (n - ns))) 1 'X'
        | (false, _) -> new Token.token (float_of_string (String.sub str ns (n - ns))) 0 'X'
    in loop ns

(* DEVELOPPEMENT *)
(*    nb  X   sign    *)
(* [[(u8, u8, char)]] *)
let inverse op = match op with
    | '-' -> '+'
    | '+' -> '-'
    | _ -> op

let transformMe str =
    let rec loop n par equ =
        if n >= String.length str then []
        else match (isNumber str.[n], isOp str.[n], str.[n]) with
            | (true, _, _) -> [createNb str n] @ loop (n + nblen str n) par equ
            | (false, _, 'X') | (false, _, 'x') -> [createNb str n] @ loop (n + nblen str n) par equ
            | (_, true, '=') -> [new Token.token 0. 0 '-'] @ loop (n + 1) par true
            | (_, true, _) -> (
                if par = 0 && equ = true then [new Token.token 0. 0 (inverse str.[n])] @ loop (n + 1) par equ
                else [new Token.token 0. 0 str.[n]] @ loop (n + 1) par equ)
            | (_, false, '(') -> [new Token.token 0. 0 str.[n]] @ loop (n + 1) (par + 1) equ
            | (_, false, ')') -> [new Token.token 0. 0 str.[n]] @ loop (n + 1) (par - 1) equ
            | (false, _, _) -> loop (n + 1) par equ
    in loop 0 0 false

let rec delNb str =
    let rec loop n =
        if n = String.length str then []
        else match (isOp str.[n], str.[n]) with
            | _ -> loop (n + 1)
    in loop 0

let delLast lst =
    List.rev (List.tl (List.rev lst))

let rec print_list lst = match lst with
    | [] -> ()
    | (x::xs) -> (x#display; print_list xs)

let poland lst =
    let rec loop tail op = match tail with
            | [] -> List.rev op
            | (x::xs) -> (match x#getOp with
                | 'X' -> [x] @ loop xs op
                | '(' | '^' -> loop xs (op @ [x])
                | ')' -> (if List.length op = 0 then loop xs []
                        else ( let last = List.nth op (List.length op - 1) in
                        if last#getOp <> '(' then [last] @ loop ([x] @ xs) (delLast op)
                        else loop xs (delLast op)))
                | _ -> ( if List.length op = 0 then loop xs [x]
                        else ( let last = List.nth op (List.length op - 1) in
                        if x#getPrecedence <= last#getPrecedence then [last] @ loop ([x] @ xs) (delLast op)
                            else loop xs (op @ [x]))))
        in loop lst []

let isCompatible x1 x2 op =
    if x1#getOp = 'X' && x2#getOp = 'X' && op#getOp <> 'X' then (match op#getOp with
                | '*' | '^' | '/' -> true
                | '+' | '-' -> (if x1#getExpo = x2#getExpo || x1#getNb = 0. || x2#getNb = 0. then true else false)
                | _ -> false
                )
    else false

let makeOp lst n =
    let x1 = List.nth lst n in
    let x2 = List.nth lst (n + 1) in
    let op = List.nth lst (n + 2) in
    let ret = match op#getOp with
        | '+' -> x1#add x2
        | '-' -> x1#sub x2
        | '*' -> x1#mult x2
        | '/' -> x1#div x2
        | '^' -> x1#pow x2
        | _ -> x1#add x2
    in
    printf "PRINT:";
    ret#display;
    printf "\n";
    let rec loop tail n1 = match (tail, n1) with
        | ([], _) -> []
        | ((x::xs), 0) | ((x::xs), -1) -> loop xs (n1 - 1)
        | ((x::xs), -2) -> [ret] @ xs
        | ((x::xs), _) -> [x] @ loop xs (n1 - 1)
    in loop lst n

let rec reduce lst =
    print_list lst; print_char '\n';
    let rec loop tail n = match tail with
        | [] -> lst
        | (x::x1::x2::xs) -> (
            if isCompatible x x1 x2 = true then reduce (makeOp lst n)
            else loop ([x1] @ [x2] @ xs) (n + 1))
        | _ -> lst
    in loop lst 0

let makeMeSimple str =
    let lst = reduce (poland (transformMe str)) in
    let rec loop tail = match tail with
            | [] -> ()
            | (x::xs) -> (x#display; loop xs)
    in loop lst
    (*
    addMe str;
    subMe str;*)

let devIt lst =
    let rec loop tail =
        match tail with
            | [] -> ()
            | (x::xs) -> (makeMeSimple x; loop xs)
    in loop lst

let createList str =
    catchError str;
    makeMeSimple str
    (*List.iter print_endline lst;
    devIt lst*)



let () =
    if Array.length Sys.argv <> 2 then (print_endline "Wrong nb of Arguments"; exit 1)
    else createList Sys.argv.(1)
