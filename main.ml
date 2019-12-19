open Printf

let isNumber = function '0' .. '9' -> true | _ -> false
let isOp = function '*' | '+' | '-' | '=' | '^' | '/' -> true | _ -> false

let parseError str nd err =
    print_string "\027[31mSyntax Error:\027[0m ";
    print_endline (match (str.[nd], isNumber str.[nd], err) with
        | ('(', _, "") | (')', _, "") -> "bracket might be missmatched"
        | ('+', _, "") | ('-', _, "") | ('=', _, "") | ('*', _, "") | ('^', _, "") | ('/', _, "")-> "unexpected token"
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
                else if str.[n] = '(' && op = false && (op <> false && nb <> false && x <> false) then parseError str n ""
                else loop (n + 1) str.[n] op nb x)
        | 4 -> parseError str n ""
        | 3 -> loop (n + 1) str.[n] op nb x
        | 2 -> if op = true || (op = false && nb = false && x = false) then parseError str n "" else loop (n + 1) str.[n] true false false
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

let befBracket str n c =
    let rec loop n1 br oldop =
        if n1 <= 0 then false
        else match str.[n1] with
            | ')' -> loop (n1 - 1) (br - 1) oldop
            | '(' -> loop (n1 - 1) (br + 1) oldop
            | '+' | '-' | '*' | '/' | '^' | '=' ->
                    if str.[n1] = c && (br > 0 || oldop = true) then true
                    else if str.[n1] = c then loop (n1 - 1) (br - 1) true
                    else if br > 0 then loop (n1 - 1) (br - 1) false
                    else loop (n1 - 1) br false
            | _ -> loop (n1 - 1) br oldop
   in loop n 0 false

let catchPower str =
    let rec loop n =
        if n = String.length str then ()
        else match str.[n] with
            | '^' -> if befBracket str n '^' = true then parseError str n "multiple power" else loop (n + 1)
            | 'X' | 'x' -> if befBracket str n '^' = true then parseError str n "X in expodential" else loop (n + 1)
            | _ ->  loop (n + 1)
    in loop 0

let catchDiv str =
    let rec loop n =
        if n = String.length str then ()
        else match str.[n] with
            | 'X' | 'x' -> if befBracket str n '/' = true then parseError str n "X in divition" else loop (n + 1)
            | _ ->  loop (n + 1)
    in loop 0

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
    catchDiv str;
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

let makeOp lst n n1 =
    let x1 = List.nth lst n in
    let x2 = List.nth lst n1 in
    let op = List.nth lst (n1 + 1) in
    let ret = x1#calc x2 op in
    printf "PRINT:";
    ret#display;
    printf "\n";
    let rec loop tail n2 = match tail with
        | [] -> []
        | (x::xs) -> (
            if n2 = n then [ret] @ loop xs (n2 + 1)
            else if n2 = n1 || n2 = (n1 + 1) then loop xs (n2 + 1)
            else [x] @ loop xs (n2 + 1)
            )
    in loop lst 0

let rec reduce lst =
    print_list lst; print_char '\n';
    let rec loop tail n = match tail with
        | [] -> lst
        | (x::x1::x2::x3::x4::xs) -> (
            if isCompatible x x1 x2 = true then reduce (makeOp lst n (n + 1))
            else if isCompatible x x2 x3 = true && x1#getPrecedence = x3#getPrecedence then reduce (makeOp lst n (n + 2))
            else if x2#getPrecedence = x4#getPrecedence && isCompatible x x3 x4 = true && x1#getOp = 'X' then reduce (makeOp lst n (n + 3))
            else if x2#getPrecedence = x4#getPrecedence && isCompatible x1 x3 x4 = true && x#getOp = 'X' then reduce (makeOp lst (n + 1) (n + 3))
            else loop ([x1] @ [x2] @ [x3] @ [x4] @ xs) (n + 1))
        | (x::x1::x2::x3::xs) ->
            if isCompatible x x2 x3 = true && x1#getPrecedence = x3#getPrecedence then reduce (makeOp lst n (n + 2))
            else if isCompatible x x1 x2 = true then reduce (makeOp lst n (n + 1))
            else loop ([x1] @ [x2] @ [x3] @ xs) (n + 1)
        | (x::x1::x2::xs) -> (
            if isCompatible x x1 x2 = true then reduce (makeOp lst n (n + 1))
            else loop ([x1] @ [x2] @ xs) (n + 1))
        | _ -> lst
    in loop lst 0

(*IDENTITY*)

let identityOneTwo lst n =
    let rec loop tail n1 = match tail with
        | [] -> []
        | (x::x1::x2::x3::x4::xs) ->
            if n1 = n then [x#calc x1 x4] @ [x#calc x2 x4] @ [x3] @ xs
            else [x] @ loop ([x1] @ [x2] @ [x3] @ [x4] @ xs) (n1 + 1)
        | (x::xs) -> [x] @ loop xs (n1 + 1)
    in loop lst 0

let identityTwoOne lst n =
    let rec loop tail n1 = match tail with
        | [] -> []
        | (x::x1::x2::x3::x4::xs) ->
            if n1 = n then [x3#calc x x4] @ [x3#calc x1 x4] @ [x2] @ xs
            else [x] @ loop ([x1] @ [x2] @ [x3] @ [x4] @ xs) (n1 + 1)
        | (x::xs) -> [x] @ loop xs (n1 + 1)
    in loop lst 0

let identityOneThree lst n =
    let rec loop tail n1 = match tail with
        | [] -> []
        | (x::x1::x2::x3::x4::x5::x6::xs) ->
            if n1 = n then [x#calc x1 x6] @ [x#calc x2 x6] @ [x3] @ [x#calc x4 x6] @ [x5] @ xs
            else [x] @ loop ([x1] @ [x2] @ [x3] @ [x4] @ [x5] @ [x6] @ xs) (n1 + 1)
        | (x::xs) -> [x] @ loop xs (n1 + 1)
    in loop lst 0

let identityThreeOne lst n =
    let rec loop tail n1 = match tail with
        | [] -> []
        | (x::x1::x2::x3::x4::x5::x6::xs) ->
            if n1 = n then [x5#calc x x6] @ [x5#calc x1 x6] @ [x2] @ [x5#calc x3 x6] @ [x4] @ xs
            else [x] @ loop ([x1] @ [x2] @ [x3] @ [x4] @ [x5] @ [x6] @ xs) (n1 + 1)
        | (x::xs) -> [x] @ loop xs (n1 + 1)
    in loop lst 0

let identityTwoTwo lst n =
    let rec loop tail n1 = match tail with
        | [] -> []
        | (x::x1::x2::x3::x4::x5::x6::xs) ->
                if n1 = n then (
                    let op = match (x2#getOp, x5#getOp) with
                        | ('-', '-') -> new Token.token 0. 0 '+'
                        | ('-', '+') | ('+', '-') -> new Token.token 0. 0 '-'
                        | _ -> x2
                    in
                    [x#calc x3 x6] @ [x#calc x4 x6] @ [x5] @ [x1#calc x3 x6] @ [x2] @[x1#calc x4 x6] @ [op] @ xs)
            else [x] @ loop ([x1] @ [x2] @ [x3] @ [x4] @ [x5] @ [x6] @ xs) (n1 + 1)
        | (x::xs) -> [x] @ loop xs (n1 + 1)
    in loop lst 0


(* 1-2 2-1 / 1-3 2-2 3-1 / 2-3 3-2 / 3-3 *)
let rec identity lst =
    print_list lst; print_char '\n';
    let rec loop tail n = match tail with
        | [] -> []
        | (x::x1::x2::x3::x4::x5::x6::xs) ->
                if x#getOp = 'X' && x1#getOp = 'X' && x2#getOp = 'X' && x3#getOp <> 'X' && x4#getOp = 'X' && x5#getOp <> 'X' && (x6#getOp = '*' || x6#getOp = '/') then identity (reduce (identityOneThree lst n))
                else if x#getOp = 'X' && x1#getOp = 'X' && x2#getOp <> 'X' && x3#getOp = 'X' && x4#getOp <> 'X' && x5#getOp = 'X' && (x6#getOp = '*' || x6#getOp = '/') then identity (reduce (identityThreeOne lst n))
                else if x#getOp = 'X' && x1#getOp = 'X' && x2#getOp <> 'X' && x3#getOp = 'X' && x4#getOp = 'X' && x5#getOp <> 'X' && (x6#getOp = '*' || x6#getOp = '/') then identity (reduce (identityTwoTwo lst n))
                else loop ([x1] @ [x2] @ [x3] @ [x4] @ [x5] @ [x6] @ xs) (n + 1)
        | (x::x1::x2::x3::x4::xs) ->
                if isCompatible x1 x2 x3 = false && (x4#getOp = '*' || x4#getOp = '/') && x#getOp = 'X' && x1#getOp = 'X' && x2#getOp = 'X' && x3#getOp <> 'X' then identity (reduce (identityOneTwo lst n))
                else if isCompatible x x1 x2 = false && (x4#getOp = '*' || x4#getOp = '/') && x#getOp = 'X' && x1#getOp = 'X' && x2#getOp <> 'X' && x3#getOp = 'X' then identity (reduce (identityTwoOne lst n))
                else loop ([x1] @ [x2] @ [x3] @ [x4] @ xs) (n + 1)
        | (x::xs) -> loop xs (n + 1)
    in loop lst 0

let makeMeSimple str =
    let lst = identity (reduce (poland (transformMe str))) in
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
