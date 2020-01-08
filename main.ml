let isNumber = function '0' .. '9' -> true | _ -> false
let isOp = function '*' | '+' | '-' | '=' | '^' | '/' -> true | _ -> false

let error str =
    print_endline ("\027[31mCalc Error:\027[0m " ^ str); exit 1

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

let befMe str n =
    let rec loop n1 oldMe =
        if n1 < 0 || n1 >= String.length str || n1 = n then oldMe
        else match str.[n1] with
        | ' ' -> loop (n1 + 1) oldMe
        | _ -> loop (n1 + 1) str.[n1]
    in loop 0 ')'

let catchPower str =
    let rec loop n =
        if n = String.length str then ()
        else match str.[n] with
            | '^' ->
                    if befBracket str n '^' = true then parseError str n "multiple power"
                    else if befMe str n = ')' then parseError str n "power on braket"
                    else loop (n + 1)
            | 'X' | 'x' -> if befBracket str n '^' = true then parseError str n "X in expodential" else loop (n + 1)
            | _ ->  loop (n + 1)
    in loop 0

let catchX str =
    let rec loop n =
        if n = String.length str then error ("no X founded\n" ^ str)
        else match str.[n] with
            | 'X' | 'x' -> ()
            | _ -> loop (n + 1)
    in loop 0

let catchBracket str fg =
    let rec loop n br eq =
        if br < 0 then parseError str (n - 1) ""
        else if n >= String.length str && br > 0 then parseError str (n - 1) "Non terminated bracket"
        else if n >= String.length str then ()
        else match str.[n] with
        | '(' ->
                if fg land 1 = 0 then error "Bracket not handled"
                else if eq = true then parseError str n "Bracket in right part of equation will always fail"
                else loop (n + 1) (br + 1) eq
        | ')' -> loop (n + 1) (br - 1) eq
        | '=' -> if br > 0 then parseError str (n - 1) "" else loop (n + 1) br true
        | _ -> loop (n + 1) br eq
    in loop 0 0 false

let catchError str flags =
    catchSyntaxError str;
    catchEq str;
    catchPower str;
    catchBracket str flags;
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


let rec print_list_no_nd lst flags =
    if flags land 4 = 0 then ()
    else match lst with
    | [] -> ()
    | (x::xs) -> (x#display; print_list_no_nd xs flags)

let print_list lst flags =
    if flags land 4 = 0 then ()
    else (
        print_list_no_nd lst flags;
        print_char '\n')

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
    let rec loop tail n2 = match tail with
        | [] -> []
        | (x::xs) -> (
            if n2 = n then [ret] @ loop xs (n2 + 1)
            else if n2 = n1 || n2 = (n1 + 1) then loop xs (n2 + 1)
            else [x] @ loop xs (n2 + 1)
            )
    in loop lst 0

let rec replaceLess lst =
    let rec loop tail = match tail with
        | [] -> []
        | (x::x1::xs) ->
                if x#getOp = 'X' && x1#getOp = '-' then replaceLess ([new Token.token (-.x#getNb) x#getExpo 'X'] @ [new Token.token 0. 0 '+'] @ xs)
                else [x] @ loop ([x1] @ xs)
        | (x::xs) -> [x] @ loop xs
    in loop lst

let rec reduce aft flags =
    let lst = replaceLess aft in
    print_list lst flags;
    let rec loop tail n = match tail with
        | [] -> lst
        | (x::x1::x2::x3::x4::xs) -> (
            if isCompatible x x1 x2 = true then reduce (makeOp lst n (n + 1)) flags
            else if isCompatible x x2 x3 = true && x1#getPrecedence = x3#getPrecedence then reduce (makeOp lst n (n + 2)) flags
            else if x2#getPrecedence = x4#getPrecedence && isCompatible x x3 x4 = true && x1#getOp = 'X' then reduce (makeOp lst n (n + 3)) flags
            else if x2#getPrecedence = x4#getPrecedence && isCompatible x1 x3 x4 = true && x#getOp = 'X' then reduce (makeOp lst (n + 1) (n + 3)) flags
            else loop ([x1] @ [x2] @ [x3] @ [x4] @ xs) (n + 1))
        | (x::x1::x2::x3::xs) ->
            if isCompatible x x2 x3 = true && x1#getPrecedence = x3#getPrecedence then reduce (makeOp lst n (n + 2)) flags
            else if isCompatible x x1 x2 = true then reduce (makeOp lst n (n + 1)) flags
            else loop ([x1] @ [x2] @ [x3] @ xs) (n + 1)
        | (x::x1::x2::xs) -> (
            if isCompatible x x1 x2 = true then reduce (makeOp lst n (n + 1)) flags
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

let identityTwoThree lst n =
    let rec loop tail n1 = match tail with
        | [] -> []
        | (x::x1::x2::x3::x4::x5::x6::x7::x8::xs) ->
                if n1 = n then (
                    let op = match (x2#getOp, x5#getOp) with
                        | ('-', '-') -> new Token.token 0. 0 '+'
                        | ('-', '+') | ('+', '-') -> new Token.token 0. 0 '-'
                        | _ -> x2
                    in
                    let op1 = match (x2#getOp, x7#getOp) with
                        | ('-', '-') -> new Token.token 0. 0 '+'
                        | ('-', '+') | ('+', '-') -> new Token.token 0. 0 '-'
                        | _ -> x2
                    in
                    [x#calc x3 x8] @ [x#calc x4 x8] @ [x5] @ [x#calc x6 x8] @ [x7] @ [x1#calc x3 x8] @ [x2] @[x1#calc x4 x8] @ [op] @ [x1#calc x6 x8] @ [op1] @ xs)
                else [x] @ loop ([x1] @ [x2] @ [x3] @ [x4] @ [x5] @ [x6] @ [x7] @ [x8] @ xs) (n1 + 1)
        | (x::xs) -> [x] @ loop xs (n1 + 1)
    in loop lst 0

let identityThreeTwo lst n =
    let rec loop tail n1 = match tail with
        | [] -> []
        | (x::x1::x2::x3::x4::x5::x6::x7::x8::xs) ->
                if n1 = n then (
                    let op = match (x7#getOp, x2#getOp) with
                        | ('-', '-') -> new Token.token 0. 0 '+'
                        | ('-', '+') | ('+', '-') -> new Token.token 0. 0 '-'
                        | _ -> x2
                    in
                    let op1 = match (x7#getOp, x4#getOp) with
                        | ('-', '-') -> new Token.token 0. 0 '+'
                        | ('-', '+') | ('+', '-') -> new Token.token 0. 0 '-'
                        | _ -> x2
                    in
                    [x5#calc x x8] @ [x5#calc x1 x8] @ [x2] @ [x5#calc x3 x8] @ [x4] @ [x6#calc x x8] @ [x7] @[x6#calc x1 x8] @ [op] @ [x6#calc x3 x8] @ [op1] @ xs)
                else [x] @ loop ([x1] @ [x2] @ [x3] @ [x4] @ [x5] @ [x6] @ [x7] @ [x8] @ xs) (n1 + 1)
        | (x::xs) -> [x] @ loop xs (n1 + 1)
    in loop lst 0

let identityThreeThree lst n =
    let rec loop tail n1 = match tail with
        | [] -> []
        | (x::x1::x2::x3::x4::x5::x6::x7::x8::x9::x10::xs) ->
                if n1 = n then (
                    let op = match (x2#getOp, x7#getOp) with
                        | ('-', '-') -> new Token.token 0. 0 '+'
                        | ('-', '+') | ('+', '-') -> new Token.token 0. 0 '-'
                        | _ -> x2
                    in
                    let op1 = match (x2#getOp, x9#getOp) with
                        | ('-', '-') -> new Token.token 0. 0 '+'
                        | ('-', '+') | ('+', '-') -> new Token.token 0. 0 '-'
                        | _ -> x2
                    in
                    let op2 = match (x4#getOp, x7#getOp) with
                        | ('-', '-') -> new Token.token 0. 0 '+'
                        | ('-', '+') | ('+', '-') -> new Token.token 0. 0 '-'
                        | _ -> x2
                    in
                    let op3 = match (x4#getOp, x9#getOp) with
                        | ('-', '-') -> new Token.token 0. 0 '+'
                        | ('-', '+') | ('+', '-') -> new Token.token 0. 0 '-'
                        | _ -> x2
                    in
                    [x#calc x5 x10] @ [x#calc x6 x10] @ [x7] @ [x#calc x8 x10] @ [x9] @
                    [x1#calc x5 x10] @ [x2] @ [x1#calc x6 x10] @ [op] @ [x1#calc x8 x10] @ [op1] @
                    [x3#calc x5 x10] @ [x4] @ [x3#calc x6 x10] @ [op2] @ [x3#calc x8 x10] @ [op3] @ xs)
                else [x] @ loop ([x1] @ [x2] @ [x3] @ [x4] @ [x5] @ [x6] @ [x7] @ [x8] @ xs) (n1 + 1)
        | (x::xs) -> [x] @ loop xs (n1 + 1)
    in loop lst 0

(* 1-2 2-1 / 1-3 2-2 3-1 / 2-3 3-2 / 3-3 *)
let rec identity lst flags =
    if flags land 1 = 0 then lst
    else (
    print_list lst flags;
    let rec loop tail n = match tail with
        | [] -> lst
        | (x::x1::x2::x3::x4::x5::x6::x7::x8::x9::x10::xs) ->
                if x#getOp = 'X' && x1#getOp = 'X' && x2#getOp <> 'X' && x3#getOp = 'X' && x4#getOp <> 'X' && x5#getOp = 'X' && x6#getOp = 'X' && x7#getOp <> 'X' && x8#getOp = 'X' && x9#getOp <> 'X' && (x10#getOp = '*' || x10#getOp = '/') then identity (reduce (identityThreeThree lst n) flags) flags
                else if x#getOp = 'X' && x1#getOp = 'X' && x2#getOp <> 'X' && x3#getOp = 'X' && x4#getOp = 'X' && x5#getOp <> 'X' && x6#getOp = 'X' && x7#getOp <> 'X' && (x8#getOp = '*' || x8#getOp = '/') then identity (reduce (identityTwoThree lst n) flags) flags
                else if x#getOp = 'X' && x1#getOp = 'X' && x2#getOp <> 'X' && x3#getOp = 'X' && x4#getOp <> 'X' && x5#getOp = 'X' && x6#getOp = 'X' && x7#getOp <> 'X' && (x8#getOp = '*' || x8#getOp = '/') then identity (reduce (identityThreeTwo lst n) flags) flags
                else if x#getOp = 'X' && x1#getOp = 'X' && x2#getOp = 'X' && x3#getOp <> 'X' && x4#getOp = 'X' && x5#getOp <> 'X' && (x6#getOp = '*' || x6#getOp = '/') then identity (reduce (identityOneThree lst n) flags) flags
                else if x#getOp = 'X' && x1#getOp = 'X' && x2#getOp <> 'X' && x3#getOp = 'X' && x4#getOp <> 'X' && x5#getOp = 'X' && (x6#getOp = '*' || x6#getOp = '/') then identity (reduce (identityThreeOne lst n) flags) flags
                else if x#getOp = 'X' && x1#getOp = 'X' && x2#getOp <> 'X' && x3#getOp = 'X' && x4#getOp = 'X' && x5#getOp <> 'X' && (x6#getOp = '*' || x6#getOp = '/') then identity (reduce (identityTwoTwo lst n) flags) flags
                else if isCompatible x1 x2 x3 = false && (x4#getOp = '*' || x4#getOp = '/') && x#getOp = 'X' && x1#getOp = 'X' && x2#getOp = 'X' && x3#getOp <> 'X' then identity (reduce (identityOneTwo lst n) flags) flags
                else if isCompatible x x1 x2 = false && (x4#getOp = '*' || x4#getOp = '/') && x#getOp = 'X' && x1#getOp = 'X' && x2#getOp <> 'X' && x3#getOp = 'X' then identity (reduce (identityTwoOne lst n) flags) flags
                else loop ([x1] @ [x2] @ [x3] @ [x4] @ [x5] @ [x6] @ [x7] @ [x8] @ [x9] @ [x10] @ xs) (n + 1)
        | (x::x1::x2::x3::x4::x5::x6::x7::x8::xs) ->
                if x#getOp = 'X' && x1#getOp = 'X' && x2#getOp <> 'X' && x3#getOp = 'X' && x4#getOp = 'X' && x5#getOp <> 'X' && x6#getOp = 'X' && x7#getOp <> 'X' && (x8#getOp = '*' || x8#getOp = '/') then identity (reduce (identityTwoThree lst n) flags) flags
                else if x#getOp = 'X' && x1#getOp = 'X' && x2#getOp <> 'X' && x3#getOp = 'X' && x4#getOp <> 'X' && x5#getOp = 'X' && x6#getOp = 'X' && x7#getOp <> 'X' && (x8#getOp = '*' || x8#getOp = '/') then identity (reduce (identityThreeTwo lst n) flags) flags
                else if x#getOp = 'X' && x1#getOp = 'X' && x2#getOp = 'X' && x3#getOp <> 'X' && x4#getOp = 'X' && x5#getOp <> 'X' && (x6#getOp = '*' || x6#getOp = '/') then identity (reduce (identityOneThree lst n) flags) flags
                else if x#getOp = 'X' && x1#getOp = 'X' && x2#getOp <> 'X' && x3#getOp = 'X' && x4#getOp <> 'X' && x5#getOp = 'X' && (x6#getOp = '*' || x6#getOp = '/') then identity (reduce (identityThreeOne lst n) flags) flags
                else if x#getOp = 'X' && x1#getOp = 'X' && x2#getOp <> 'X' && x3#getOp = 'X' && x4#getOp = 'X' && x5#getOp <> 'X' && (x6#getOp = '*' || x6#getOp = '/') then identity (reduce (identityTwoTwo lst n) flags) flags
                else if isCompatible x1 x2 x3 = false && (x4#getOp = '*' || x4#getOp = '/') && x#getOp = 'X' && x1#getOp = 'X' && x2#getOp = 'X' && x3#getOp <> 'X' then identity (reduce (identityOneTwo lst n) flags) flags
                else if isCompatible x x1 x2 = false && (x4#getOp = '*' || x4#getOp = '/') && x#getOp = 'X' && x1#getOp = 'X' && x2#getOp <> 'X' && x3#getOp = 'X' then identity (reduce (identityTwoOne lst n) flags) flags
                else loop ([x1] @ [x2] @ [x3] @ [x4] @ [x5] @ [x6] @ [x7] @ [x8] @ xs) (n + 1)
        | (x::x1::x2::x3::x4::x5::x6::xs) ->
                if x#getOp = 'X' && x1#getOp = 'X' && x2#getOp = 'X' && x3#getOp <> 'X' && x4#getOp = 'X' && x5#getOp <> 'X' && (x6#getOp = '*' || x6#getOp = '/') then identity (reduce (identityOneThree lst n) flags) flags
                else if x#getOp = 'X' && x1#getOp = 'X' && x2#getOp <> 'X' && x3#getOp = 'X' && x4#getOp <> 'X' && x5#getOp = 'X' && (x6#getOp = '*' || x6#getOp = '/') then identity (reduce (identityThreeOne lst n) flags) flags
                else if x#getOp = 'X' && x1#getOp = 'X' && x2#getOp <> 'X' && x3#getOp = 'X' && x4#getOp = 'X' && x5#getOp <> 'X' && (x6#getOp = '*' || x6#getOp = '/') then identity (reduce (identityTwoTwo lst n) flags) flags
                else if isCompatible x1 x2 x3 = false && (x4#getOp = '*' || x4#getOp = '/') && x#getOp = 'X' && x1#getOp = 'X' && x2#getOp = 'X' && x3#getOp <> 'X' then identity (reduce (identityOneTwo lst n) flags) flags
                else if isCompatible x x1 x2 = false && (x4#getOp = '*' || x4#getOp = '/') && x#getOp = 'X' && x1#getOp = 'X' && x2#getOp <> 'X' && x3#getOp = 'X' then identity (reduce (identityTwoOne lst n) flags) flags
                else loop ([x1] @ [x2] @ [x3] @ [x4] @ [x5] @ [x6] @ xs) (n + 1)
        | (x::x1::x2::x3::x4::xs) ->
                if isCompatible x1 x2 x3 = false && (x4#getOp = '*' || x4#getOp = '/') && x#getOp = 'X' && x1#getOp = 'X' && x2#getOp = 'X' && x3#getOp <> 'X' then identity (reduce (identityOneTwo lst n) flags) flags
                else if isCompatible x x1 x2 = false && (x4#getOp = '*' || x4#getOp = '/') && x#getOp = 'X' && x1#getOp = 'X' && x2#getOp <> 'X' && x3#getOp = 'X' then identity (reduce (identityTwoOne lst n) flags) flags
                else loop ([x1] @ [x2] @ [x3] @ [x4] @ xs) (n + 1)
        | (x::xs) -> loop xs (n + 1)
    in loop lst 0)

let rec isPrecedence lst = match lst with
    | [] -> false
    | (x::xs) -> if x#getPrecedence > 2 then true else isPrecedence xs

let rec reduceMeAgain lst flags =
    print_list lst flags;
    let rec loop st n tail = match tail with
    | [] -> [List.nth lst st] @ reduceMeAgain (List.tl lst) flags
    | (x::x1::xs) ->
            if (List.nth lst st)#getOp <> 'X' then [List.nth lst st] @ reduceMeAgain (List.tl lst) flags
            else if isCompatible (List.nth lst st) x x1 && n <> 0 then reduceMeAgain (makeOp lst st n) flags
            else loop st (n + 1) ([x1] @ xs)
    | (x::xs) ->
            if (List.nth lst st)#getOp <> 'X' then [List.nth lst st] @ reduceMeAgain (List.tl lst) flags
            else loop st (n + 1) xs
    in
    if List.length lst <= 0 then []
    else loop 0 0 lst

let makeMeSimple str flags =
    let lst = identity (reduce (poland (transformMe str)) flags) flags in
    let rec loop n lst2 =
        if n >= 3 then error "Unknown Error"
        else if isPrecedence lst2 = true then loop (n + 1) (identity (reduce lst2 flags) flags)
        else reduce (reduceMeAgain lst2 flags) flags
    in loop 0 lst

let getHelp _ =
    print_endline (Sys.argv.(0) ^ " \"equation\" [flags]");
    print_endline "Flags are:";
    print_endline "-b     Enable bracket  \027[31m/!\\\027[0m Warning this feature is not stable";
    print_endline "-p     Display Reverse Polish notation at the end";
    print_endline "-d     Display steps";
    print_endline "-h     Display this message";
    exit 0

let getFlags flgs =
    let rec loop n =
        if n >= String.length flgs && n = 1 then error ("Unknown flag " ^ (String.make 1 flgs.[0]))
        else if n >= String.length flgs then 0
        else match flgs.[n] with
            | 'b' -> (print_endline ("\027[33mWarning: The bracket feature can fail\027[0m "); 1 lor loop (n + 1))
            | 'p' -> 2 lor loop (n + 1)
            | 'd' -> 4 lor loop (n + 1)
            | 'h' -> getHelp()
            | '-' -> if n = 0 then loop (n + 1) else error ("Unknown flag " ^ (String.make 1 flgs.[n]))
            | _ -> error ("Unknown flag " ^ (String.make 1 flgs.[n]))
    in loop 0

let rec checkSecDeg lst deg = match lst with
    | [] -> deg
    | (x::xs) -> if x#getExpo > 2 || x#getExpo < 0 then (error ("The polynomial degree is " ^ (string_of_int x#getExpo) ^ " I can't solve")) else checkSecDeg xs (max deg x#getExpo)

let compareExpo x y =
    if x#getOp <> 'X' then -1
    else if y#getOp <> 'X' then 1
    else match (x#getExpo > y#getExpo, x#getExpo = y#getExpo) with
        | (true, false) -> 1
        | (false, false) -> -1
        | _ -> 0

let rec createReadableList lst =
    if List.length lst = 0 then []
    else [List.hd (List.rev lst)] @ createReadableList (List.tl (List.rev lst))

let really lst =
    let rec loop n =
        Unix.sleep 1;
        print_char '.';
        flush stdout;
        if n < 2 then loop (n + 1)
        else print_char '\n'
    in loop 0;
    print_list_no_nd lst 4;
    print_endline "= 0";
    loop 0;
    print_endline "Probably Right"

let mySqrt div =
    let rec loop pad n ab =
        if n *. n = div then n
        else if pad = 0. then n
        else if n *. n < div && ab = false then loop (pad *. 2.) (n +. pad) ab
        else if n *. n < div then loop (pad /. 2.) (n +. pad) ab
        else loop (pad /. 2.) (n -. pad) true
    in loop 1. 0. false

let calcDisc lst =
    let a = try (List.hd lst)#noEx with Failure _ -> new Token.token 0. 0 'X' in
    let b = try (List.nth lst 2)#noEx with Failure _ -> new Token.token 0. 0 'X' in
    let c = if b#getNb = 0. then try (List.nth lst 2)#noEx with Failure _ -> new Token.token 0. 0 'X'
        else try (List.nth lst 4)#noEx with Failure _ -> new Token.token 0. 0 'X' in
    let delt = (b#pow (new Token.token 2. 0 'X'))#getNb -. 4. *. a#getNb *. c#getNb in
    print_string ("Discriminant is " ^ (string_of_float delt));
    match (delt > 0., delt = 0.) with
        | (true, false) -> (print_endline " the two solutions are:";
            print_float ((-.b#getNb -. (mySqrt delt))/.(2. *. a#getNb)); print_char '\n';
            print_float ((-.b#getNb +. (mySqrt delt))/.(2. *. a#getNb)); print_char '\n')
        | (false, false) -> (print_endline " the two complex solution are:";
            print_endline ("(" ^ (string_of_float (-.b#getNb)) ^ " - i√" ^ string_of_float(-.delt) ^ ") / (2 * " ^ string_of_float a#getNb ^ ")");
            print_endline ("(" ^ (string_of_float (-.b#getNb)) ^ " + i√" ^ string_of_float(-.delt) ^ ") / (2 * " ^ string_of_float a#getNb ^ ")"))
        | _ -> (print_endline " the solution is:";
            print_float ((-.b#getNb)/.(2. *. a#getNb)); print_char '\n')

let calcX lst =
    let a = try (List.hd lst)#noEx with Failure _ -> new Token.token 0. 0 'X' in
    let b = try (List.nth lst 2)#noEx with Failure _ -> new Token.token 0. 0 'X' in
    let res = (-.b#getNb)/.a#getNb in
    print_endline ("x = " ^ (string_of_float res))

let secDeg lst flags =
    let newList = createReadableList (List.sort compareExpo lst) in
    print_string "Reduced form:            ";
    print_list_no_nd newList 4;
    print_endline "= 0";
    let deg =  checkSecDeg newList 0 in
    print_endline ("Polynomial degree:       " ^ string_of_int deg);
    match deg with
    | 2 -> calcDisc newList
    | 1 -> calcX newList
    | _ -> really newList

let createList str flgs =
    let flags = getFlags flgs in
    catchError str flags;
    let lst = makeMeSimple str flags in
    if flags land 2 = 2 then (
        print_string "Reverse Polish notation: ";
        print_list_no_nd lst 4; print_endline "= 0"; secDeg lst flags)
    else secDeg lst flags

let () =
    if Array.length Sys.argv > 3 || Array.length Sys.argv < 2 then error "Wrong nb of Arguments"
    else createList Sys.argv.(1) (try Sys.argv.(2) with Invalid_argument _ -> "")
