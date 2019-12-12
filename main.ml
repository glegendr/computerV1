let isNumber = function '0' .. '9' -> true | _ -> false
let isOp = function '*' | '+' | '-' | '=' | '^' -> true | _ -> false

let parseError str nd =
    print_string "\027[31mSyntax Error:\027[0m ";
    print_endline (match (str.[nd], isNumber str.[nd]) with
        | ('(', _) | (')', _) -> "bracket might be missmatched"
        | ('+', _) | ('-', _) | ('=', _) | ('*', _) | ('^', _) -> "unexpected token"
        | (' ', false) -> "two space in a row"
        | (_, false) -> "unexpected character"
        | (_, true) -> "unexpected token");
    print_endline str;
    let rec loop n =
        if n < 0 then ""
        else " " ^ loop (n - 1)
    in
    print_endline ((loop (nd - 1)) ^ "^");
    exit 0

let split str =
    let rec loop str st nd par_nb =
        if par_nb < 0 then parseError str (nd - 1)
        else if nd = String.length str && par_nb <> 0 then parseError str (String.rindex str '(')
        else if nd = String.length str then [String.sub str st (nd - st)]
        else match (str.[nd], par_nb)  with
            | ('(', _) -> loop str st (nd + 1) (par_nb + 1)
            | (')', _) -> loop str st (nd + 1) (par_nb - 1)
            | ('+', 0) | ('-', 0) | ('=', 0) -> [String.sub str st (nd - st)] @ loop str nd (nd  + 1) par_nb
            | (_, _ )-> loop str st (nd + 1) par_nb
    in loop str 0 0 0


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
        if n = String.length str && op = true then parseError str (n - 1)
        else if n = String.length str then ()
        else if n1 = '.' && isNumber str.[n] = false then parseError str n
        else if str.[n] = n1 && typeMe str.[n] <> 5 && isNumber str.[n] = false then parseError str n
        else match typeMe str.[n] with
        | 6 -> if x = true then parseError str n else loop (n + 1) str.[n] false false true
        | 5 -> (if str.[n] = ')' && op = true then parseError str n
                else if str.[n] = '(' && op = false then parseError str n
                else loop (n + 1) str.[n] op nb x)
        | 4 -> parseError str n
        | 3 -> loop (n + 1) str.[n] op nb x
        | 2 -> if op = true then parseError str n else loop (n + 1) str.[n] true false false
        | 1 -> if nb = true && typeMe n1 <> 1 then parseError str n else loop (n + 1) str.[n] false true false
        | _ -> parseError str n 
    in loop 0 'W' false false false

let catchEq str =
    let rec loop n neq =
        if n = String.length str then ()
        else match (str.[n], neq) with
            | ('=', false) -> loop (n + 1) true
            | ('=', true) -> parseError str n
            | _ -> loop (n + 1) neq
    in loop 0 false

let catchPower str =
    let rec loop n pow =
        if n = String.length str then ()
        else match str.[n] with
            | '^' -> if pow = true then parseError str n else loop (n + 1) true
            | '*' | '+' | '-' | '=' -> loop (n + 1) false
            | _ -> loop (n + 1) pow
    in loop 0 false

let catchX str =
    let rec loop n =
        if n = String.length str then (print_endline ("\027[31mCalc Error:\027[0m no X founded\n" ^ str); exit 0)
        else match str.[n] with
            | 'X' | 'x' -> ()
            | _ -> loop (n + 1)
    in loop 0

let catchError str =
    catchSyntaxError str;
    catchEq str;
    catchPower str;
    catchX str

let createList str =
    catchError str;
    let lst = split str in
    List.iter print_endline lst



let () =
    if Array.length Sys.argv <> 2 then print_endline "Wrong nb of Arguments"
    else createList Sys.argv.(1)
