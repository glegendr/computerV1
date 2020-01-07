open Printf

let error str =
    print_endline ("\027[31mCalc Error:\027[0m " ^ str); exit 1

let rec pow nb pw =
    if pw = 1 then nb
    else if pw = 0 then 1.
    else if pw > 99999 then error "Power to big"
    else if pw < 0 then nb *. (pow nb (pw + 1))
    else nb *. (pow nb (pw - 1))

class token nb expo op =
    object (self)
        val _nb:float = nb
        val _expo:int = if nb = 0. then 0 else expo

        val _op:char = op
        val _precedence:int = match op with
            | '^' -> 4
            | '*' | '/' -> 3
            | '+' | '-' -> 2
            | _ -> 1
        val _associativity:bool = match op with
            | '^' -> false
            | _ -> true

        method getNb = _nb
        method getExpo = _expo

        method getOp = _op
        method getPrecedence = _precedence
        method getAssociativity = _associativity
        method noEx = new token _nb 0 _op

        method add (number:token) =
            if _nb = 0. then new token number#getNb number#getExpo _op
            else if number#getNb = 0. then new token _nb _expo _op
            else if _nb +. number#getNb = 0. then new token 0. 0 _op
            else new token (_nb +. number#getNb) _expo _op
        method sub (number:token) =
            if _nb = 0. then new token (-.number#getNb) number#getExpo _op
            else if number#getNb = 0. then new token _nb _expo _op
            else if _nb -. number#getNb = 0. then new token 0. 0 _op
            else new token (_nb -. number#getNb) _expo _op
        method mult (number:token) =
            if _nb *. number#getNb = 0. then new token 0. 0 _op
            else new token (_nb *. number#getNb) (_expo + number#getExpo) _op
        method div (number:token) =
            if number#getNb = 0. then error "Divition per 0"
            else if _nb = infinity && number#getNb = infinity then error "Infinity divided by infinity"
            else if _nb /. number#getNb = 0. then new token 0. 0 _op
            else new token (_nb /. number#getNb) (_expo - number#getExpo) _op
        method pow (number:token) =
            if _expo = 0 && number#getNb > 0. then new token (pow _nb (int_of_float number#getNb)) _expo _op
            else if _expo = 0 then new token (1. /. (pow _nb (int_of_float number#getNb))) _expo _op
            else new token _nb (_expo * (int_of_float number#getNb)) _op

        method calc (number:token) (op:token) =
            match op#getOp with
                | '+' -> self#add number
                | '-' -> self#sub number
                | '*' -> self#mult number
                | '/' -> self#div number
                | '^' -> self#pow number
                | _ -> self#add number

        method display = match _op with
            | 'X' -> (
                print_float nb;
                if expo <> 0 then (print_string ("X^" ^ string_of_int expo ^ " "))
                else print_char ' ' )
            | _ -> (print_char _op; print_char ' ');
    end
