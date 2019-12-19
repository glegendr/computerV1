open Printf

let rec pow nb pw =
    if pw = 1 then nb
    else if pw = 0 then 1.
    else if pw > 99999 then (print_endline "\027[31mCalc Error:\027[0m Power to big"; exit 1)
    else nb *. (pow nb (pw - 1))

class token nb expo op =
    object (self)
        val _nb:float = nb
        val _expo:int = expo

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

        method add (number:token) =
            if _nb +. number#getNb = 0. then new token 0. 0 _op
            else new token (_nb +. number#getNb) _expo _op
        method sub (number:token) =
            if _nb -. number#getNb = 0. then new token 0. 0 _op
            else new token (_nb -. number#getNb) _expo _op
        method mult (number:token) =
            if _nb *. number#getNb = 0. then new token 0. 0 _op
            else new token (_nb *. number#getNb) (_expo + number#getExpo) _op
        method div (number:token) =
            if number#getNb = 0. then (print_endline "\027[31mCalc Error:\027[0m Divition per 0"; exit 1)
            else if _nb /. number#getNb = 0. then new token 0. 0 _op
            else new token (_nb /. number#getNb) (_expo - number#getExpo) _op
        method pow (number:token) =
            if _expo = 0 then new token (pow _nb (int_of_float number#getNb)) _expo _op
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
                if expo > 0 then (print_string ("X^" ^ string_of_int expo ^ " "))
                else print_char ' ' )
            | _ -> (print_char _op; print_char ' ');
    end
