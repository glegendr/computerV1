open Printf

class token nb expo op =
    object
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

        method add (number:token) = new token (_nb +. number#getNb) _expo _op
        method sub (number:token) = new token (_nb -. number#getNb) _expo _op
        method mult (number:token) = new token (_nb *. number#getNb) (_expo + number#getExpo) _op
        method div (number:token) = new token (_nb /. number#getNb) (_expo - number#getExpo) _op
        method pow (number:token) = new token (_nb +. number#getNb) (_expo + int_of_float number#getNb) _op

        method display = match _op with
            | 'X' -> (
                print_float nb;
                if expo > 0 then (print_string ("X^" ^ string_of_int expo ^ " "))
                else print_char ' ' )
            | _ -> (
                print_char _op; print_char ' '(*;
                print_string (" " ^ string_of_int _precedence ^ " ");
                if _associativity = true then print_endline "Left"
                else print_endline "Right" *))
    end
