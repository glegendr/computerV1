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

        method add (number:token) = {<_nb = _nb +. number#getNb; _expo = _expo; _op = _op; _precedence = _precedence; _associativity = _associativity>}
        method sub (number:token) = {<_nb = _nb -. number#getNb; _expo = _expo; _op = _op; _precedence = _precedence; _associativity = _associativity>}
        method mult (number:token) =
            if number#getExpo = _expo then {<_nb = _nb *. number#getNb; _expo = _expo * 2; _op = _op; _precedence = _precedence; _associativity = _associativity>}
            else {<_nb = _nb *. number#getNb; _expo = max _expo number#getExpo; _op = _op; _precedence = _precedence; _associativity = _associativity>}

        method display = match _op with
            | 'X' -> (
                print_float nb;
                if expo > 0 then (print_endline ("X^" ^ string_of_int expo))
                else print_char '\n' )
            | _ -> (
                print_char _op;
                print_string (" " ^ string_of_int _precedence ^ " ");
                if _associativity = true then print_endline "Left"
                else print_endline "Right" )
    end
