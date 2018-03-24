(* $Id: bigint.ml,v 1.5 2014-11-11 15:06:24-08 - - $ *)
(* Brittany Lei  bclei@ucsc.edu  asg2 *)

open Printf

module Bigint = struct

    type sign     = Pos | Neg
    type bigint   = Bigint of sign * int list
    type ord = GT | EQ | LT
    let  radix    = 10
    let  radixlen =  1

    let car       = List.hd
    let cdr       = List.tl
    let map       = List.map
    let reverse   = List.rev
    let length    = List.length
    let strcat    = String.concat
    let strlen    = String.length
    let strsub    = String.sub
    let zero      = Bigint (Pos, [])

    let charlist_of_string str =
        let last = strlen str - 1
        in  let rec charlist pos result =
            if pos < 0
            then result
            else charlist (pos - 1) (str.[pos] :: result)
        in  charlist last []

    let bigint_of_string str =
        let len = strlen str
        in  let to_intlist first =
                let substr = strsub str first (len - first) in
                let digit char = int_of_char char - int_of_char '0' in
                map digit (reverse (charlist_of_string substr))
            in  if   len = 0
                then zero
                else if   str.[0] = '_'
                     then Bigint (Neg, to_intlist 1)
                     else Bigint (Pos, to_intlist 0)

    let string_of_bigint (Bigint (sign, value)) = match value with
        | []    -> "0"
        | value -> let reversed = reverse value
                   in  strcat ""
                       ((if sign = Pos then "" else "-") ::
                        (map string_of_int reversed))

    let trimzeros list =
        let rec trimzeros' list' = match list' with
            | []       -> []
            | [0]      -> []
            | car::cdr ->
                let cdr' = trimzeros' cdr
                in  match car, cdr' with
                    | 0, [] -> []
                    | car, cdr' -> car::cdr'
        in trimzeros' list

    let rec cmp' list1 list2 = match (list1, list2) with
        | list1, [] -> 0 (* equal all the way *)
        | [], list2 -> 0
        | car1::cdr1, car2::cdr2 ->
            if car1 > car2 then 1
            else if car1 < car2 then -1
            else cmp' cdr1 cdr2

    let cmp list1 list2 =
        if (length list1) > (length list2)
        then 1
        else if (length list1) < (length list2)
        then -1
        else cmp' (reverse list1) (reverse list2) (* compare from MSD *)

    (* binop functions *)
    let rec sub' list1 list2 carry = match (list1, list2, carry) with
        | [], list2, 0         -> list2
        | list1, [], 0         -> list1
        | [], list2, carry     -> sub' [carry] list2 0
        | list1, [], carry     -> sub' list1 [carry] 0
        | car1::cdr1, car2::cdr2, carry ->
            let diff = car1 - car2 - carry
            in if diff >= 0 (* first number bigger *)
               then diff :: sub' cdr1 cdr2 0
               (* +radix for carry *)
               else (diff + radix) :: sub' cdr1 cdr2 1

    let rec add' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], carry   -> add' list1 [carry] 0
        | [], list2, carry   -> add' [carry] list2 0
        | car1::cdr1, car2::cdr2, carry ->
            let sum = car1 + car2 + carry
            in  sum mod radix :: add' cdr1 cdr2 (sum / radix)

    let add (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if neg1 = neg2
        then Bigint (neg1, add' value1 value2 0)
        else if (cmp value1 value2) = 1
        then Bigint (neg1, trimzeros(sub' value1 value2 0))
        else if (cmp value1 value2) = -1
        then Bigint (neg2, trimzeros(sub' value2 value1 0))
        else zero (* diff sign same number *)

    let switchsign sign =
        if sign = Pos
        then Neg
        else Pos

    let sub (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if neg1 <> neg2
        then Bigint (neg1, add' value1 value2 0)
        else if (cmp (trimzeros value1) (trimzeros value2)) = 1
        then Bigint (neg1, sub' value1 value2 0)
        else if (cmp (trimzeros value1) (trimzeros value2)) = -1
        then let neg = switchsign neg2
             in Bigint (neg, sub' value2 value1 0)
        else zero

    let double number = add' number number 0

    let rec mul' (multiplier, powerof2, multiplicand') =
        if (cmp powerof2 multiplier) = 1
        then multiplier, [0]
        else let remainder, product =
             mul' (multiplier, double powerof2, double multiplicand')
             in if (cmp remainder powerof2) = -1
                then remainder, product
                else (trimzeros(sub' remainder powerof2 0)),
                     (add' product multiplicand' 0)

    let mul (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        let _, product = mul' (value1, [1], value2)
        in if neg1 = neg2
           then Bigint (Pos, product) (* same sign mul is POS *)
           else Bigint (Neg, product)

    let rec divrem' (dividend, powerof2, divisor') =
        if (cmp divisor' dividend) = 1
        then [0], dividend
        else let quotient, remainder =
                 divrem' (dividend, double powerof2, double divisor')
             in  if (cmp remainder divisor') = -1
                 then quotient, remainder
                 else (add' quotient powerof2 0),
                      (trimzeros (sub' remainder divisor' 0))

    let divrem (dividend, divisor') = divrem' (dividend, [1], divisor')

    let div (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        let quotient, _ = divrem (value1, value2)
        in if neg1 = neg2
           then Bigint (Pos, quotient)
           else Bigint (Neg, quotient)

    let rem (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        let _, remainder = divrem (value1, value2)
        in if neg1 = neg2
           then Bigint (Pos, remainder)
           else Bigint (Neg, remainder)

    let even number = (car number) mod 2 = 0
    (* let frexp' = frexp (* So we can trace these functions. *)
    let ldexp' (frac, expt) = ldexp frac expt *)

    let rec pow' (base, expt, result) = match expt with
        | [0]                 -> result
        | expt when even expt ->
            let _, product = mul' (base, [1], base)
            in let quotient, _ = divrem (expt, [2])
               in pow' (product, quotient, result)
        | expt                ->
            let _, product = mul' (base, [1], result)
            in pow' (base, (sub' expt [1] 0), product)

    let pow (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if neg2 = Neg
        then zero (* fraction in int is 0 *)
        else Bigint (neg1, pow' (value1, value2, [1]))

end
