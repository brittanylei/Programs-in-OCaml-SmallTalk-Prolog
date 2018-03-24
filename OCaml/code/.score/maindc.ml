(* $Id: maindc.ml,v 1.5 2017-04-07 13:24:41-07 - - $ *)
(* bclei *)

include Scanner
include Bigint

open Bigint
open Printf
open Scanner

let car       = List.hd
let cdr       = List.tl

let strcat    = String.concat
let strlen    = String.length
let strsub    = String.sub

type stack_t = Bigint.bigint Stack.t
let push = Stack.push
let pop = Stack.pop

let ord thechar = int_of_char thechar
type binop_t = bigint -> bigint -> bigint

(* let rec print_number' number newNum' =
  if strlen newNum' = 69
  then let newNum' = strcat newNum' ["\\\n"]
  in print_number' (cdr number) newNum'
  else let newNum' = strcat newNum' (strsub (string_of_bigint number) (car number) 1)
  in newNum' *)

(* let rec print_number' number number' =
  let newNum = number' ^ (car number)
  in if (strlen newNum) = 69
  then let number' = number' ^ "\\\n"
  in print_number' number number'
  else print_number' number number'

let print_number number =
  let number' = List.map string_of_int number
  in printf "%s\n%!" (print_number' (cdr number') (car number') ) *)


(* let rec print_number' number newnumber =
  if (strlen newnumber) = 69
  then let newnumber = newnumber ^ "\\\n"
       in print_number' number newnumber
  else let newnumber = newnumber ^ car number
       in print_number' (cdr number) newnumber

let print_number number =
    let stringl = (string_of_bigint number)
    in let charl = (charlist_of_string stringl)
    in printf "%s\n%!" (string_of_bigint (bigint_of_string
        (print_number' charl []))) *)



let rec print_number' number newnum =
    let len = strlen number
    in if len < 69
    then printf "%s\n%!" number
    else begin
          (printf "%s%s\n%!" (strsub number 0 69) "\\");
          (print_number' (strsub number 69 (len - 69)) newnum)
    end

(* let print_number number = printf "%s\n%!" (string_of_bigint number) *)
let print_number number = print_number' (string_of_bigint number) []

let print_stackempty () = printf "stack empty\n%!"

(* reg *)
let aget = Array.get
let aset = Array.set
let amake = Array.make
let ord = Char.code

type command = Load of char | Store of char * float

let symbol_table = amake 256 (false, zero)

let executereg (thestack: stack_t) (oper: char) (reg: int) =
    try match oper with
        | 'l' -> (
              let entry = aget symbol_table reg
              in match entry with
                 | false, _ -> printf "register '%c' is empty\n%!" (char_of_int reg)
                 | true, value -> push value thestack
                 )
        | 's' ->
              aset symbol_table reg (true, pop thestack);
        | _   -> printf "0%o 0%o is unimplemented\n%!" (ord oper) reg
    with Stack.Empty -> print_stackempty()

let executebinop (thestack: stack_t) (oper: binop_t) =
    try let right = pop thestack
        in  try let left = pop thestack
                in  push (oper left right) thestack
            with Stack.Empty -> (print_stackempty ();
                                 push right thestack)
    with Stack.Empty -> print_stackempty ()

let execute (thestack: stack_t) (oper: char) =
    try match oper with
        | '+'  -> executebinop thestack Bigint.add
        | '-'  -> executebinop thestack Bigint.sub
        | '*'  -> executebinop thestack Bigint.mul
        | '/'  -> executebinop thestack Bigint.div
        | '%'  -> executebinop thestack Bigint.rem
        | '^'  -> executebinop thestack Bigint.pow
        | 'c'  -> Stack.clear thestack
        | 'd'  -> push (Stack.top thestack) thestack
        | 'f'  -> Stack.iter print_number thestack
        | 'l'  -> failwith "operator l scanned with no register"
        | 'p'  -> print_number (Stack.top thestack)
        | 'q'  -> raise End_of_file
        | 's'  -> failwith "operator s scanned with no register"
        | '\n' -> ()
        | ' '  -> ()
        | _    -> printf "0%o is unimplemented\n%!" (ord oper)
    with Stack.Empty -> print_stackempty()

let toploop (thestack: stack_t) inputchannel =
    let scanbuf = Lexing.from_channel inputchannel in
    let rec toploop () =
        try  let nexttoken = Scanner.scanner scanbuf
             in  (match nexttoken with
                 | Number number       -> push number thestack
                 | Regoper (oper, reg) -> executereg thestack oper reg
                 | Operator oper       -> execute thestack oper
                 );
             toploop ()
        with End_of_file -> printf "End_of_file\n%!";
    in  toploop ()

let readfiles () =
    let thestack : bigint Stack.t = Stack.create ()
    in  ((if Array.length Sys.argv > 1
         then try  let thefile = open_in Sys.argv.(1)
                   in  toploop thestack thefile
              with Sys_error message -> (
                   printf "%s: %s\n%!" Sys.argv.(0) message;
                   exit 1));
        toploop thestack stdin)

let interact () =
    let thestack : bigint Stack.t = Stack.create ()
    in  toploop thestack stdin

let _ = if not !Sys.interactive then readfiles ()
