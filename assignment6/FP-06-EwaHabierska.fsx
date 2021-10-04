
// #r @"/Users/ewahabierska/Documents/ITU/FP/w6/Vector/VectorSimple.dll";;
// open VectorSimple
// 6.1
type Fexpr = | Const of float
             | X 
             | Add of Fexpr * Fexpr
             | Sub of Fexpr * Fexpr
             | Mul of Fexpr * Fexpr
             | Div of Fexpr * Fexpr
             | Sin of Fexpr
             | Cos of Fexpr
             | Log of Fexpr
             | Exp of Fexpr

let rec F fe:string = 
    match fe with
    | Const(value) -> string value
    | Add(fe1,fe2) -> F fe1 + " " + F fe2 + " +"
    | Sub(fe1,fe2) -> F fe1 + " " + F fe2 + " -"
    | Mul(fe1,fe2) -> F fe1 + " " + F fe2 + " *"
    | Div(fe1,fe2) -> F fe1 + " " + F fe2 + " /"
    | Sin(value) -> F value + " sin"
    | Cos(value) -> F value + " cos"
    | Log(value) -> F value + " log"
    | Exp(value) -> F value + " exp"

F (Sin (Sin(Const 4.)));;
F (Mul (Add(Const 5.0, Const 4.0), Const 5.0              ))
F (Mul  (Add(Const 5.0,Const 7.0),  Sub(Const 5.0,Const 7.0)))

//6.2 (HR 6.8.1)
let stack1 = [1.0..2.0..15.]
type Instruction = 
    | ADD | SUB | MUL | DIV | SIN | COS | LOG | EXP | PUSH of float

let rec intpInstr stack instruction = //(fe:list):list = 
    match instruction, stack with
    | ADD, (x::s::xs) -> ((x+s)::xs)
    | SUB, (x::s::xs) -> ((x-s)::xs)
    | MUL, (x::s::xs) -> ((x*s)::xs)
    | DIV, (x::s::xs) -> ((x/s)::xs)
    | SIN, (x::xs) -> (sin x::xs)
    | COS, (x::xs) -> (cos x::xs)
    | LOG, (x::xs) -> (log x::xs)
    | EXP, (x::xs) -> (exp x::xs)
    | PUSH(f), (x::xs) -> (f::x::xs)

intpInstr stack1,PUSH(6.)

//6.2 (HR 6.8.2)
let intpProg instrukcja = 
    let rec helper (stack: float list) is = 
        match is with 
        | []                -> stack.Head // what is it
        | instruction::rest -> let resultStack = intpInstr stack instruction 
                               helper resultStack rest
    helper [] instrukcja

let instrukcje = [PUSH(3.0);PUSH(4.0);ADD]
intpProg instrukcje;;

//6.2 (HR 6.8.3)
let rec trans fe x =
    match fe with
    | Const(value)      -> [PUSH(value)]
    | Add(f1, f2)  -> (trans f1 x) @ (trans f2 x) @ [ADD]
    | Sub(f1, f2)  -> (trans f1 x) @ (trans f2 x) @ [SUB]
    | Mul(f1, f2)  -> (trans f1 x) @ (trans f2 x) @ [MUL]
    | Div(f1, f2)  -> (trans f1 x) @ (trans f2 x) @ [DIV]
    | Sin(expr)         -> (trans expr x) @ [SIN]
    | Cos(expr)         -> (trans expr x) @ [COS]
    | Log(expr)         -> (trans expr x) @ [LOG]
    | Exp(expr)         -> (trans expr x) @ [EXP]

//6.3 (HR 7.2)
//signature file
module Complex
[<Sealed>]
type Complex =
    static member ( + ) : Complex * Complex -> Complex
    static member ( - ) : Complex * Complex -> Complex
    static member ( * ) : Complex * Complex -> Complex
    static member ( / ) : Complex * Complex -> Complex
val make : float * float -> Complex

module Complex
type Complex =
  | C of float * float
  static member ( + ) (C (r1,c1), C (r2,c2)) = (C(r1 + r2, c1 + c2))
  static member ( - ) (C(x1,y1),C(x2,y2)) = C(x1-x2,y1-y2)
  static member ( * ) (C (r1,c1), C (r2,c2)) = (C(r1*r2 - c1*c2, c1*r2 + r1*c2))
  static member ( / ) (C (r1,c1), C (r2,c2)) = (C((r1*(r2/(r2**2. + c2**2.)))-(c1*(-c2/(r2**2.+ c2**2.))), ((c1*(r2/(r2**2. + c2**2.)))+(r1*(-c2/(r2**2. + c2**2.))))))
let make(x,y) = C(x,y)




