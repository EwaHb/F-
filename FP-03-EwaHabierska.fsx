let list = [1..3..30]
let list2 = [0 .. 10 .. 100]
let listEwa = [1..10]
// 3.1

let downToInt =  function
| 0 -> [] //if 0 then else
| n -> [1..1..n]
downToInt 5;;
downToInt 0;;

let downToElement x (i:int) = 
    match i with
        | 0 -> [] //if 0 then else
        | n -> let list = [1..1..n]
               List.map (fun y ->  x ) list
downToElement "a" 5

let downTo2 n = 
    match n with
        | 0 -> []
        | _ -> [1..1..n]
downTo2 0;;
downTo2 5;;

// 3.2

let rec removeOddIdx = function 
    |[] -> []
    |[x] -> [x]
    | (x1::x2::xs) -> [x1] @ removeOddIdx xs

removeOddIdx list;;
removeOddIdx list2;;


// 3.3

let rec combinePair = function
    | (x0::x1::list)  -> [(x0,x1)] @ combinePair(list)
    | _             -> []

let rec combinePair1 = function
    | (x0::x1::list)  -> (x0,x1)::combinePair1(list)
    | _             -> []
combinePair1 listEwa;;

//3.4
type Pence = { number : int;};;
type Shilling = { number : int;};;
type Pound = { number : int;};;
type Currency ={ Pound:int; Shilling: int; Pence:int;};;
let pocket1 = {Pound = 2; Shilling = 21; Pence = 0;};;
let pocket2 = {Pound = 1; Shilling = 6; Pence = 55;};;
let pocket3 = {Pound = 0; Shilling = 0; Pence = 410;};;
let pocket4= {Pound = 0; Shilling = 0; Pence = 0;};;


let firstStepPlus (x:Currency, y:Currency):Currency = 
                {Pound = x.Pound + y.Pound; 
                Shilling = x.Shilling + y.Shilling; 
                Pence = x.Pence + y.Pence;}
firstStepPlus (pocket1,pocket2);;

let konwertPlus (x:Currency) =
    if (x.Pence>12 || x.Shilling> 20 )then 
    let pence = x.Pence % 12
    //let shillingPrep = x.Shilling + x.Pence/12
    let shilling = (x.Shilling + (x.Pence/12))%20
    let pound = x.Pound + (x.Shilling/20)
    let piniondz:Currency = {Pound = pound; Shilling = shilling; Pence = pence;}
    piniondz
    else
    x
konwertPlus (pocket1);;
konwertPlus pocket3;;

let finalStepPlus (x:Currency,y:Currency) =
    (x,y)
    |> firstStepPlus
    |> konwertPlus

finalStepPlus (pocket1,pocket2);;
//---------------------------
let konwertMinus (x:Currency):Currency =
    let pence = (x.Pound* 20 * 12) + (x.Shilling*12) + x.Pence 
    let xxx:Currency = {Pound = 0; Shilling = 0; Pence = pence;}
    (xxx);;
konwertMinus pocket1;;

let odejmowanie (x:Currency, y:Currency):Currency = 
                if (x.Pence < y.Pence) then failwith "Cannot return negative amount"
                {Pound = x.Pound - y.Pound; 
                Shilling = x.Shilling - y.Shilling; 
                Pence = x.Pence - y.Pence;}
odejmowanie (pocket1,pocket2);;

let finalStepMinus (x:Currency,y:Currency):Currency =
    let f1 = konwertMinus x
    let f2 = konwertMinus y
    let f3 = odejmowanie (f1,f2)
    let f4 = konwertPlus f3
    (f4)

finalStepMinus (pocket3,pocket4);;

let (&-) x1 x2 = finalStepMinus(x1,x2)
let (&+) x1 x2 = finalStepPlus(x1,x2);

//3.5
//3.5.1

type cNb = float * float;;

let addComplex (x:cNb,y:cNb) =
    let x1 , x2 = x
    let y1, y2 = y
    (x1+y1, x2+y2)

let (+.) x y = addComplex (x,y)


let mulComplex (x:cNb,y:cNb) =
    let x1 , x2 = x
    let y1, y2 = y
    (x1*y1 - x2*y2, y2*x2 - x1*y2)

let ( *.) x y = mulComplex (x,y)
//3.5.2
let addition (x,y) =
    let x1 = -x
    let y1 = -y
    (x1,y1)

let (-.) x y = addition (x,y)

let multiplication (a,b) = 
        let f1 = (a*a + b*b)
        (a/f1, -(b/f1))

let (./.) (1) (a,b) = multiplication (a,b)

//3.6
let rec altsumTwoClauses = function
    | [] -> 0
    | x0 :: xs  -> x0 - altsumTwoClauses xs

altsumTwoClauses list;;





//+ , - , * , /      <--- INFIX OPERATORS (in between)

// (.+)  , (.-) , (.*)   <---- INFIX FUNCTIONS (they use infix operators (in between) but as a function)

//(.+) 2 4;;

// let (&.) (x1,y1) (x2,y2) = x1*x2 + y1*y2: float;;
// let (&.) (2,4) (1,2);;

// this is the end.
//---------------------------------------

// let len lst =
//   let mutable index = 0
//   for _ in lst do
//     index <- index + 1
//   index


// let pairs param =
//     let lengh = len list
//     if lengh % 2 = 0 then
//     let L1 = List.filter(fun x -> x % 2 = 1) param
//     let L2 = List.filter(fun x -> x % 2 = 0) param
//     let L3 = List.zip L1 L2
//     (L3)
//     else
//     let O1 = List.tail param // trzeba usunac ostatni element
//     let O2 = List.filter(fun x -> x % 2 = 1) O1
//     let O3 = List.filter(fun x -> x % 2 = 0) O1
//     let F = List.zip O2 O1
//     (F)

// let X1 = [1;2;3;4;5;6;7;8];;
// let X2 = [1;2;3;4;5];;

// pairs X2;;

// let folder state value = 
//     if value % 2 = 0 then 
//         state + value 
//     else 
//         state

// let initialState = 0

// List.fold folder initialState list

// let minutes (x:int, y:int) :int =
//     let all = x*60 + y
//     (all);;






