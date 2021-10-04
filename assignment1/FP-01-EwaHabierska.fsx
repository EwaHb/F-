//ex.1
let sqr = function
| x -> x*x;;

let haha x = x+x

//ex.2
let pow = function
| (x,n) -> System.Math.Pow(x, n);;

//ex.1.3
let g = function
| n -> n+4;;

//ex.1.4

let h (x:int,y:int):int =
    let xx = x*x |> float
    let yy = y*y |> float
    let xxyy = xx+yy
    let hh = System.Math.Sqrt(xxyy)
    let h = hh |> int
    (h);;

h (2,7);;

//ex.1.5
let rec f = function
| 0 -> 0
| n -> f(n-1) + n;;

f 5;;

// f(5)
// f(4) + 5
// f(3) + 4 + 5
// f(2) + 3 + 4 + 5
// f(1) + 2 + 3 + 4 + 5
// f(0) + 1 + 2 + 3 + 4 + 5
// 0 + 1 + 2 + 3 + 4 + 5 = 15 

//ex.1.6
let rec fibonacci = function
| 0 -> 0
| 1 -> 1
| n -> fibonacci(n-1) + fibonacci(n-2);;

//ex.1.7
let rec sum = function
| (m,0) -> m
| (m,n) -> sum(m , (n - 1)) + (m+n);;

sum (5,2);;


//1.8
// (System.Math.PI, fact -1) -> stack overflow -> (float, not defined) 

// fact(fact 4) -> int
let rec fact = function
    | 0 -> 1
    | n -> n * fact(n-1);;  

// power(System.Math.PI, fact 2) -> float
let rec power = function
    | (_,0) -> 1.0                
    | (x,n) -> x * power(x,(n-1));; 

// (power, fact) -> (float, int)

//ex.1.9
// f 3 = 4
// g 3 = 9

//ex.1.10
let dup = function
| x -> ""+x+x;;

//ex.1.11
let rec dupn s n =
    match n with
    | _ when n <= 0 -> ""
    | _ -> s + (dupn s (n-1));;


// old solution for 1.4
// let h  = function
// | (x,y) -> System.Math.Sqrt(x*x + y*y);;