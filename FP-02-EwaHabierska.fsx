// Ewa Habierska
// ewha@itu.dk

//ex. 2.1
let timediff (x1:int,y1:int) (x2:int,y2:int) = 
    let hour = x1-x2
    let minU = (60-y2 + y1)*(-1)
    let minD = (60-y1 + y2)
    let oD = if hour = -1 then 0 else (hour*60)*(-1)
    let oU = if hour = 1 then 0 else (hour*60)
    let fin = if hour=0 then y2-y1
               elif hour < 0 then oD+minD
               else oU+minU
                
    (fin);;

    timediff(12,34) (11,35);;
    timediff (12,34) (13,35);;

// ex. 2.2
let minutes (x:int, y:int) :int =
    let all = x*60 + y
    (all);;

minutes (14,24);;
minutes (23,1);;

let rec pow = function
    | (x:string,0:int) -> ""
    | (x,y) -> x + pow(x, y-1);;

//ex. 2.3
// let rec pow = function
//     | (x:string,0:int) -> ""
//     | (x,y) -> x + pow(x, y-1);;
   
// pow("3",2);;

//ex. 2.4
let rec factorial n =
    match n with
    | 0 | 1 -> 1
    | _ -> n * factorial(n-1);;
   
let  bin (x:int, y:int):int =
    let fin = (factorial x)/((factorial y)*(factorial(x-y)))
    (fin);;

bin (5,3);;

//ex. 2.5
// 2.9.1 type int * int -> int
// 2.9.2 for the negative arguments
// 2.9.3
// f(2,3) -> f(1,2*3) -> f(0, 1*2*3) -> 6
// f(x,y) this is how the function is given the parameters, in this case x,y
let rec f = function
 | (0,y) -> y
 | (x,y) -> f(x-1, x*y);;

 //ex. 2.6
 // 2.10.1 type int
 // 2.10.2 the result is 0
 // 2.10.3 the result would be fact -1

//ex. 2.7
let curry f = fun g -> fun h -> f(g,h);
let uncurry g = fun(f,h) -> g f h;