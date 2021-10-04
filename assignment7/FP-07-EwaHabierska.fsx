
//7.1 (HR 9.1)

// as the firs step, a space for the list xs needs to be created so we create sf0 with an item in a stack xs with a reference to the list in a heap

//we do the first push for g 2
//we create sf1 with a stack with:
// xs -> heap [1;2]
// ys -> heap 2 and now the arrow points to xs
// rev -> heap g 1 pointing to [2]

//now we are pusing g 1 which creates sf2
// xs -> heap [1;2]
// ys -> heap [1] point to g 0
// rev -> heap g 0 pointing to [1]

//now we are pusing g 0 which creates sf3
// xs -> heap [1;2]

// poping g 0 delates the sf3
// xs -> heap [1;2]

// poping g 1 delates the sf2
// xs -> heap: [1;2]
// ys -> heap: [1] points to [1;2] and now we have a link from [1] to xs which goes to the garbage collector in the next step
// rev -> heap: [2] points to [1] points to [1] 

// poping g 2 delates the sf1
// xs -> heap: [1;2]
// ys -> heap: [2] points to [2;1;1] and again we have a link from [2] to rev of g 1 which goest to the garbage collector in the next step
// rev -> heap: [1] points to [1] points to [2] points to [2]

// as the last step: pop sf0
// g [res] -> heap: [1;1;2;2]


//7.2 (HR 9.3)
let rec sum = function
| (m, 0) -> m 
| (m, n) -> m + n + sum(m, n-1);;

let rec sumI(m,n) = if n>0 then m + n + sumI(m,n-1) else m;;

sum(4,5);;
sumI(4,5);;

let ninethree m n = 
    let lista = [1..m]
    let mutable sum = Unchecked.defaultof<int> 
    let f x = sum <- sum + x + n
    List.iter f lista
    (sum);;

// improved
let ninethreex (m, n) =
        let mutable sum = m
        for x in 1..n do
            sum <- sum + (m + x)
        sum

ninethree 4 1;;
ninethreex (4,1);;
ninethree 0 1;;
ninethreex (0,1);;

//alterntive solution but not as a one function
let list = [1..4]
let mutable sum = 0;;
let cons = 1;;
let f x = sum <- sum + x + cons;;
List.iter f list;;
sum;;

//7.3 (HR 9.4)
let ninefour x =
    let mutable lenght = Unchecked.defaultof<int>
    let func x = lenght <- lenght + 1
    List.iter func x
    (lenght);;

let rec f1(l1) = if (not (List.isEmpty l1)) then 1 + f1(List.tail l1) else 0;;

let lista = [1..10];;
ninefour lista;;
funkcja1 lista;;

//improved
let ninefourx l = 
        let mutable length = 0
        for x in l do
            length <- length+1
        length

ninefourx list;;
ninefour list;;

//7.4 (HR 9.6)

let xs16 = List.init 1000000 (fun i -> 16);;

let rec fact = function 
    | 0 -> 1
    | n -> n * fact(n-1);;

let rec factA = function 
    | (0,m) -> m
    | (n,m) -> factA(n-1,n*m);;





let rec factAI(n,m) = if n<>0 then factAI(n-1,n*m) else m;;

factA(5,1);;
factAI(5,1);;

let rec factC n c =
    if n=0 then c 1
    else factC (n-1) (fun res -> c(n*res));;
factC 5 id;;

#time;;
for i in xs16 do let _ = factC i id in ();; //Real: 00:00:00.284, CPU: 00:00:00.282, GC gen0: 127, gen1: 0
for i in xs16 do let _ = fact i in ();; //Real: 00:00:00.063, CPU: 00:00:00.062, GC gen0: 0, gen1: 0
for i in xs16 do let _ = factA(i,1) in ();; //Real: 00:00:00.043, CPU: 00:00:00.043, GC gen0: 0, gen1: 0
for i in xs16 do let _ = () in ();; //Real: 00:00:00.011, CPU: 00:00:00.011, GC gen0: 0, gen1: 0

//7.5 and 7.6 (HR 8.6 and 9.7)
let rec fib = function
    | 0 -> 0
    | 1 -> 1
    | n -> fib(n-1) + fib(n-2);;

// the solution for the 7.5 (HR 8.6)
let fibW n =  let mutable nb = n
              let mutable f = 0 
              let mutable s = 1 
              let mutable next = 0
              while (nb>0) do 
                  next <- f + s
                  f <- s 
                  s <- next
                  nb <- nb-1
              next;;

let mutable x = 0;;
let mutable y = 0;;

// sloutions to the 7.6 (HR. 9.7)

let rec fibA(n,a,b) = if n <> 0 
                       then fibA(n-1,a+b,a) 
                       else a;;

let rec fibC (n:int) (c:int->int) = 
                                    if n=0 then c(0) 
                                    else if n=1 then c(1) 
                                    else fibC (n-2) (fun a -> fibC (n-1) (fun b -> c (a+b)));;

#time;;

fibW 70;;
fibA (70,0,1);;
fibC 70 id;;

for i in xs16 do let _ = fibW i in ();; //Real: 00:00:00.047, CPU: 00:00:00.047, GC gen0: 0, gen1: 0
for i in xs16 do let _ = fibA(i,1,0) in ();; //Real: 00:00:00.027, CPU: 00:00:00.026, GC gen0: 0, gen1: 0
for i in xs16 do let _ = fibC i id in ();; // my computer can not handle it

