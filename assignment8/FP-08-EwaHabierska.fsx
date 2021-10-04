

type BinTree<'a> = 
    | Leaf
    | Node of BinTree<'a> * 'a * BinTree<'a>;;

let t3 = Node(Node(Leaf, -3, Leaf), 0, Node(Leaf, 2, Leaf));;


(* 8.1 | HR 9.8 
Develop a version of the counting function for binary trees
countA: int -> BinTree<’a> -> int
that makes use of an accumulating parameter. Observe that this function is not tail recursive.
*)

let rec countA (acc:int) (tree:BinTree<'a>) = 
    match tree with
    | Leaf -> acc
    | Node(tl,n,tr) -> countA (countA (acc+1) tr) tl;;

countA 0 t3;;

(* 8.2 | HR 9.9 
Declare a tail-recursive functions with the type
countAC : BinTree<’a> -> int -> (int -> ’b) -> ’b
such that count t = countAC t 0 id. The intuition with countAC t a c is that a is the
number of nodes being counted so far and c is the continuation.
*)
let rec countAC t a c=  
       match t with
        | Leaf -> c a
        | Node(tl,n,tr) -> countAC tl (a+1) (fun vl -> countAC tr vl c);;

countAC t3 0 id;;

(* 8.3 | HR 9.10
Consider the following list-generating function:

let rec bigListK n k = 
    if n=0 then k []
    else bigListK (n-1) (fun res -> 1::k(res));;

The call bigListK 130000 id causes a stack overflow. Analyze this problem.
*)

(* 9.10 answer

it does not cuse a stack overflow 

 it is a tail recrusive function which creates a list by continuation. This is not an effective way
 of doing it because this function takes quite much time but can handle about 30% longer lists than
 using an accumulatin parameter.
 
 There is an example in the book of the tail recursion using continuation which is the same as the one
 in this exercise. This function is presented below:
 
 let rec bigListC n c =
       if n=0 then c []
       else bigListC (n-1) (fun res -> c(1::res));;
 
 As we can see these functions are the same, moreover in the book there is an example of a call
 with way bigger parametrs than 130000. This is following:

 bigListC 12000000 id;; - no errors
 bigListC 16000000 id;; - no errors
 bigListC 17000000 id;; - and here, according to the book, error should apperar.

 Unfortunately is apperas by calling bigListC 1200000 already ;(
 *)

(* 8.4 HR 9.11
Declare tail-recursive functions leftTree and rightTree.
By use of leftTree it should be possible to generate a big unbalanced
tree to the left containing n + 1 values in the nodes so that n is the
value in the root, n − 1 is the value in the root of the left subtree, and so on.
All subtree to the right are leaves. Similarly, using rightTree
it should be possible to generate a big
unbalanced tree to the right.

Use these functions to show the stack limit when using count and countA from Exercise 9.8.

Use these functions to test the performance of countC and countAC from Exercise 9.9.
*)

let rec leftTree n c =
        match n with
        | 0 -> c(Node(Leaf,0,Leaf))
        | n ->  leftTree (n-1) (fun x -> c(Node(x,n,Leaf)));;

let rec rightTree n c =
        match n with
        | 0 -> c(Node(Leaf,0,Leaf))
        | n -> rightTree (n-1) (fun x -> c(Node(Leaf,0,x)));;

leftTree 4 id;;
rightTree 4 id;;

(* 9.11 | 1 *)

// count (leftTree 50000 id);;
// count (leftTree 130000 id);;
// count (leftTree 500000 id);;// - stackoverflow
// countA 0 (leftTree 500000 id);;
// countA 0 (leftTree 130000000 id);;
// countA 0 (leftTree 170000000 id);; // - I didn't get stackoverflow but my computer couldn't compute that

(* 9.11 | 2 *)

let rec countC t c = 
       match t with
        | Leaf -> c 0 
        | Node(tl,n,tr) -> countC tl (fun vl -> countC tr (fun vr -> c(vl+vr+1)));;

countC t3 id;;

#time;;
countAC (leftTree 500000 id) 0 id;; //Real: 00:00:00.105, CPU: 00:00:00.150, GC gen0: 12, gen1: 2
countC  (leftTree 500000 id) id;; //Real: 00:00:00.112, CPU: 00:00:00.156, GC gen0: 16, gen1: 1



(* 8.5 | HR 11.1 *)
let odd = 
    let sek = Seq.initInfinite (fun i -> i)
    let isOdd x = (x%2) = 1
    let flt1 = Seq.filter isOdd sek
    let flt = Seq.filter (fun x -> x%2 = 1) sek
    (flt);;
odd;;


(* 8.6 | HR 11.2 *)

// first proposition
let rec fact = function 
    | 0 -> 1
    | n -> n * fact(n-1);;

let facs = Seq.initInfinite (fun i -> fact i);;

facs;;
Seq.nth 5 facs;;

//second proposition
let facs1 = Seq.initInfinite (fun i -> let fn x =
                                            let rec fact = function
                                                | 0 -> 1
                                                | n -> n* fact(n-1)
                                            fact x
                                       fn i
                                            );;
facs1;;
Seq.nth 5 facs1;;
                                      








                            
