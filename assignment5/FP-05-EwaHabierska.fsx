
// type BinTree<'a> =
//     | Leaf
//     | Node of 'a * BinTree<'a> * BinTree<'a>

open System

//5.1
type 'a BinTree = 
     Leaf
    | Node of 'a * 'a BinTree * 'a BinTree

let rec inOrder = function
| Leaf -> []
| Node(x,tl,tr) -> (inOrder tl) @ [x] @ (inOrder tr);;

let t1 = Node(-3, Node(0,Node(6,Leaf,Leaf),Leaf),Leaf);;
let t2 = Node(43.0,Node(25.0, Node(56.0,Leaf, Leaf), Leaf), Node(562.0, Leaf, Node(78.0, Leaf,Leaf)));;
inOrder t1;;

//5.2
let rec mapInOrder f = function
    | Leaf -> Leaf
    | Node(x,tl,tr) -> 
                        let tl = mapInOrder f tl
                        Node(f x, tl, mapInOrder f tr)

mapInOrder (fun a -> a + 1) t1;; 

//5.3
let rec foldInOrder f p t =
    match t with
    | Leaf          -> p
    | Node(x,tl,tr) ->
          let help = foldInOrder f p tl
          foldInOrder f (f x help) tr ;;
          
foldInOrder (fun n a -> a + n) 0.0 t2;;

//5.4
type aExp =
| N of int
| V of string
| Add of aExp * aExp (* addition *)
| Mul of aExp * aExp (* multiplication *) 
| Sub of aExp * aExp (* subtraction *)

let rec A a s = match a with
| N n -> n
| V x -> Map.find x s
| Add(a1, a2) -> A a1 s + A a2 s 
| Mul(a1, a2) -> A a1 s * A a2 s
| Sub(a1, a2) -> A a1 s - A a2 s


type bExp =
| TT
| FF
| Eq of aExp * aExp
| Lt of aExp * aExp
| Neg of bExp
| Con of bExp * bExp

let rec B b s = match b with
| TT -> true
| FF -> false
| Eq(a1, a2) -> (a1=a2)
| Lt(a1, a2) -> (a1<a2)
| Neg(a1) -> not (B a1 s)
| Con(a1, a2) -> B a1 s && B a2 s;;


type stm = (*statements*)
| Ass of string * aExp (*assignment*) //-> "" + 
| Skip 
| Seq of stm * stm  (* sequential composition *)
| ITE of bExp * stm * stm (* if-then-else *)
| While of bExp * stm (* while *)
| Inc of int
| IfThen of bExp * stm
| While1 of stm * bExp

let rec I stm s = match stm with
    | Ass(x,a) -> Map.add x (A a s) s
    | Skip -> s
    | Seq(stm1, stm2) -> I stm1 (I stm2 s) 
    | ITE(b,stm1,stm2) -> if (B b s) then I stm s else I stm s
    | While(b, stm) -> let variab = while (B b s) do s
                        let ddd = I stm variab
                        (ddd);;
    | Inc(x) -> Map.add "s" (x+1) s // 5.6
    | IfThen (b, stm)       -> I (ITE (b, stm, Skip)) // 5.5
    | While1 (stm,b) -> I (While (Neg b, stm)) s // 5.5
    
                         


let reg1 = Map.ofList [("a1",25); ("a2",4); ("a3",5)];;

