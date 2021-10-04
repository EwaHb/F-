open System.Globalization




//4.1
let explodeChar (s:string) = List.ofArray(s.ToCharArray());;
explodeChar "stirng";; 

let rec explodeChar1 (s:string) = 
    match s with
    | "" -> []
    | s -> s.Chars(0) :: explodeChar1 (s.Remove (0,1));;

explodeChar1 "string";;
// Rozdzielamy stringa do listy of String:
let explodeString param = 
                let explode (s:string) = List.ofArray(s.ToCharArray())
                let t1 = explode param
                let h2 (param1 : char list) = List.map (fun x -> (x|> string)) param1
                h2 t1

explodeString "ddd"

//4.2
let implodeChar (lista: char list) = List.foldBack (fun literka pustyString ->  (literka|>string) + pustyString ) lista "" ;;
implodeChar ['h';'e'];;

let implodeString lista = List.foldBack (fun literka pustyString ->  literka + pustyString ) lista "" ;;

let implodeInt (lista: int list) = List.foldBack (fun literka pustyString ->  (literka|>string) + pustyString ) lista "" ;;
implodeInt [1;2;3;4;5];;

let rec implodeRev (lista:char list) =
    match lista with
    | [] -> ""
    | lista -> List.fold (fun literka pustyString ->  (pustyString|>string) + literka ) "" lista

//4.3
jezeli chcemy przekonwertowac calego stringa na due litery to ponizsza funkcja:
let toUpper = String.map System.Char.ToUpper 
toUpper "srt";;

jezeli chcemy chara przekonwertowac to wtedy wykonujemy operacje System.Char.ToUpper(x) ktora konwertuje literke:
let toUpper1 (x:string)= (explode >> (List.map (fun x -> System.Char.ToUpper(x))) >> implode) x;;

let toUpper2 (x:string) =
    x |> (implode << (List.map (fun x -> System.Char.ToUpper(x))) << explode)

toUpper2 "hej";;

pokazane jak są przekazywane wyniki funckji jako parametry. "X" jest parametrem stringiem. x jest przekawyzany do funkcji explode (czyli mozna to tak zapiac x |> explode). Funkcja explode wyrzuca nam liste charów. więc tą listę charów przekazujemy dalej do funckji (List.map (fun x -> System.Char.ToUpper(x))) ktora nam wyrzuci tę samą listę czarów ale w duych literach. potem przekazujemy to do funcki implode ktora nam z powrotem zlaczy liste w stringa.

let toUpper3 (x:string)=
    x
    |> explode
    |> (List.map (fun x -> System.Char.ToUpper(x)))
    |> implode

toUpper3 "hej";;

//4.4
let palindrome (x:string):bool = 
    let f1 = (explode >> (List.map (fun x -> System.Char.ToUpper(x)))) x
    let f2 = (explode >> (List.map (fun x -> System.Char.ToUpper(x))) >> List.rev) x
    (f1=f2)

palindrome "Aannaa"

let palindrome2 (x:string):bool = 
    let f1 = (explode >> (List.map (fun x -> System.Char.ToUpper(x)))) x
    let f2 = List.rev f1
    (f1=f2)
palindrome2 "Aannaa"

//4.5
let rec ack (x,y) =
    match (x,y) with
    | (0,_) -> y+1
    | (_,0) -> ack (x-1,1)
    | (_,_) -> ack (x-1, ack(x, y-1))

ack (3,11);; // val it : int = 16381

//4.6

let time f =
    let start = System.DateTime.Now in let res = f () in
    let finish = System.DateTime.Now in (res, finish - start);

time (fun () -> ack (3,11))

let timeArg1 f param =
    let start = System.DateTime.Now in
    let res = f param in
    let finish = System.DateTime.Now in
    (res, finish - start);;

timeArg1 (fun () -> ack(3,11))

//4.7
let rec downTo1 f (n,e) = 
    match n with
    |_ when n<=0 -> e
    |_ when n>0 -> downTo1 f (n-1, f(n,e));;

let multi (a,b) = a*b;;
let fac n = downTo1 multi (n,2);;
fac 8;;




    
    







// //let list = [1..3..30]
// let animals = ["cat"; "bird"; "fish"; "fox"]
// // let removeOddIdx param = 
// //     let xxx = List.filter(fun x -> x % 2 = 1) param
// //     (xxx)
// //List.fold (fun (ts : char list) (i : string) ->  i.[0] :: ts) [] ["Ala"; "Ewa"; "Iza"];;
// let norm(x1:string,y1:string) = x1+y1;;
// let split (param:string) = List.ofArray(param.ToCharArray());;
// let f0 param = List.fold (fun (ts : string list) (i : string) ->  i :: ts) [] param;;
// f0 animals;;





//let (@) xs ys = List.foldBack (fun x rst -> x::rst) xs ys;;
// List.fold (fun (ts : char list) (i : string) ->  i.[0] :: ts) [] ["Ala"; "Ewa"; "Iza"];;

//List.foldBack (fun (ts : char list) (i : string) ->  i.[0] :: ts) ["Ala"; "Ewa"; "Iza"] [] ;;
// let s = [1;2;3];;
// let xxx param = List.foldBack (-) param 0;;





// let elements (param:string) = 
//     seq {
//         let tee = StringInfo.GetTextElementEnumerator(param)
//         while tee.MoveNext() do 
//             yield tee.GetTextElement() 
//     }
// let reversed = elements |> Array.ofSeq |> Array.rev |> String.concat ""