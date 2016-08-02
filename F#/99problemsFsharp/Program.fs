// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

//P01 (*) Find the last box of a list.
//Example:
//* (my-last '(a b c d))
//(D)

let p1 l =  Seq.last l

let rec p12 = function
    | [hd] -> hd
    | _ :: tl -> p12 tl
    | _ -> failwith "Empty list."

//P02 (*) Find the last but one box of a list.
//Example:
//* (my-but-last '(a b c d))
//(C D)

let p2 l = Seq.pairwise l |> Seq.last

//P03 (*) Find the K'th element of a list.
//The first element in the list is number 1.
//Example:
//* (element-at '(a b c d e) 3)
//C

let p3 l k =
    try
        Some(Seq.nth k l)
    with
        | _-> None

//P04 (*) Find the number of elements of a list.

let p4 l = Seq.length

//P05 (*) Reverse a list.

let p5 l = l |> Seq.toList |> List.rev

//P06 (*) Find out whether a list is a palindrome.
//A palindrome can be read forward or backward; e.g. (x a m a x).
let p6 l = List.forall2 (=) (p5 l)  (Seq.toList l) 


//P07 (**) Flatten a nested list structure.
//Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).
//
//Example:
//* (my-flatten '(a (b (c d) e)))
//(A B C D E)
//
//Hint: Use the predefined functions list and append.


type  T<'t> =
|L of 't
|N of T<'t> list

let rec flatten l =
    match l with
    |L x -> [x]
    |N x -> List.concat(List.map flatten x)
 
let tree = N[L 14;N[];N[L 1;L 10;L 23]]

let result = flatten tree


//P08 (**) Eliminate consecutive duplicates of list elements.
//If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
//
//Example:
//* (compress '(a a a a b c c a a d e e e e))
//(A B C A D E)

let p8 = function
    |  [] -> []
    | x::xs -> List.fold (fun acc x -> if x = List.head acc then acc else x::acc ) [x] xs
   

//P09 (**) Pack consecutive duplicates of list elements into sublists.
//If a list contains repeated elements they should be placed in separate sublists.
//
//Example:
//* (pack '(a a a a b c c a a d e e e e))
//((A A A A) (B) (C C) (A A) (D) (E E E E))

let p9 xs = 
    let collect x = function
        | (y::xs)::xss when x = y -> (x::y::xs)::xss
        | xss -> [x]::xss
    List.foldBack collect xs []

//P10 (*) Run-length encoding of a list.
//Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.
//
//Example:
//* (encode '(a a a a b c c a a d e e e e))
//((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))
 
let p10 l =
    let transform t =
        (List.head t, List.length t)
    l |> p9 |> List.map transform

//Problem 11
//(*) Modified run-length encoding.
//
//Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.
//
//Example:
//
//* (encode-modified '(a a a a b c c a a d e e e e))
//((4 A) B (2 C) (2 A) D (4 E))
//Example in Haskell:
//
//P11> encodeModified "aaaabccaadeeee"
//[Multiple 4 'a',Single 'b',Multiple 2 'c',
// Multiple 2 'a',Single 'd',Multiple 4 'e']
//Solutions
type Pies = |Single of char | Multiple of int*char
let encodeModified xs =
    let f = function
        |(1, c) -> Single c
        |(l, c) -> Multiple (l, c)
    let rec folder (results:Pies list) (l, c) (xs:char list) : Pies list =
         match xs with
            | [] -> results @ [f (l, c)]  
            | x::xs when x = c -> folder results (l+1, x) xs  
            | x::xs -> folder (results @ [f (l,c)]) (1, x) xs  
    match xs with
        | [] -> []
        | x::xs -> folder [] (1, x) xs


[<EntryPoint>]
let main argv = 
    printfn "%A" (p3 [1;2;3] 2)
    0 // return an integer exit code
