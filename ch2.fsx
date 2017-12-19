open System.ComponentModel
open System.Runtime.InteropServices
//2.1
let f n = n % 2 = 0 || n % 3 = 0 && n % 5 <> 0

//2.2
let (+.) x y : string = x + y

let f (x, y) = 
    let rec f = function
        | (y, 0) -> y
        | (y, z) -> f (y +. x, z - 1)
    f ("", y)

let f (x, y) = Seq.fold (fun s _ -> s +. x) "" {1..y}

//2.3
let isIthChar (str : string, i, ch) = str.[i] = ch

//2.4
let occFromIth (str : string, i, ch) = 
    if (i >= String.length str) then 0
    else
        Seq.skip i str
        |> Seq.filter (fun x -> x = ch)
        |> Seq.length

//2.5
let occInString (str, ch) = 
    str
    |> Seq.filter (fun x -> x = ch)
    |> Seq.length

//2.6
let notDivisibleBy (d, n) = n % d <> 0

//2.7.1
let test a b c = 
    b >= a 
    && notDivisibleBy (a, c) 
    && notDivisibleBy (a + 1, c) 
    && notDivisibleBy (b, c) 


let rec gcd = 
    function 
    | (x, 0) -> x
    | (x, y) -> gcd (y, x % y)

type tree<'c> = 
    | Empty
    | Node of 'c tree * 'c * 'c tree

type k<'a, 'b> = 
    | A of 'b
    | B of 'a

type list'<'t> = 
    | Head of 't
    | Tail of 't list'

let rec create_list t = 
    match t with
    | Empty -> []
    | Node(a, b, c) -> create_list a @ [ b ] @ create_list c

let make_list n = 
    let rec f n a = 
        match n with
        | 0 -> a
        | _ -> f (n - 1) (n :: a)
    f n []

let reverse_tail l = 
    let rec f l a = 
        match l with
        | [] -> a
        | h :: t -> f t (h :: a)
    f l []

let reverse_tail1 l = 
    let rec f = 
        function 
        | ([], a) -> a
        | (h :: t, a) -> f (t, h :: a)
    f (l, [])