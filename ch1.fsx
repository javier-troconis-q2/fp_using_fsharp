//1.1
let g n = n + 4
//1.2
let h (x, y) = System.Math.Sqrt(x * x + y * y)

//1.3
//g = int -> int
//h = float * float -> float
//1.4
let rec f = 
    function 
    | 0 -> 0
    | n -> n + f (n - 1)

let f_tail n = 
    let rec f_tail = 
        function 
        | (0, y) -> y
        | (x, y) -> f_tail (x - 1, y + 1)
    f_tail (n, n)

//1.5
let rec fib = 
    function 
    | 0 -> 0
    | 1 -> 1
    | n -> fib (n - 1) + fib (n - 2)

//1.6
let rec sum = 
    function 
    | (m, 0) -> m
    | (m, n) -> m + n + sum (m, n - 1)

let sum_tail (m, n) = 
    let rec sum_tail = 
        function 
        | (r, 0) -> r
        | (r, n) -> sum_tail (m + n + r, n - 1)
    sum_tail (m, n)

//1.7
// (System.Math.PI, fact -1) = float * int
// fact(fact 4) = int
// power(System.Math.PI, fact 2) = float
// (power, fact) = float -> int -> float * int -> int
//1.8
//f 3 = 4
//g 3 = 9
let rec alt_sum = 
    function 
    | [] -> 0
    | h :: t -> h - alt_sum t

let rec seq_of_pairs = 
    function 
    | h1 :: (h2 :: _ as t) -> (h1, h2) :: seq_of_pairs t
    | _ -> []

let rec sum_prod = 
    function 
    | [] -> (0, 1)
    | h :: t -> 
        let (h', t') = sum_prod t
        (h + h', h * t')

let rec foldl (f : 'a -> 'b -> 'a) (s : 'a) (l : 'b list) : 'a = 
    match l with
    | [] -> s
    | h :: t -> foldl f (f s h) t

let rec foldr (f : 'a -> 'b -> 'b) (s : 'b) (l : 'a list) : 'b = 
    match l with
    | [] -> s
    | h :: t -> f h (foldr f s t)

let rec reverse = 
    function 
    | [] -> []
    | h :: t -> 
        let t' = reverse t
        t' @ [h]

let reverse_tail l = 
    let rec reverse_tail' l a =
        match l with 
        | [] -> a
        | h :: t -> reverse_tail' t (h::a)
    reverse_tail' l []

let rec gdc x y =
    match x % y with
    | 0 -> y
    | z -> gdc y z        

List.fold gdc 0 [88; 120; 240]