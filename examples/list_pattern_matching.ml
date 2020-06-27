let rec max l = match l with
  | x :: [] -> x
  | x :: y :: z -> if x < y then max (y :: z) else max (x :: z) in
max [1; 2; 4; 2; 5; 9; 3;];;
let rec len l = match l with
  | [] -> 0
  | x ::y -> 1 + len y in
len (3 :: 0 :: 2 :: 4 :: 8 :: []);;