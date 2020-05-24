let s x y z =
  x z (y z) in
s (fun x y -> x + y) (fun x -> x * 2) 3;;