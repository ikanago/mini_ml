let apply = 
  fun f ->
  fun x ->
  fun y ->
    if x < y
    then f x + y
    else f x * y
in apply (fun x -> x + 1) 5 3;;

let apply = fun f x y ->
  if x < y
  then f x + y
  else f x * y
in apply (fun x -> x + 1) 5 3;;

let apply f x y =
  if x < y
  then f x + y
  else f x * y
in apply (fun x -> x + 1) 5 3;;