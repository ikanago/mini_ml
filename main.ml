let apply = 
  fun f ->
  fun y -> 
    f y
in let f = fun x -> x + 1 
in apply f 5;;