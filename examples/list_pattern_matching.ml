let a = [1; 2; 3;] in
let rec len x = match x with
  | [] -> 0
  | head::tail -> 1 + len tail
in len a;;