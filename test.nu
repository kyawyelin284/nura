let rec sumList lst acc =
  match lst with
  | [] -> acc
  | x:xs -> sumList(xs, acc + x)
in
print(sumList([1,2,3,4], 0))
