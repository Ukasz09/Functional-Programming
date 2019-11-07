let extendedFoldRight f1 f2 listOfPair acc1 acc2 =
  ((List.fold_right (fun (x,y)-> f1 x) listOfPair acc1),(List.fold_right (fun (x,y)-> f2 y) listOfPair acc2))
;;  

let extendedFoldLeft f1 f2 listOfPair acc1 acc2 =
  ((List.fold_left (fun acc (x,y)-> f1 acc x) acc1 listOfPair),(List.fold_left (fun acc (x,y)-> f2 acc y) acc2 listOfPair))
;;
