let rec quicksort = function
    [] -> []
  | x::xs -> 
      let small = List.filter (fun y -> y < x ) xs
      and large = List.filter (fun y -> y >= x ) xs
      in quicksort small @ (x :: quicksort large)
;;
