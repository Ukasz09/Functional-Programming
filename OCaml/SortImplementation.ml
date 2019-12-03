let rec quicksort = function
    [] -> []
  | x::xs -> 
      let small = List.filter (fun y -> y < x ) xs
      and large = List.filter (fun y -> y >= x ) xs
      in quicksort small @ (x :: quicksort large)
;;


let insertSort list =
  let rec insert elem newList = 
    match newList with 
        [] -> elem::[]
      | h::t -> if h>=elem then elem::newList
          else h::insert elem t
  in List.fold_left (fun acc newElement -> insert newElement acc) [] list
;;


let rec mergeSort list = 
  let partition = List.length list / 2 in
    if partition = 0 then list
    else 
      let rec merge list1 list2 =
        match (list1, list2) with
            ([], list2) -> list2
          | (list1, []) -> list1
          | (h1 :: t1, h2 :: t2) ->
              if h1<h2 then h1 :: (merge t1 list2)
              else h2 :: (merge list1 t2)
      and split left right counter =
        if counter = 0 then (List.rev left, right)
        else split (List.hd right :: left) (List.tl right) (counter - 1)
      in let (left, right) = split [] list partition
      in merge (mergeSort left) (mergeSort right)
;;
