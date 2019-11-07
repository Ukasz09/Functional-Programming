(* From given list of list return list of list in which all number are diffrent than given by user value
  Example: Input: [[1;2;3];[3;4];[5;6];[4;2]] 3, Output: [[5;6];[4;2]] *)
let filterDifferentThenParameter lists parameter=
  List.filter (fun x-> (List.mem parameter x)=false && x<>[]) lists
;;

(*Convert binary number (saved as list of 1 or 0 numbers) to decimal numbers*)
let binaryToDecimal binaryList =
  let rec binaryHelper list expotent accum=
    match list with
        [] -> accum
      | h::t -> if h=0 then binaryHelper t (expotent-1) accum else
          if h=1 then binaryHelper t (expotent-1) (accum+h*int_of_float (2.**( float_of_int (expotent)-.1.0)))
          else raise (Failure "Invalid binary number")
  in binaryHelper binaryList (List.length binaryList) 0
;;

(*Return all lucky numbers which is less than given by user number 
Lucky number definition: sum first of 3 figures are equal sum of last 3 figures
if number is less figures than 6 then we fill it up to 6 figures by adding 0 on individual number place *)
let findLuckyNumbersTail upperLimitOfNumber= 
  let valueOfNumberPlace baseNumber numberToCalculate =
    if baseNumber>numberToCalculate then 0
    else (numberToCalculate / baseNumber) mod 10 in 

  let rec luckyNumberHelper actualNumber=
    if actualNumber > upperLimitOfNumber then [] 
    else 
      let sumOfFirstThree = valueOfNumberPlace 1 actualNumber + valueOfNumberPlace 10 actualNumber + valueOfNumberPlace 100 actualNumber in 
      let sumOfSecondThree = valueOfNumberPlace 1000 actualNumber + valueOfNumberPlace 10000 actualNumber + valueOfNumberPlace 100000 actualNumber in
        if (sumOfFirstThree = sumOfSecondThree) then
          actualNumber::luckyNumberHelper (actualNumber + 1)
        else luckyNumberHelper (actualNumber + 1)
  in luckyNumberHelper 0
;;
