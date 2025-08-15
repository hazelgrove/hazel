let self = {|
let length : forall a -> [a] -> Int =
  typfun a -> fun xs ->
    case xs
    | [] => 0
    | hd::tl => 1 + length@<a>(tl)
    end
in
let append : forall a -> [a] -> [a] -> [a] =
  typfun a -> fun xs -> fun ys ->
    case xs
    | [] => ys
    | hd::tl => hd::append@<a>(tl)(ys)
    end
in
let reverse : forall a -> [a] -> [a] =
  typfun a -> let rev_helper : forall b -> [b] -> [b] -> [b] =
    typfun b -> fun acc -> fun list ->
      case list
      | [] => acc
      | hd::tl => rev_helper@<b>(hd::acc)(tl)
      end
  in fun xs -> rev_helper@<a>([])(xs)
in

let list1 : [Int] = [1, 2, 3] in
let list2 : [Int] = [4, 5, 6] in

let len1 : Int = length@<Int>(list1) in # 3 #
let len2 : Int = length@<Int>(list2) in # 3 #

let appended_list : [Int] = append@<Int>(list1)(list2) in # [1, 2, 3, 4, 5, 6] #
let reversed_list : [Int] = reverse@<Int>(list1) in # [3, 2, 1] #

(len1, len2, appended_list, reversed_list)
|};
