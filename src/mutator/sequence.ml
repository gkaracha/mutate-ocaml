type 'a sequence = 'a list

let to_list xs = xs

let unit_sequence x = [x]

let empty = []

let append xs ys = xs @ ys

let ($$) xs ys = append xs ys

let fmap f xs = List.map f xs
