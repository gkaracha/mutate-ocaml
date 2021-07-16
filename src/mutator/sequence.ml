type 'a t = 'a list

let to_list xs = xs

let singleton x = [x]

let empty = []

let append xs ys = xs @ ys

let ($$) xs ys = append xs ys

let fmap f xs = List.map f xs

let (<$>) f xs = fmap f xs
