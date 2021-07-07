type 'a t = 'a list

let return x = [x]

let bind x f = List.concat_map f x
let[@inline] (>>=) x f = bind x f

let choose xs ys = List.append xs ys
let[@inline] (<|>) xs ys = choose xs ys

let from_list xs = xs
let run xs = xs

