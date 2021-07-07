type 'a t = 'a list

(* Monad instance *)
let return x = [x]
let bind x f = List.concat_map f x
let[@inline] (>>=) x f = bind x f

(* Alternative instance *)
let choose xs ys = List.append xs ys
let[@inline] (<|>) xs ys = choose xs ys

(* Applicative instance *)
let (<*>) mf mx =
  mf >>= fun f ->
  mx >>= fun x ->
  return (f x)
let[@inline] pure x = return x

(* Functor instance *)
let fmap f m = List.map f m
let[@inline] (<$>) f m = fmap f m

(* General *)
let rec mmap f = function
  | [] -> return []
  | (x :: xs) ->
    f x       >>= fun y ->
    mmap f xs >>= fun ys ->
    return (y :: ys)

(* List-specific *)
let from_list xs = xs
let run xs = xs
