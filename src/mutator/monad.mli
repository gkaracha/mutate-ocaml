type 'a t

(* Monad instance *)
val return : 'a -> 'a t
val bind : 'a t -> ('a -> 'b t) -> 'b t
val (>>=) : 'a t -> ('a -> 'b t) -> 'b t

(* Alternative instance *)
val choose : 'a t -> 'a t -> 'a t
val (<|>) : 'a t -> 'a t -> 'a t

(* Applicative instance *)
val (<*>) : ('a -> 'b) t -> 'a t -> 'b t
val pure : 'a -> 'a t

(* Functor instance *)
val fmap : ('a -> 'b) -> 'a t -> 'b t
val (<$>) : ('a -> 'b) -> 'a t -> 'b t

(* General *)
val mmap : ('a -> 'b t) -> 'a list -> ('b list) t

(* List-specific *)
val from_list : 'a list -> 'a t
val run : 'a t -> 'a list
