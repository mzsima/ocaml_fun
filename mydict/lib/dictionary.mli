open Base

type ('a, 'b) t

val create  : hash:('a -> int) -> equal:('a -> 'a -> bool) -> ('a, 'b) t
val length  : ('a, 'b) t -> int
val add     : ('a, 'b) t -> key:'a -> data:'b -> unit
val find    : ('a, 'b) t -> 'a -> 'b option
val iter    : ('a, 'b) t -> f:(key:'a -> data:'b -> unit) -> unit 
val remove  : ('a, 'b) t -> 'a -> unit