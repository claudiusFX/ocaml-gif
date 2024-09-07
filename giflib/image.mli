type t

val v : ?offset:int * int -> int * int -> ColorTable.t -> int array -> t
val dimensions : t -> int * int
val offset : t -> int * int
val palette : t -> ColorTable.t
val pixels : t -> int array
val rgb_pixels : t -> (int * int * int) array
