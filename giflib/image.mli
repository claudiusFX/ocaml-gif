(** {1 GIF image}

    Represents a single image within the GIF file. *)

type t

val v :
  ?offset:int * int ->
  ?transparent:int option ->
  ?delay_time:int option ->
  int * int ->
  ColorTable.t ->
  Bytes.t ->
  int ->
  bool ->
  t
(** [v ?offset ?transparent ?delay_time dim palette compressed_image_data
     lzw_code_size interlaced] Create an image record for use inside a GIF.t. *)

val of_pixels :
  ?offset:int * int ->
  ?transparent:int option ->
  ?delay_time:int option ->
  int * int ->
  ColorTable.t ->
  int array ->
  t
(** [of_pixels ?offset ?transparency ?delay_time dimensions palette pixels]
    Creates a new image record from a set of pixel data and a palette. *)

val dimensions : t -> int * int
(** The dimensions of the image. Note that this might be smaller than the
    dimensions of the GIF overall, as animated GIFs can have partial update
    frames. *)

val offset : t -> int * int
(** The offset of this image within the overall GIF dimensions *)

val palette : t -> ColorTable.t
(** The palette of RGB colour entries within the image *)

val pixels : t -> int array
(** The pixels in the palette space of the image *)

val rgb_pixels : t -> (int * int * int) array
(** The pixels in RGB space of the image *)

val transparent : t -> int option
(** If specified, the palette entry that should be treated as transparent for
    this image and not rendered on screen. *)

val delay_time : t -> int option
(** If specified, the time in 1/100ths of a second that should be delayed after
    this frame is rendered. *)

val compressed_image_data : t -> Bytes.t
(** Get the raw compressed image data. Use `pixels` or `rgb_pixels` to get the
    uncompressed data. *)
