type t = {
  x_offset : int;
  y_offset : int;
  width : int;
  height : int;
  palette : ColorTable.t;
  compressed_image_data : Bytes.t;
  lzw_code_size : int;
  interlaced : bool;
  pixels : int array ref;
  transparent : int option;
  delay_time : int option;
}

let v ?offset ?transparent ?delay_time dim palette compressed_image_data
    lzw_code_size interlaced =
  let width, height = dim in
  let x_offset, y_offset = match offset with None -> (0, 0) | Some x -> x in
  let transparent = match transparent with None -> None | Some x -> x in
  let delay_time = match delay_time with None -> None | Some x -> x in
  let pixels = ref [||] in
  {
    x_offset;
    y_offset;
    width;
    height;
    palette;
    compressed_image_data;
    lzw_code_size;
    interlaced;
    pixels;
    transparent;
    delay_time;
  }

let of_pixels ?offset ?transparent ?delay_time dimensions palette pixels =
  let width, height = dimensions in
  if width < 0 || height < 0 then
    raise (Invalid_argument "Dimensions can not be negative");
  if width * height != Array.length pixels then
    raise (Invalid_argument "Dimensions and pixel array have different size");
  let palette_size = ColorTable.size palette in
  if palette_size == 0 then raise (Invalid_argument "Palette has no entries");
  let rec calc_bpp n acc =
    if n > 8 then raise (Invalid_argument "Palette larger than 256 entries");
    match acc >= palette_size with
    | true -> n
    | false -> calc_bpp (n + 1) (acc * 2)
  in
  let bpp = calc_bpp 1 2 in
  let prepped_pixels =
    List.init (Array.length pixels) (fun i -> (Z.of_int pixels.(i), bpp))
  in
  let packed_pixels = Lzw.flatten_codes 8 prepped_pixels in
  let compressed_pixels = Lzw.encode packed_pixels bpp in
  v ?offset ?transparent ?delay_time dimensions palette compressed_pixels bpp
    false

let dimensions i = (i.width, i.height)
let offset i = (i.x_offset, i.y_offset)
let palette i = i.palette
let transparent i = i.transparent
let delay_time i = i.delay_time
let compressed_image_data i = i.compressed_image_data

(* Obrazki z przeplotem maja zmieniona kolejnosc wierszy. Ta funkcja
   zwraca kopie obrazka z prawidlowo uporzadkowanymi wierszami. *)
let deinterlace w h pixels =
  let new_pixels = Array.make (w * h) 0 in
  let copy_row src dest =
    for i = 0 to w - 1 do
      new_pixels.((src * w) + i) <- pixels.((dest * w) + i)
    done
  in
  let i = ref 0 and j = ref 0 in
  while !j < h - 1 do
    copy_row !j !i;
    incr i;
    j := !j + 8
  done;
  j := 4;
  while !j < h - 1 do
    copy_row !j !i;
    incr i;
    j := !j + 8
  done;
  j := 2;
  while !j < h - 1 do
    copy_row !j !i;
    incr i;
    j := !j + 4
  done;
  j := 1;
  while !j < h - 1 do
    copy_row !j !i;
    incr i;
    j := !j + 2
  done;
  new_pixels

(* There are times when we want to know about the image metadata with the pixels
   and so the initial decode is done lazily. *)
let pixels i =
  match i.width * i.height with
  | 0 -> [||]
  | _ -> (
      let p = !(i.pixels) in
      match Array.length p with
      | 0 ->
          let decoded_data =
            Lzw.decode i.compressed_image_data i.lzw_code_size
          in
          if Bytes.length decoded_data != i.width * i.height then
            failwith
              (Printf.sprintf "too few/many pixels: expected %d got %d"
                 (i.width * i.height)
                 (Bytes.length decoded_data));
          let pixels =
            Array.init (Bytes.length decoded_data) (fun i ->
                int_of_char (Bytes.get decoded_data i))
          in
          (match i.interlaced with
          | false -> i.pixels := pixels
          | true -> i.pixels := deinterlace i.width i.height pixels);
          !(i.pixels)
      | _ -> p)

let rgb_pixels i = Array.map (fun p -> i.palette.(p)) (pixels i)
