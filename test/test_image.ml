open OUnit2
open Giflib

let test_image_too_few_colors _ =
  let width = 100 and height = 100 in
  let colour_table = [||] in
  let pixels = Array.init (width * height) (fun _ -> 0) in
  assert_raises (Invalid_argument "Palette has no entries") (fun () ->
      Image.of_pixels (width, height) colour_table pixels)

let test_image_too_many_colors _ =
  let width = 100 and height = 100 in
  let colour_table = Array.init 257 (fun i -> (i, i, i)) in
  let pixels = Array.init (width * height) (fun _ -> 0) in
  assert_raises (Invalid_argument "Palette larger than 256 entries") (fun () ->
      Image.of_pixels (width, height) colour_table pixels)

let test_dimensions_wrong _ =
  let width = 100 and height = 100 in
  let colour_table = Array.init 32 (fun i -> (i, i, i)) in
  let pixels = Array.init (width * height) (fun _ -> 0) in
  assert_raises
    (Invalid_argument "Dimensions and pixel array have different size")
    (fun () -> Image.of_pixels (width + 1, height + 1) colour_table pixels)

let suite =
  "Image.t tests"
  >::: [
         "Test too few colors" >:: test_image_too_few_colors;
         "Test too many colors" >:: test_image_too_many_colors;
         "Test dimensions are wrong" >:: test_dimensions_wrong;
       ]

let () = run_test_tt_main suite
