open OUnit2
open Giflib

let test_example_file _ =
  let g = GIF.from_file "../../../test/testdata/test1.gif" in
  assert_equal ~msg:"image count" 1 (GIF.image_count g);
  assert_equal ~msg:"screen dims" (16, 16) (GIF.dimensions g);
  let i = GIF.get_image g 0 in
  let w, h = Image.dimensions i in
  assert_equal ~msg:"dimensions" (16, 16) (w, h);
  assert_equal ~msg:"offset" (0, 0) (Image.offset i);
  assert_equal ~msg:"transparent" (Some 2) (Image.transparent i);
  assert_equal ~msg:"delay" (Some 0) (Image.delay_time i);
  let pixels = Image.pixels i in
  assert_equal (w * h) (Array.length pixels);
  for y = 0 to h - 1 do
    for x = 0 to w - 1 do
      let expected = 1 - (y / 2 mod 2) in
      let v = pixels.((y * w) + x) in
      assert_equal v expected
    done
  done;
  let palette = Image.palette i in
  assert_equal ~msg:"palette size" ~printer:string_of_int
    (ColorTable.size palette) 4;
  assert_equal ~msg:"black pixel" (0, 0, 0) (ColorTable.get palette 1);
  assert_equal ~msg:"orange pixel" (255, 118, 17) (ColorTable.get palette 0)

let test_get_image_fail _ =
  let g = GIF.from_file "../../../test/testdata/test1.gif" in
  assert_raises (GIF.Error "get_image: frame not found") (fun () ->
      GIF.get_image g 1)

let test_read_image_twice _ =
  let g = GIF.from_file "../../../test/testdata/test1.gif" in
  let _ = GIF.get_image g 0 in
  let i = GIF.get_image g 0 in
  let w, h = Image.dimensions i in
  assert_equal ~msg:"dimensions" (16, 16) (w, h);
  let pixels = Image.pixels i in
  assert_equal (w * h) (Array.length pixels);
  assert_equal (ColorTable.size (Image.palette i)) 4

let test_read_mono_image _ =
  let g = GIF.from_file "../../../test/testdata/flitter.gif" in
  assert_equal 1 (GIF.image_count g);
  assert_equal ~msg:"screen dims" (640, 480) (GIF.dimensions g);
  let i = GIF.get_image g 0 in
  let w, h = Image.dimensions i in
  assert_equal ~msg:"resolution" (640, 480) (w, h);
  let pixels = Image.pixels i in
  assert_equal ~msg:"pixel count" (w * h) (Array.length pixels);
  assert_equal ~msg:"palette" ~printer:string_of_int 256
    (ColorTable.size (Image.palette i))

let test_write_image_6_bpp _ =
  let width = 100 and height = 100 in
  let colours = 64 and bpp = 6 in
  let temp_dir = Filename.temp_dir "test" "write" in
  let colour_table = Array.init colours (fun i -> (i, i, i)) in
  let pixels =
    List.init (width * height) (fun i -> (Z.of_int (i mod colours), bpp))
  in
  let packed_pixels = Lzw.flatten_codes 8 pixels in
  let compressed = Lzw.encode packed_pixels bpp in
  let image = Image.v (width, height) colour_table compressed bpp false in
  let src_gif = GIF.from_image image in
  let filename = temp_dir ^ "/6bpp.gif" in
  GIF.to_file src_gif filename;

  let dst_gif = GIF.from_file filename in
  assert_equal 1 (GIF.image_count dst_gif);
  assert_equal ~msg:"screen dims" (width, height) (GIF.dimensions dst_gif)

let test_write_image_8_bpp _ =
  let width = 100 and height = 100 in
  let colours = 256 and bpp = 8 in
  let temp_dir = Filename.temp_dir "test" "write" in
  let colour_table = Array.init colours (fun i -> (i, i, i)) in
  let pixels =
    List.init (width * height) (fun i -> (Z.of_int (i mod colours), bpp))
  in
  let packed_pixels = Lzw.flatten_codes 8 pixels in
  let compressed = Lzw.encode packed_pixels bpp in
  let image = Image.v (width, height) colour_table compressed bpp false in
  let src_gif = GIF.from_image image in
  let filename = temp_dir ^ "/6bpp.gif" in
  GIF.to_file src_gif filename;

  let dst_gif = GIF.from_file filename in
  assert_equal 1 (GIF.image_count dst_gif);
  assert_equal ~msg:"screen dims" (width, height) (GIF.dimensions dst_gif)

let suite =
  "BasicLoading"
  >::: [
         "Load file" >:: test_example_file;
         "Fail get imagae" >:: test_get_image_fail;
         "Test re-read image" >:: test_read_image_twice;
         "Test read mono image" >:: test_read_mono_image;
         "Test write 6 bpp image" >:: test_write_image_6_bpp;
         "Test write 8 bpp image" >:: test_write_image_8_bpp;
       ]

let () = run_test_tt_main suite
