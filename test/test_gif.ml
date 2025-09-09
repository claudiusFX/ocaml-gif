open OUnit2
open Giflib

let test_example_file _ =
  let g = GIF.from_file "testdata/test1.gif" in
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
  let g = GIF.from_file "testdata/test1.gif" in
  assert_raises (GIF.Error "get_image: frame not found") (fun () ->
      GIF.get_image g 1)

let test_read_image_twice _ =
  let g = GIF.from_file "testdata/test1.gif" in
  let _ = GIF.get_image g 0 in
  let i = GIF.get_image g 0 in
  let w, h = Image.dimensions i in
  assert_equal ~msg:"dimensions" (16, 16) (w, h);
  let pixels = Image.pixels i in
  assert_equal (w * h) (Array.length pixels);
  assert_equal (ColorTable.size (Image.palette i)) 4

let test_read_mono_image _ =
  let g = GIF.from_file "testdata/flitter.gif" in
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

  let img = GIF.get_image src_gif 0 in
  let palette = Image.palette img in
  assert_equal ~msg:"palette size" ~printer:string_of_int colours
    (ColorTable.size palette);

  let dst_gif = GIF.from_file filename in
  assert_equal 1 (GIF.image_count dst_gif);
  assert_equal ~msg:"screen dims" (width, height) (GIF.dimensions dst_gif);

  let img = GIF.get_image dst_gif 0 in
  let palette = Image.palette img in
  assert_equal ~msg:"palette size" ~printer:string_of_int colours
    (ColorTable.size palette)

let test_write_image_of_pixels_6_bpp _ =
  let width = 100 and height = 100 in
  let colours = 64 in
  let temp_dir = Filename.temp_dir "test" "write" in
  let colour_table = Array.init colours (fun i -> (i, i, i)) in
  let pixels = Array.init (width * height) (fun i -> i mod colours) in
  let image = Image.of_pixels (width, height) colour_table pixels in
  let src_gif = GIF.from_image image in
  let filename = temp_dir ^ "/6bpp.gif" in
  GIF.to_file src_gif filename;

  let img = GIF.get_image src_gif 0 in
  let palette = Image.palette img in
  assert_equal ~msg:"palette size" ~printer:string_of_int colours
    (ColorTable.size palette);

  let dst_gif = GIF.from_file filename in
  assert_equal 1 (GIF.image_count dst_gif);
  assert_equal ~msg:"screen dims" (width, height) (GIF.dimensions dst_gif);

  let img = GIF.get_image dst_gif 0 in
  let palette = Image.palette img in
  assert_equal ~msg:"palette size" ~printer:string_of_int colours
    (ColorTable.size palette)

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

  let img = GIF.get_image src_gif 0 in
  let palette = Image.palette img in
  assert_equal ~msg:"palette size" ~printer:string_of_int colours
    (ColorTable.size palette);

  let dst_gif = GIF.from_file filename in
  assert_equal 1 (GIF.image_count dst_gif);
  assert_equal ~msg:"screen dims" (width, height) (GIF.dimensions dst_gif);

  let img = GIF.get_image dst_gif 0 in
  let palette = Image.palette img in
  assert_equal ~msg:"palette size" ~printer:string_of_int colours
    (ColorTable.size palette)

let test_write_image_of_pixels_8_bpp _ =
  let width = 100 and height = 100 in
  let colours = 256 in
  let temp_dir = Filename.temp_dir "test" "write" in
  let colour_table = Array.init colours (fun i -> (i, i, i)) in
  let pixels = Array.init (width * height) (fun i -> i mod colours) in
  let image = Image.of_pixels (width, height) colour_table pixels in
  let src_gif = GIF.from_image image in
  let filename = temp_dir ^ "/6bpp.gif" in
  GIF.to_file src_gif filename;

  let img = GIF.get_image src_gif 0 in
  let palette = Image.palette img in
  assert_equal ~msg:"palette size" ~printer:string_of_int colours
    (ColorTable.size palette);

  let dst_gif = GIF.from_file filename in
  assert_equal 1 (GIF.image_count dst_gif);
  assert_equal ~msg:"screen dims" (width, height) (GIF.dimensions dst_gif);

  let img = GIF.get_image dst_gif 0 in
  let palette = Image.palette img in
  assert_equal ~msg:"palette size" ~printer:string_of_int colours
    (ColorTable.size palette)

let test_write_image_non_power_2_colors _ =
  let width = 100 and height = 100 in
  let colours = 20 in
  let temp_dir = Filename.temp_dir "test" "write" in
  let colour_table = Array.init colours (fun i -> (i, i, i)) in
  let pixels = Array.init (width * height) (fun i -> i mod colours) in
  let image = Image.of_pixels (width, height) colour_table pixels in
  let src_gif = GIF.from_image image in
  let filename = temp_dir ^ "/6bpp.gif" in
  GIF.to_file src_gif filename;

  let img = GIF.get_image src_gif 0 in
  let palette = Image.palette img in
  let expected_colors = 32 in
  assert_equal ~msg:"palette size" ~printer:string_of_int expected_colors
    (ColorTable.size palette);

  let dst_gif = GIF.from_file filename in
  assert_equal 1 (GIF.image_count dst_gif);
  assert_equal ~msg:"screen dims" (width, height) (GIF.dimensions dst_gif);

  let img = GIF.get_image dst_gif 0 in
  let palette = Image.palette img in
  assert_equal ~msg:"palette size" ~printer:string_of_int expected_colors
    (ColorTable.size palette)

let test_write_animation_8_bpp _ =
  let width = 100 and height = 100 in
  let colours = 256 and bpp = 8 in
  let temp_dir = Filename.temp_dir "test" "write" in
  let colour_table = Array.init colours (fun i -> (i, i, i)) in

  let make_image frame_index =
    let pixels =
      List.init (width * height) (fun i ->
          (Z.of_int ((i + frame_index) mod colours), bpp))
    in
    let packed = Lzw.flatten_codes bpp pixels in
    let compressed = Lzw.encode packed bpp in
    Image.v ~delay_time:(Some 5) (width, height) colour_table compressed bpp
      false
  in

  let frames = [ make_image 0; make_image 1; make_image 2 ] in
  let gif = GIF.from_images frames in
  let filename = temp_dir ^ "/animated.gif" in
  GIF.to_file gif filename;

  let dst_gif = GIF.from_file filename in
  assert_equal ~msg:"frame count" 3 (GIF.image_count dst_gif);
  assert_equal ~msg:"screen dims" (width, height) (GIF.dimensions dst_gif);

  (* Local helper to access a pixel at (x, y) *)
  let get_pixel img x y =
    let w, _ = Image.dimensions img in
    let pixels = Image.pixels img in
    pixels.((y * w) + x)
  in

  (* Check for first pixel in each frame to confirm correct ordering *)
  List.iteri
    (fun idx expected_offset ->
      let img = GIF.get_image dst_gif idx in
      let actual = get_pixel img 0 0 in
      let expected = expected_offset mod colours in
      assert_equal
        ~msg:(Printf.sprintf "frame %d: pixel (0,0)" idx)
        expected actual)
    [ 0; 1; 2 ]

let suite =
  "BasicLoading"
  >::: [
         "Load file" >:: test_example_file;
         "Fail get imagae" >:: test_get_image_fail;
         "Test re-read image" >:: test_read_image_twice;
         "Test read mono image" >:: test_read_mono_image;
         "Test write 6 bpp image" >:: test_write_image_6_bpp;
         "Test write 6 bpp image using of_pixels"
         >:: test_write_image_of_pixels_6_bpp;
         "Test write 8 bpp image" >:: test_write_image_8_bpp;
         "Test write 8 bpp image using of_pixels"
         >:: test_write_image_of_pixels_8_bpp;
         "Test write 20 color image" >:: test_write_image_non_power_2_colors;
         "Test write 8 bpp animation" >:: test_write_animation_8_bpp;
       ]

let () = run_test_tt_main suite
