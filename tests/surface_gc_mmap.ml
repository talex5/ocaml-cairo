open Printf
open Cairo

(* Test that using the ref-count of the surface to express its
   dependency on the context works. *)
let mmap_context () =
  let s = 8 in
  let fd = Unix.openfile "image.mmap" [O_RDWR; O_CREAT] 0o600 in
  Unix.unlink "image.mmap";
  Unix.ftruncate fd (s * s * 4);
  let data =
    Unix.map_file fd Int32 C_layout true [| s; s |]
    |> Bigarray.array2_of_genarray
  in
  let surf = Image.create_for_data32 data in
  Gc.finalise (fun _ -> eprintf "`surf' is collected by the GC.\n%!") surf;
  create surf

let () =
  Gc.compact();  Gc.compact();
  let cr = mmap_context() in
  Gc.finalise (fun _ -> eprintf "`cr' is collected by the GC.\n%!") cr;
  printf "`surf' should be garbage collected but the surface still held \
	by `cr'.\n%!";
  Gc.compact();  Gc.compact();
  Surface.finish(get_target cr);
  printf "`cr' should be garbage collected.\n%!";
  Gc.compact();  Gc.compact()
