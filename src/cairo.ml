(* File: cairo.ml

   Copyright (C) 2009

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     WWW: http://math.umh.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License version 3 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)

type status =
  | NO_MEMORY
  | INVALID_RESTORE
  | INVALID_POP_GROUP
  | NO_CURRENT_POINT
  | INVALID_MATRIX
  | INVALID_STATUS
  | NULL_POINTER
  | INVALID_STRING
  | INVALID_PATH_DATA
  | READ_ERROR
  | WRITE_ERROR
  | SURFACE_FINISHED
  | SURFACE_TYPE_MISMATCH
  | PATTERN_TYPE_MISMATCH
  | INVALID_CONTENT
  | INVALID_FORMAT
  | INVALID_VISUAL
  | FILE_NOT_FOUND
  | INVALID_DASH
  | INVALID_DSC_COMMENT
  | INVALID_INDEX
  | CLIP_NOT_REPRESENTABLE
  | TEMP_FILE_ERROR
  | INVALID_STRIDE
  | FONT_TYPE_MISMATCH
  | USER_FONT_IMMUTABLE
  | USER_FONT_ERROR
  | NEGATIVE_COUNT
  | INVALID_CLUSTERS
  | INVALID_SLANT
  | INVALID_WEIGHT

exception Error of status
let () = Callback.register_exception "Cairo.Error" (Error INVALID_RESTORE)

external status_to_string  : status -> string = "caml_cairo_status_to_string"

type t
type surface
type pattern

external create : surface -> t = "caml_cairo_create"
external save : t -> unit = "caml_cairo_save"
external restore : t -> unit = "caml_cairo_restore"

external get_target : t -> surface = "caml_cairo_get_target"

module Group =
struct
  external push_group : t -> unit = "caml_cairo_push_group"
  external push_group_with_content : t -> content -> unit
    = "caml_cairo_push_group_with_content"

  let push ?content cr =
    match content with
    | None -> push_group_stub cr
    | Some c -> push_group_with_content cr c

  external pop : t -> pattern = "caml_cairo_pop_group"
  external pop_to_source : t -> unit = "caml_cairo_pop_group_to_source"

  external get_target : t -> surface = "caml_cairo_get_group_target"
end

external set_source_rgb : t -> r:float -> g:float -> b:float -> unit
  = "caml_cairo_set_source_rgb"

external set_source_rgba : t -> r:float -> g:float -> b:float -> a:float -> unit
  = "caml_cairo_set_source_rgba"

external set_source : t -> Pattern.t -> unit = "caml_cairo_set_source"

external get_source : t -> Pattern.t = "caml_cairo_get_source"

type antialias =
  | ANTIALIAS_DEFAULT
  | ANTIALIAS_NONE
  | ANTIALIAS_GRAY
  | ANTIALIAS_SUBPIXEL

external set_antialias : t -> antialias -> unit = "caml_cairo_set_antialias"
external get_antialias : t -> antialias = "caml_cairo_get_antialias"

external set_dash_stub : t -> float array -> osf:float -> unit
  = "caml_cairo_set_dash"

let set_dash cr ?(ofs=0.0) dashes = set_dash_stub cr ofs dashes

external get_dash : t -> float array * float = "caml_cairo_get_dash"

external set_fill_rule : t -> fill_rule -> unit = "caml_cairo_set_fill_rule"
external get_fill_rule : t -> fill_rule = "caml_cairo_get_fill_rule"

external set_line_cap : t -> line_cap -> unit = "caml_cairo_set_line_cap"
external get_line_cap : t -> line_cap = "caml_cairo_get_line_cap"

external set_line_join : t -> line_join -> unit = "caml_cairo_set_line_join"
external get_line_join : t -> line_join = "caml_cairo_get_line_join"

external set_line_width : t -> float -> unit = "caml_cairo_set_line_width"
external get_line_width : t -> float = "caml_cairo_get_line_width"

external set_miter_limit : t -> float -> unit = "caml_cairo_set_miter_limit"
external get_miter_limit : t -> float = "caml_cairo_get_miter_limit"

external set_operator : t -> operator -> unit = "caml_cairo_set_operator"
external get_operator : t -> operator = "caml_cairo_get_operator"

external set_tolerance : t -> float -> unit = "caml_cairo_set_tolerance"
external get_tolerance : t -> float = "caml_cairo_get_tolerance"

external clip_stub : t -> unit = "caml_cairo_clip"
external clip_preserve : t -> unit = "caml_cairo_clip_preserve"

let clip ?(preserve=false) cr =
  if preserve then clip_preserve cr else clip_stub cr

external clip_extents : t -> bounding_box = "caml_cairo_clip_extents"

external clip_reset : t -> unit = "caml_cairo_reset_clip"

external clip_rectangle_list : t -> rectangle list
  = "caml_cairo_copy_clip_rectangle_list"

external fill_stub : t -> unit = "caml_cairo_fill"
external fill_preserve : t -> unit = "caml_cairo_fill_preserve"

let fill ?(preserve=false) cr =
  if preserve then fill_preserve cr else fill_stub cr

external fill_extents : t -> bounding_box = "caml_cairo_fill_extents"

external in_fill : t -> x:float -> y:float -> bool = "caml_cairo_in_fill"

external mask : t -> pattern -> unit = "caml_cairo_mask"
external mask_surface : t -> surface -> x:float -> y:float -> unit
  = "caml_cairo_mask_surface"

external paint_stub : t -> unit = "caml_cairo_paint"
external paint_with_alpha : t -> float -> unit = "caml_cairo_paint_with_alpha"

let paint ?alpha cr =
  match alpha with
  | None -> paint_stub cr
  | Some a -> paint_with_alpha cr a

external stroke_stub : t -> unit = "caml_cairo_stroke"
external stroke_preserve : t -> unit = "caml_cairo_stroke_preserve"

let stroke ?(preserve=false) cr =
  if preserve then stroke_preserve cr else stroke_stub cr

external stroke_extents : t -> bounding_box = "caml_cairo_stroke_extents"

external in_stroke : t -> x:float -> y:float -> bool = "caml_cairo_in_stroke"

external copy_page : t -> unit = "caml_cairo_copy_page"
external show_page : t -> unit = "caml_cairo_show_page"


(* ---------------------------------------------------------------------- *)

module Surface =
struct
  type t = surface

  type content = COLOR | ALPHA | COLOR_ALPHA
end

(* ---------------------------------------------------------------------- *)

module Pattern =
struct
  type t = pattern


end
