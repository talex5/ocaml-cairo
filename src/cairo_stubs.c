/* File: cairo_stubs.c

   Copyright (C) 2009

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     WWW: http://math.umh.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License version 3 or
   later as published by the Free Software Foundation.  See the file
   LICENCE for more details.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. */

#include <cairo.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/signals.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/intext.h>
#include <caml/bigarray.h>

#include "cairo_macros.c"
#include "cairo_ocaml_types.c"

/* cairo_t functions.
***********************************************************************/

CAMLexport value caml_cairo_create(value vsurf)
{
  CAMLparam1(vsurf);
  CAMLlocal1(vcontext);
  cairo_t *cr;

  cr = cairo_create(SURFACE_VAL(vsurf));
  caml_check_status(cr);
  CAIRO_ASSIGN(vcontext, cr);
  CAMLreturn(vcontext);
}

DO_FUNCTION(cairo_save)
DO_FUNCTION(cairo_restore)

CAMLexport value caml_cairo_get_target(value vcr)
{
  CAMLparam1(vcr);
  CAMLlocal1(vsurf);
  cairo_t *cr = CAIRO_VAL(vcr);
  cairo_surface_t* s = cairo_get_target(cr);
  SURFACE_ASSIGN(vsurf, s);
  caml_check_status(cr);
  CAMLreturn(vsurf);
}

DO_FUNCTION(cairo_push_group)

CAMLexport value caml_cairo_push_group_with_content(value vcr, value vcontent)
{
  CAMLparam2(vcr, vcontent);
  cairo_t *cr = CAIRO_VAL(vcr);
  cairo_content_t content;
  CONTENT_ASSIGN(content, vcontent);
  cairo_push_group_with_content(cr, content);
  caml_check_status(cr);
  CAMLreturn(Val_unit);
}

CAMLexport value caml_cairo_pop_group(value vcr)
{
  CAMLparam1(vcr);
  CAMLlocal1(vpat);
  cairo_t *cr = CAIRO_VAL(vcr);
  cairo_pattern_t* pat = cairo_pop_group(cr);
  caml_check_status(cr);
  PATTERN_ASSIGN(vpat, pat);
  CAMLreturn(vpat);
}

DO_FUNCTION(cairo_pop_group_to_source)

CAMLexport value caml_cairo_get_group_target(value vcr)
{
  CAMLparam1(vcr);
  CAMLlocal1(vsurf);
  cairo_t* cr = CAIRO_VAL(vcr);
  cairo_surface_t* s = cairo_get_group_target(cr);
  caml_check_status(cr);
  SURFACE_ASSIGN(vsurf, s);
  CAMLreturn(vsurf);
}

DO3_FUNCTION(cairo_set_source_rgb, Double_val, Double_val, Double_val)

DO4_FUNCTION(cairo_set_source_rgba, Double_val, Double_val,
             Double_val, Double_val)

DO3_FUNCTION(cairo_set_source_surface, SURFACE_VAL, Double_val, Double_val)

DO1_FUNCTION(cairo_set_source, PATTERN_VAL)



CAMLexport value caml_cairo_get_source(value vcr)
{
  CAMLparam1(vcr);
  CAMLlocal1(vpat);
  cairo_t* cr = CAIRO_VAL(vcr);
  cairo_pattern_t* pat = cairo_get_source(cr);
  caml_check_status(cr);
  PATTERN_ASSIGN(vpat, pat);
  CAMLreturn(vpat);
}


#define ANTIALIAS_VAL(v) Int_val(v)
#define VAL_ANTIALIAS(v) Val_int(v)

DO1_FUNCTION(cairo_set_antialias, ANTIALIAS_VAL)
GET_FUNCTION(cairo_get_antialias, VAL_ANTIALIAS, cairo_antialias_t)

CAMLexport value caml_cairo_set_dash(value vcr, value vdashes, value voffset)
{
  CAMLparam3(vcr, vdashes, voffset);
  cairo_t* cr = CAIRO_VAL(vcr);
  double *dashes;
  const int num_dashes = FLOAT_ARRAY_LENGTH(vdashes);
  int i;
  for(i = 0; i < num_dashes; i++)  dashes[i] = Double_field(vdashes, i);
  cairo_set_dash(cr, dashes, num_dashes, Double_val(voffset));
  caml_check_status(cr);
  CAMLreturn(Val_unit);
}

CAMLexport value caml_cairo_get_dash(value vcr)
{
  CAMLparam1(vcr);
  CAMLlocal2(couple, vdashes);
  cairo_t* cr = CAIRO_VAL(vcr);
  int num_dashes = cairo_get_dash_count(cr);
  double *dashes;
  double offset;
  int i;

  couple = caml_alloc_tuple(2);
  if (num_dashes == 0) { /* return ([||], 0.) */
    Store_field(couple, 0, caml_alloc_tuple(0));  /* [||] */
    Store_field(couple, 1, caml_copy_double(0.0));
  }
  else {
    dashes = malloc(num_dashes * sizeof(double));
    cairo_get_dash(cr, dashes, &offset);
    vdashes = caml_alloc(num_dashes * Double_wosize, Double_array_tag);
    for(i = 0; i < num_dashes; i++)
      Store_double_field(vdashes, i, dashes[i]);
    Store_field(couple, 0, vdashes);
    Store_field(couple, 1, caml_copy_double(offset));
    free(dashes);
  }
  CAMLreturn(couple);
}

#define FILL_RULE_VAL(v) Int_val(v)
#define VAL_FILL_RULE(v) Val_int(v)

DO1_FUNCTION(cairo_set_fill_rule, FILL_RULE_VAL)
GET_FUNCTION(cairo_get_fill_rule, VAL_FILL_RULE, cairo_fill_rule_t)

#define LINE_CAP_VAL(v) Int_val(v)
#define VAL_LINE_CAP(v) Val_int(v)

DO1_FUNCTION(cairo_set_line_cap, FILL_RULE_VAL)
GET_FUNCTION(cairo_get_line_cap, VAL_LINE_CAP, cairo_line_cap_t)

#define LINE_JOIN_VAL(v) Int_val(v)
#define VAL_LINE_JOIN(v) Val_int(v)

DO1_FUNCTION(cairo_set_line_join, LINE_JOIN_VAL)
GET_FUNCTION(cairo_get_line_join, VAL_LINE_JOIN, cairo_line_join_t)

DO1_FUNCTION(cairo_set_line_width, Double_val)
GET_FUNCTION(cairo_get_line_width, caml_copy_double, double)

DO1_FUNCTION(cairo_set_miter_limit, Double_val)
GET_FUNCTION(cairo_get_miter_limit, caml_copy_double, double)

#define OPERATOR_VAL(v) Int_val(v)
#define VAL_OPERATOR(v) Val_int(v)

DO1_FUNCTION(cairo_set_operator, OPERATOR_VAL)
GET_FUNCTION(cairo_get_operator, VAL_OPERATOR, cairo_operator_t)

DO1_FUNCTION(cairo_set_tolerance, Double_val)
GET_FUNCTION(cairo_get_tolerance, caml_copy_double, double)

DO_FUNCTION(cairo_clip)
DO_FUNCTION(cairo_clip_preserve)
GET_EXTENTS(cairo_clip_extents)
DO_FUNCTION(cairo_reset_clip)

CAMLexport value caml_cairo_copy_clip_rectangle_list(value vcr)
{
  CAMLparam1(vcr);
  CAMLlocal3(vlist, vrec, cons);
  cairo_t* cr = CAIRO_VAL(vcr);
  cairo_rectangle_list_t* list = cairo_copy_clip_rectangle_list(cr);
  int i;
  cairo_rectangle_t *r;
  /* assert(list != NULL); */
  caml_raise_Error(list->status);
  vlist = Val_int(0); /* [] */
  for(i = 0, r = list->rectangles;  i < list->num_rectangles;  i++, r++) {
    /* New rectangle (pure float record) */
    vrec = caml_alloc(4 * Double_wosize, Double_array_tag);
    Store_double_field(vrec, 0, r->x);
    Store_double_field(vrec, 1, r->y);
    Store_double_field(vrec, 2, r->width);
    Store_double_field(vrec, 3, r->height);
    /* New cons cell */
    cons = caml_alloc_tuple(2);
    Store_field(cons, 0, vrec);
    Store_field(cons, 1, vlist);
    vlist = cons;
  }
  cairo_rectangle_list_destroy(list);
  CAMLreturn(vlist);
}


DO_FUNCTION(cairo_fill)
DO_FUNCTION(cairo_fill_preserve)

GET_EXTENTS(cairo_fill_extents)

CAMLexport value caml_cairo_in_fill(value vcr, value vx, value vy)
{
  CAMLparam3(vcr, vx, vy);
  cairo_t* cr = CAIRO_VAL(vcr);
  cairo_bool_t b = cairo_in_fill(cr, Double_val(vx), Double_val(vy));
  caml_check_status(cr);
  /* doc of cairo_bool_t: b=0 or 1 */
  CAMLreturn(Val_int(b));
}

DO1_FUNCTION(cairo_mask, PATTERN_VAL)

CAMLexport value caml_cairo_mask_surface(value vcr, value vsurf,
                                         value vx, value vy)
{
  CAMLparam4(vcr, vsurf, vx, vy);
  cairo_t* cr = CAIRO_VAL(vcr);
  cairo_mask_surface(cr, SURFACE_VAL(vsurf), Double_val(vx), Double_val(vy));
  caml_check_status(cr);
  CAMLreturn(Val_unit);
}

DO_FUNCTION(cairo_paint)
DO1_FUNCTION(cairo_paint_with_alpha, Double_val)

DO_FUNCTION(cairo_stroke)
DO_FUNCTION(cairo_stroke_preserve)

GET_EXTENTS(cairo_stroke_extents)

CAMLexport value caml_cairo_in_stroke(value vcr, value vx, value vy)
{
  CAMLparam3(vcr, vx, vy);
  cairo_t* cr = CAIRO_VAL(vcr);
  cairo_bool_t b = cairo_in_stroke(cr, Double_val(vx), Double_val(vy));
  caml_check_status(cr);
  /* doc of cairo_bool_t: b=0 or 1 */
  CAMLreturn(Val_int(b));
}

DO_FUNCTION(cairo_copy_page)
DO_FUNCTION(cairo_show_page)

/* TODO: cairo_set_user_data, cairo_get_user_data */

/* Paths -- Creating paths and manipulating path data
***********************************************************************/

CAMLexport value caml_cairo_copy_path(value vcr)
{
  CAMLparam1(vcr);
  CAMLlocal1(vpath);
  cairo_t* cr = CAIRO_VAL(vcr);
  cairo_path_t* path = cairo_copy_path(cr);
  caml_raise_Error(path->status);
  PATH_ASSIGN(vpath, path);
  CAMLreturn(vpath);
}

CAMLexport value caml_cairo_copy_path_flat(value vcr)
{
  CAMLparam1(vcr);
  CAMLlocal1(vpath);
  cairo_t* cr = CAIRO_VAL(vcr);
  cairo_path_t* path = cairo_copy_path_flat(cr);
  caml_raise_Error(path->status);
  PATH_ASSIGN(vpath, path);
  CAMLreturn(vpath);
}

DO1_FUNCTION(cairo_append_path, PATH_VAL)

CAMLexport value caml_cairo_get_current_point(value vcr)
{
  CAMLparam1(vcr);
  CAMLlocal1(vcouple);
  cairo_t* cr = CAIRO_VAL(vcr);
  double x, y;
  cairo_get_current_point(cr, &x, &y);
  caml_check_status(cr);
  /* Couple (x,y) */
  vcouple = caml_alloc_tuple(2);
  Store_field(vcouple, 0, caml_copy_double(x));
  Store_field(vcouple, 1, caml_copy_double(y));
  CAMLreturn(vcouple);
}

DO_FUNCTION(cairo_new_path)
DO_FUNCTION(cairo_new_sub_path)
DO_FUNCTION(cairo_close_path)

CAMLexport value caml_cairo_glyph_path(value vcr, value vglyphs)
{
  CAMLparam2(vcr, vglyphs);
  cairo_t* cr = CAIRO_VAL(vcr);
  int num_glyphs = Wosize_val(vglyphs);
  cairo_glyph_t *glyphs, *p;
  int i;

  glyphs = malloc(num_glyphs * sizeof(cairo_glyph_t));
  for(i=0, p = glyphs; i < num_glyphs; i++, p++) {
    SET_GLYPH_VAL(p, Field(vglyphs, i));
  }
  cairo_glyph_path(cr, glyphs, num_glyphs);
  free(glyphs);
  caml_check_status(cr);
  CAMLreturn(Val_unit);
}

DO1_FUNCTION(cairo_text_path, String_val)
GET_EXTENTS(cairo_path_extents)

DO5_FUNCTION(cairo_arc, Double_val, Double_val, Double_val, Double_val,
             Double_val)
DO5_FUNCTION(cairo_arc_negative, Double_val, Double_val, Double_val,
             Double_val, Double_val)
DO6_FUNCTION(cairo_curve_to, Double_val, Double_val, Double_val,
             Double_val, Double_val, Double_val)
DO2_FUNCTION(cairo_line_to, Double_val, Double_val)
DO2_FUNCTION(cairo_move_to, Double_val, Double_val)
DO4_FUNCTION(cairo_rectangle, Double_val, Double_val, Double_val, Double_val)

DO6_FUNCTION(cairo_rel_curve_to, Double_val, Double_val, Double_val,
             Double_val, Double_val, Double_val)
DO2_FUNCTION(cairo_rel_line_to, Double_val, Double_val)
DO2_FUNCTION(cairo_rel_move_to, Double_val, Double_val)


/* Interacting with the paths content from OCaml. */

CAMLexport value caml_cairo_path_fold(value vpath, value fn, value va)
{
  CAMLparam3(vpath, fn, va);
  CAMLlocal2(vacc, vdata);
  cairo_path_t * path = PATH_VAL(vpath);
  cairo_path_data_t *data;
  int i;

  vacc = va;
  for(i = 0; i < path->num_data; i += path->data[i].header.length) {
    data = &path->data[i];
    PATH_DATA_ASSIGN(vdata, data);
    vdata = caml_callback2(fn, vacc, vdata);
  }
  CAMLreturn(vacc);
}

CAMLexport value caml_cairo_path_to_array(value vpath)
{
  CAMLparam1(vpath);
  CAMLlocal2(varray, vdata);
  cairo_path_t * path = PATH_VAL(vpath);
  cairo_path_data_t *data;
  int i;

  varray = caml_alloc_tuple(path->num_data);
  for(i = 0; i < path->num_data; i += path->data[i].header.length) {
    data = &path->data[i];
    PATH_DATA_ASSIGN(vdata, data);
    Store_field(varray, i, vdata);
  }
  CAMLreturn(varray);
}

CAMLexport value caml_cairo_path_of_array(value varray)
{
  CAMLparam1(varray);
  CAMLlocal2(vpath, vdata);
  int length = Wosize_val(varray);
  cairo_path_t* path;
  cairo_path_data_t *data;
  int i, num_data;

  path = malloc(sizeof(cairo_path_t));
  path->status = CAIRO_STATUS_SUCCESS;
  path->num_data = num_data;
  /* Compute the total length */
  num_data = 0;
#define ADD1 num_data += 1
#define ADD2(x,y) num_data += 2 /* 1 header + 1 point */
#define ADD4(x1,y1, x2,y2, x3,y3) num_data += 4 /* 1 header + 3 point */
  for(i = 0; i < length; i++) {
    vdata = Field(varray, i);
    SWITCH_PATH_DATA(vdata, ADD2, ADD2, ADD4, ADD1);
  }

#define MOVE(x1,y1)                             \
  data->header.type = CAIRO_PATH_MOVE_TO;       \
  data->header.length = 2;                      \
  data[1].point.x = caml_copy_double(x1);       \
  data[1].point.y = caml_copy_double(y1)
#define LINE(x1,y1)                             \
  data->header.type = CAIRO_PATH_LINE_TO;       \
  data->header.length = 2;                      \
  data[1].point.x = caml_copy_double(x1);       \
  data[1].point.y = caml_copy_double(y1)
#define CURVE(x1,y1, x2,y2, x3,y3)              \
  data->header.type = CAIRO_PATH_CURVE_TO;      \
  data->header.length = 4;                      \
  data[1].point.x = caml_copy_double(x1);       \
  data[1].point.y = caml_copy_double(y1);       \
  data[2].point.x = caml_copy_double(x2);       \
  data[2].point.y = caml_copy_double(y2);       \
  data[3].point.x = caml_copy_double(x3);       \
  data[3].point.y = caml_copy_double(y3)
#define CLOSE                                   \
  data->header.type = CAIRO_PATH_CLOSE_PATH;    \
  data->header.length = 1;

  path->data = malloc(num_data * sizeof(cairo_path_data_t));
  for(i = 0; i < num_data; i += data->header.length) {
    vdata = Field(varray, i);
    data = &path->data[i];
    SWITCH_PATH_DATA(vdata,MOVE, LINE, CURVE, CLOSE);
  }
  PATH_ASSIGN(vpath, path);
  CAMLreturn(vpath);
}


/* Patterns -- Sources for drawing
***********************************************************************/

CAMLexport value caml_cairo_pattern_add_color_stop_rgb
(value vpat, value vofs, value vr, value vg, value vb)
{
  /* noalloc */
  cairo_pattern_add_color_stop_rgb(PATTERN_VAL(vpat), Double_val(vofs),
                                   Double_val(vr), Double_val(vg),
                                   Double_val(vb));
  return(Val_unit);
}


CAMLexport value caml_cairo_pattern_add_color_stop_rgba
(value vpat, value vofs, value vr, value vg, value vb, value va)
{
  /* noalloc */
  cairo_pattern_add_color_stop_rgba(PATTERN_VAL(vpat), Double_val(vofs),
                                    Double_val(vr), Double_val(vg),
                                    Double_val(vb), Double_val(va));
  return(Val_unit);
}

CAMLexport value caml_cairo_pattern_add_color_stop_rgba_bc
(value * argv, int argn)
{
  return caml_cairo_pattern_add_color_stop_rgba
    (argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}


CAMLexport value caml_cairo_pattern_get_color_stop_count(value vpat)
{
  CAMLparam1(vpat);
  int count;
  cairo_status_t st = cairo_pattern_get_color_stop_count(PATTERN_VAL(vpat),
                                                         &count);
  caml_raise_Error(st);
  CAMLreturn(Val_int(count));
}

CAMLexport value caml_cairo_pattern_get_color_stop_rgba(value vpat,
                                                        value vidx)
{
  CAMLparam2(vpat, vidx);
  CAMLlocal1(vcolors);
  double offset, red, green, blue, alpha;
  cairo_status_t st = cairo_pattern_get_color_stop_rgba
    (PATTERN_VAL(vpat), Int_val(vidx), &offset, &red, &green, &blue, &alpha);
  caml_raise_Error(st);
  /* tuple (offset, red, green, blue, alpha) */
  vcolors = caml_alloc_tuple(5);
  Store_field(vcolors, 0, caml_copy_double(offset));
  Store_field(vcolors, 1, caml_copy_double(red));
  Store_field(vcolors, 2, caml_copy_double(green));
  Store_field(vcolors, 3, caml_copy_double(blue));
  Store_field(vcolors, 4, caml_copy_double(alpha));
  CAMLreturn(vcolors);
}

CAMLexport value caml_cairo_pattern_create_rgb(value vr, value vg, value vb)
{
  CAMLparam3(vr,vg,vb);
  CAMLlocal1(vpat);
  cairo_pattern_t* pat = cairo_pattern_create_rgb(Double_val(vr),
                                                  Double_val(vg),
                                                  Double_val(vb));
  caml_raise_Error(cairo_pattern_status(pat));
  PATTERN_ASSIGN(vpat, pat);
  CAMLreturn(vpat);
}

CAMLexport value caml_cairo_pattern_create_rgba(value vr, value vg, value vb,
                                                value va)
{
  CAMLparam4(vr,vg,vb,va);
  CAMLlocal1(vpat);
  cairo_pattern_t* pat = cairo_pattern_create_rgba(Double_val(vr),
                                                   Double_val(vg),
                                                   Double_val(vb),
                                                   Double_val(va));
  caml_raise_Error(cairo_pattern_status(pat));
  PATTERN_ASSIGN(vpat, pat);
  CAMLreturn(vpat);
}

CAMLexport value caml_cairo_pattern_get_rgba(value vpat)
{
  CAMLparam1(vpat);
  CAMLlocal1(vrgba);
  double red, green, blue, alpha;
  cairo_status_t st = cairo_pattern_get_rgba(PATTERN_VAL(vpat),
                                             &red, &green, &blue, &alpha);
  caml_raise_Error(st);
  vrgba = caml_alloc_tuple(4);
  Store_field(vrgba, 0, caml_copy_double(red));
  Store_field(vrgba, 1, caml_copy_double(green));
  Store_field(vrgba, 2, caml_copy_double(blue));
  Store_field(vrgba, 3, caml_copy_double(alpha));
  CAMLreturn(vrgba);
}

CAMLexport value caml_cairo_pattern_create_for_surface(value vsurf)
{
  CAMLparam1(vsurf);
  CAMLlocal1(vpat);
  cairo_pattern_t* pat = cairo_pattern_create_for_surface(SURFACE_VAL(vsurf));
  caml_raise_Error(cairo_pattern_status(pat));
  PATTERN_ASSIGN(vpat, pat);
  CAMLreturn(vpat);
}

CAMLexport value caml_cairo_pattern_get_surface(value vpat)
{
  CAMLparam1(vpat);
  CAMLlocal1(vsurf);
  cairo_surface_t *surface;
  cairo_status_t st = cairo_pattern_get_surface(PATTERN_VAL(vpat),
                                                &surface);
  caml_raise_Error(st);
  SURFACE_ASSIGN(vsurf, surface);
  CAMLreturn(vsurf);
}

CAMLexport value caml_cairo_pattern_create_linear
(value vx0, value vy0, value vx1, value vy1)
{
  CAMLparam4(vx0, vy0, vx1, vy1);
  CAMLlocal1(vpat);
  cairo_pattern_t* pat = cairo_pattern_create_linear
    (Double_val(vx0), Double_val(vy0), Double_val(vx1), Double_val(vy1));
  caml_raise_Error(cairo_pattern_status(pat));
  PATTERN_ASSIGN(vpat, pat);
  CAMLreturn(vpat);
}

CAMLexport value caml_cairo_pattern_get_linear_points(value vpat)
{
  CAMLparam1(vpat);
  CAMLlocal1(vcoord);
  double x0, y0, x1, y1;
  cairo_status_t st = cairo_pattern_get_linear_points
    (PATTERN_VAL(vpat), &x0, &y0, &x1, &y1);
  caml_raise_Error(st);
  vcoord = caml_alloc_tuple(4);
  Store_field(vcoord, 0, caml_copy_double(x0));
  Store_field(vcoord, 1, caml_copy_double(y0));
  Store_field(vcoord, 2, caml_copy_double(x1));
  Store_field(vcoord, 3, caml_copy_double(y1));
  CAMLreturn(vcoord);
}

CAMLexport value caml_cairo_pattern_create_radial
(value vx0, value vy0, value vr0, value vx1, value vy1, value vr1)
{
  CAMLparam5(vx0, vy0, vr0, vx1, vy1);
  CAMLxparam1(vr1);
  CAMLlocal1(vpat);
  cairo_pattern_t* pat = cairo_pattern_create_radial
    (Double_val(vx0), Double_val(vy0), Double_val(vr0),
     Double_val(vx1), Double_val(vy1), Double_val(vr1));
  caml_raise_Error(cairo_pattern_status(pat));
  PATTERN_ASSIGN(vpat, pat);
  CAMLreturn(vpat);
}

CAMLexport value caml_cairo_pattern_create_radial_bc(value * argv, int argn)
{
  return caml_cairo_pattern_create_radial(argv[0], argv[1], argv[2], argv[3],
                                          argv[4], argv[5]);
}

CAMLexport value caml_cairo_pattern_get_radial_circles(value vpat)
{
  CAMLparam1(vpat);
  CAMLlocal1(vcircles);
  double x0, y0, r0, x1, y1, r1;
  cairo_status_t st = cairo_pattern_get_radial_circles
    (PATTERN_VAL(vpat), &x0, &y0, &r0, &x1, &y1, &r1);
  caml_raise_Error(st);
  vcircles = caml_alloc_tuple(6);
  Store_field(vcircles, 0, caml_copy_double(x0));
  Store_field(vcircles, 1, caml_copy_double(y0));
  Store_field(vcircles, 2, caml_copy_double(r0));
  Store_field(vcircles, 3, caml_copy_double(x1));
  Store_field(vcircles, 4, caml_copy_double(y1));
  Store_field(vcircles, 5, caml_copy_double(r1));
  CAMLreturn(vcircles);
}

CAMLexport value caml_cairo_pattern_set_extend(value vpat, value vextend)
{
  /* noalloc */
  cairo_pattern_set_extend(PATTERN_VAL(vpat), EXTEND_VAL(vextend));
  return(Val_unit);
}

CAMLexport value caml_cairo_pattern_get_extend(value vpat)
{
  CAMLparam1(vpat);
  cairo_extend_t extend = cairo_pattern_get_extend(PATTERN_VAL(vpat));
  CAMLreturn(VAL_EXTEND(extend));
}

CAMLexport value caml_cairo_pattern_set_filter(value vpat, value vfilter)
{
  /* noalloc */
  cairo_pattern_set_filter(PATTERN_VAL(vpat), FILTER_VAL(vfilter));
  return(Val_unit);
}

CAMLexport value caml_cairo_pattern_get_filter(value vpat)
{
  CAMLparam1(vpat);
  cairo_filter_t filter = cairo_pattern_get_filter(PATTERN_VAL(vpat));
  CAMLreturn(VAL_FILTER(filter));
}

CAMLexport value caml_cairo_pattern_set_matrix(value vpat, value vmat)
{
  /* noalloc */
  cairo_matrix_t *matrix;
  SET_MATRIX_VAL(matrix, vmat);
  cairo_pattern_set_matrix(PATTERN_VAL(vpat), matrix);
  return(Val_unit);
}

CAMLexport value caml_cairo_pattern_get_matrix(value vpat)
{
  CAMLparam1(vpat);
  CAMLlocal1(vmat);
  cairo_matrix_t *matrix;
  cairo_pattern_get_matrix(PATTERN_VAL(vpat), matrix);
  MATRIX_ASSIGN(vmat, matrix);
  CAMLreturn(vmat);
}


/* Transformations - Manipulating the current transformation matrix
***********************************************************************/

DO2_FUNCTION(cairo_translate, Double_val, Double_val)
DO2_FUNCTION(cairo_scale, Double_val, Double_val)
DO1_FUNCTION(cairo_rotate, Double_val)

CAMLexport value caml_cairo_transform(value vcr, value vmat)
{
  /* noalloc */
  cairo_matrix_t *matrix;
  SET_MATRIX_VAL(matrix, vmat);
  cairo_transform(CAIRO_VAL(vcr), matrix);
  return(Val_unit);
}

CAMLexport value caml_cairo_set_matrix(value vcr, value vmat)
{
  /* noalloc */
  cairo_matrix_t *matrix;
  SET_MATRIX_VAL(matrix, vmat);
  cairo_set_matrix(CAIRO_VAL(vcr), matrix);
  return(Val_unit);
}

CAMLexport value caml_cairo_get_matrix(value vcr)
{
  CAMLparam1(vcr);
  CAMLlocal1(vmat);
  cairo_matrix_t *matrix;
  cairo_get_matrix(CAIRO_VAL(vcr), matrix);
  MATRIX_ASSIGN(vmat, matrix);
  CAMLreturn(vmat);
}

DO_FUNCTION(cairo_identity_matrix)

#define COORD_TRANSFORM(name)                                 \
  CAMLexport value caml_##name(value vcr, value vx, value vy) \
  {                                                           \
    CAMLparam3(vcr, vx, vy);                                  \
    CAMLlocal1(vcouple);                                      \
    cairo_t* cr = CAIRO_VAL(vcr);                             \
    double x = Double_val(vx);                                \
    double y = Double_val(vy);                                \
    name(cr, &x, &y);                                         \
    vcouple = caml_alloc_tuple(2);                            \
    Store_field(vcouple, 0, caml_copy_double(x));             \
    Store_field(vcouple, 1, caml_copy_double(y));             \
    CAMLreturn(vcouple);                                      \
  }

COORD_TRANSFORM(cairo_user_to_device)
COORD_TRANSFORM(cairo_user_to_device_distance)
COORD_TRANSFORM(cairo_device_to_user)
COORD_TRANSFORM(cairo_device_to_user_distance)


/* Font options
***********************************************************************/

DO1_FUNCTION(cairo_set_font_options, FONT_OPTIONS_VAL)

CAMLexport value caml_cairo_get_font_options(value vcr)
{
  CAMLparam1(vcr);
  CAMLlocal1(vfont_option);
  cairo_font_options_t *options = cairo_font_options_create();
  caml_raise_Error(cairo_font_options_status(options));
  cairo_get_font_options(CAIRO_VAL(vcr), options);
  FONT_OPTIONS_ASSIGN(vfont_option, options);
  CAMLreturn(vfont_option);
}

CAMLexport value caml_cairo_font_options_create(value vunit)
{
  CAMLparam1(vunit);
  CAMLlocal1(vfo);
  cairo_font_options_t* fo = cairo_font_options_create();
  caml_raise_Error(cairo_font_options_status(fo));
  FONT_OPTIONS_ASSIGN(vfo, fo);
  CAMLreturn(vfo);
}

CAMLexport value caml_cairo_font_options_copy(value vorig)
{
  CAMLparam1(vorig);
  CAMLlocal1(vcopy);
  cairo_font_options_t* copy = cairo_font_options_copy(FONT_OPTIONS_VAL(vorig));
  caml_raise_Error(cairo_font_options_status(copy));
  FONT_OPTIONS_ASSIGN(vcopy, copy);
  CAMLreturn(vcopy);
}

#define SET_FONT_OPTIONS(name, of_val)                  \
  CAMLexport value caml_##name(value vfo, value v)      \
  {                                                     \
    CAMLparam2(vfo, v);                                 \
    name(FONT_OPTIONS_VAL(vfo), of_val(v));             \
    CAMLreturn(Val_unit);                               \
  }

#define GET_FONT_OPTIONS(name, val_of, type)            \
  CAMLexport value caml_##name(value vfo)               \
  {                                                     \
    CAMLparam1(vfo);                                    \
    type ret = name(FONT_OPTIONS_VAL(vfo));             \
    CAMLreturn(val_of(ret));                            \
  }

SET_FONT_OPTIONS(cairo_font_options_merge, FONT_OPTIONS_VAL)
SET_FONT_OPTIONS(cairo_font_options_set_antialias, ANTIALIAS_VAL)
GET_FONT_OPTIONS(cairo_font_options_get_antialias,
                 VAL_ANTIALIAS, cairo_antialias_t)

#define SUBPIXEL_ORDER_VAL(v) Int_val(v)
#define VAL_SUBPIXEL_ORDER(v) Val_int(v)

SET_FONT_OPTIONS(cairo_font_options_set_subpixel_order, SUBPIXEL_ORDER_VAL)
GET_FONT_OPTIONS(cairo_font_options_get_subpixel_order,
                 VAL_SUBPIXEL_ORDER, cairo_subpixel_order_t)

#define HINT_STYLE_VAL(v) Int_val(v)
#define VAL_HINT_STYLE(v) Val_int(v)

SET_FONT_OPTIONS(cairo_font_options_set_hint_style, HINT_STYLE_VAL)
GET_FONT_OPTIONS(cairo_font_options_get_hint_style,
                 VAL_HINT_STYLE, cairo_hint_style_t)

#define HINT_METRICS_VAL(v) Int_val(v)
#define VAL_HINT_METRICS(v) Val_int(v)

SET_FONT_OPTIONS(cairo_font_options_set_hint_metrics, HINT_METRICS_VAL)
GET_FONT_OPTIONS(cairo_font_options_get_hint_metrics,
                 VAL_HINT_METRICS, cairo_hint_metrics_t)

/* Font face
***********************************************************************/

CAMLexport value caml_cairo_font_face_get_type(value vff)
{
  CAMLparam1(vff);
  cairo_font_type_t ft = cairo_font_face_get_type(FONT_FACE_VAL(vff));
  CAMLreturn(VAL_FONT_TYPE(ft));
}


DO1_FUNCTION(cairo_set_font_face, FONT_FACE_VAL)

CAMLexport value caml_cairo_get_font_face(value vcr)
{
  CAMLparam1(vcr);
  CAMLlocal1(vff);
  cairo_font_face_t* ff = cairo_get_font_face(CAIRO_VAL(vcr));
  caml_raise_Error(cairo_font_face_status(ff));
  /* Since we are going to create a value with the [ff] and this value
     is shared with the one hold inside the cairo context, one must
     increase the reference count (to avoid that destroying one of
     these object leaves a dangling pointer for the other).  */
  cairo_font_face_reference(ff);
  vff = ALLOC(font_face);
  FONT_FACE_VAL(vff) = ff;
  CAMLreturn(vff);
}

/* Scaled font
***********************************************************************/

DO1_FUNCTION(cairo_set_scaled_font, SCALED_FONT_VAL)

CAMLexport value caml_cairo_get_scaled_font(value vcr)
{
  CAMLparam1(vcr);
  CAMLlocal1(vsf);
  cairo_scaled_font_t* sf = cairo_get_scaled_font(CAIRO_VAL(vcr));
  /* create a value with shared [sf] => must increase ref. count */
  cairo_scaled_font_reference(sf);
  vsf = ALLOC(scaled_font);
  SCALED_FONT_VAL(vsf) = sf;
  CAMLreturn(vsf);
}

CAMLexport value caml_cairo_scaled_font_create
(value vff, value vfont_matrix, value vctm, value voptions)
{
  CAMLparam4(vff, vfont_matrix, vctm, voptions);
  CAMLlocal1(vsf);
  cairo_matrix_t *font_matrix, *ctm;
  SET_MATRIX_VAL(font_matrix, vfont_matrix);
  SET_MATRIX_VAL(ctm, vctm);
  cairo_scaled_font_t* sf = cairo_scaled_font_create
    (FONT_FACE_VAL(vff), font_matrix, ctm, FONT_OPTIONS_VAL(voptions));
  vsf = ALLOC(scaled_font);
  SCALED_FONT_VAL(vsf) = sf;  
  CAMLreturn(vsf);
}

CAMLexport value caml_cairo_scaled_font_extents(value vsf)
{
  CAMLparam1(vsf);
  CAMLlocal1(vfe);
  cairo_font_extents_t fe;
  cairo_scaled_font_extents(SCALED_FONT_VAL(vsf), &fe);
  vfe = caml_alloc(5 * Double_wosize, Double_array_tag);
  Store_double_field(vfe, 0, fe.ascent);
  Store_double_field(vfe, 1, fe.descent);
  Store_double_field(vfe, 2, fe.height);
  Store_double_field(vfe, 3, fe.max_x_advance);
  Store_double_field(vfe, 4, fe.max_y_advance);
  CAMLreturn(vfe);
}

CAMLexport value caml_cairo_scaled_font_text_extents(value vsf, value vutf8)
{
  CAMLparam2(vsf, vutf8);
  CAMLlocal1(vte);
  cairo_text_extents_t te;
  cairo_scaled_font_text_extents(SCALED_FONT_VAL(vsf), String_val(vutf8), &te);
  vte = caml_alloc(6 * Double_wosize, Double_array_tag);
  Store_double_field(vte, 0, te.x_bearing);
  Store_double_field(vte, 1, te.y_bearing);
  Store_double_field(vte, 2, te.width);
  Store_double_field(vte, 3, te.height);
  Store_double_field(vte, 4, te.x_advance);
  Store_double_field(vte, 5, te.y_advance);
  CAMLreturn(vte);
}

CAMLexport value caml_cairo_scaled_font_glyph_extents(value vsf, value vglyphs)
{
  CAMLparam2(vsf, vglyphs);
  CAMLlocal1(vte);
  cairo_text_extents_t te;
  int i, num_glyphs = Wosize_val(vglyphs);
  cairo_glyph_t *glyphs, *p;
  
  glyphs = malloc(num_glyphs * sizeof(cairo_glyph_t));
  for(i=0, p = glyphs; i < num_glyphs; i++, p++) {
    SET_GLYPH_VAL(p, Field(vglyphs, i));
  }
  cairo_scaled_font_glyph_extents(SCALED_FONT_VAL(vsf),
                                  glyphs, num_glyphs, &te);
  free(glyphs);
  vte = caml_alloc(6 * Double_wosize, Double_array_tag);
  Store_double_field(vte, 0, te.x_bearing);
  Store_double_field(vte, 1, te.y_bearing);
  Store_double_field(vte, 2, te.width);
  Store_double_field(vte, 3, te.height);
  Store_double_field(vte, 4, te.x_advance);
  Store_double_field(vte, 5, te.y_advance);
  CAMLreturn(vte);
}

CAMLexport value caml_cairo_scaled_font_text_to_glyphs
(value vsf, value vx, value vy, value vutf8)
{
  CAMLparam4(vsf, vx, vy, vutf8);
  CAMLlocal4(vglyphs, vclusters, vtriplet, v);
  cairo_glyph_t *glyphs = NULL;
  int i, num_glyphs;
  cairo_text_cluster_t *clusters = NULL;
  int num_clusters;
  cairo_text_cluster_flags_t cluster_flags;
  cairo_status_t status;
  
  status = cairo_scaled_font_text_to_glyphs
    (SCALED_FONT_VAL(vsf), Double_val(vx), Double_val(vy),
     String_val(vutf8), string_length(vutf8),
     &glyphs, &num_glyphs,  &clusters, &num_clusters,  &cluster_flags);
  caml_raise_Error(status);

  vglyphs = caml_alloc_tuple(num_glyphs);
  for(i = 0; i < num_glyphs; i++) {
    GLYPH_ASSIGN(v, glyphs[i]);
    Store_field(vglyphs, i, v);
  }
  cairo_glyph_free(glyphs);
  vclusters = caml_alloc_tuple(num_clusters);
  for(i = 0; i < num_clusters; i++) {
    CLUSTER_ASSIGN(v, clusters[i]);
    Store_field(vclusters, i, v);
  }
  cairo_text_cluster_free(clusters);
  /* FIXME: cluster_flags */
  /* (glyphs, clusters, cluster_flags) */
  vtriplet = caml_alloc_tuple(3);
  Store_field(vtriplet, 0, vglyphs);
  Store_field(vtriplet, 1, vclusters);
  Store_field(vtriplet, 2, VAL_CLUSTER_FLAGS(cluster_flags));
  CAMLreturn(vtriplet);
}

CAMLexport value caml_cairo_scaled_font_get_font_face(value vsf)
{
  CAMLparam1(vsf);
  CAMLlocal1(vff);
  cairo_font_face_t* ff;
  ff = cairo_scaled_font_get_font_face(SCALED_FONT_VAL(vsf));
  /* FIXME: The documentation does not say whether it is shared or
     not; assuming it is as for other functions. */
  cairo_font_face_reference(ff);
  vff = ALLOC(font_face);
  FONT_FACE_VAL(vff) = ff;
  CAMLreturn(vff);
}


CAMLexport value caml_cairo_scaled_font_get_font_options(value vsf)
{
  CAMLparam1(vsf);
  CAMLlocal1(vfo);
  cairo_font_options_t *fo = cairo_font_options_create();
  caml_raise_Error(cairo_font_options_status(fo));
  cairo_scaled_font_get_font_options(SCALED_FONT_VAL(vsf), fo);
  FONT_OPTIONS_ASSIGN(vfo, fo);
  CAMLreturn(vfo);
}

#define SCALED_FONT_GET_MATRIX(name)                                    \
  CAMLexport value caml_##name(value vsf)                               \
  {                                                                     \
    CAMLparam1(vsf);                                                    \
    CAMLlocal1(vmatrix);                                                \
    cairo_matrix_t *matrix;                                             \
    name(SCALED_FONT_VAL(vsf), matrix);                                 \
    MATRIX_ASSIGN(vmatrix, matrix);                                     \
    CAMLreturn(vmatrix);                                                \
  }

SCALED_FONT_GET_MATRIX(cairo_scaled_font_get_font_matrix)
SCALED_FONT_GET_MATRIX(cairo_scaled_font_get_ctm)
SCALED_FONT_GET_MATRIX(cairo_scaled_font_get_scale_matrix)


/* Local Variables: */
/* compile-command: "make -k cairo_stubs.o" */
/* End: */
