/* myfig.h */
/* ***** Generated from pstoedit ***** */
#ifndef __myfig_H__
#define __myfig_H__
#include <cairo.h>
extern cairo_t * (*myfig_render[])(cairo_surface_t *, cairo_t *);
extern int myfig_total_pages;
extern int myfig_width[];
extern int myfig_height[];
extern void myfig_init(void);
#endif /* __myfig_H__ */

