/* File: "loglib.h"   (c) 1997-1998, Marc Feeley */

/* Library to produce state logs. */

#ifndef LOGLIB_H
#define LOGLIB_H

#include "arith64.h"

/*---------------------------------------------------------------------------*/

/* RGB colors are represented by a 32 bit unsigned integer */

typedef U32 RGB;

#define RGBCOLOR(r,g,b)(((((U32)(r)<<8)+(U32)(g))<<8)+(U32)(b))
#define RED(c)((c)>>16)
#define GREEN(c)(((c)>>8)&0xff)
#define BLUE(c)((c)&0xff)

/*---------------------------------------------------------------------------*/

/* Predefined colors. */

#define log_BLACK    RGBCOLOR(  0,  0,  0)
#define log_WHITE    RGBCOLOR(255,255,255)
#define log_RED      RGBCOLOR(255,  0,  0)
#define log_GREEN    RGBCOLOR(0,  255,  0)
#define log_BLUE     RGBCOLOR(0,    0,255)
#define log_YELLOW   RGBCOLOR(255,255,  0)
#define log_MAGENTA  RGBCOLOR(255,  0,255)
#define log_CYAN     RGBCOLOR(  0,255,255)

#define log_LAVENDER RGBCOLOR(230,230,250)
#define log_PINK     RGBCOLOR(255,192,203)
#define log_GOLD     RGBCOLOR(255,215,  0)
#define log_TOMATO   RGBCOLOR(255, 99, 71)
#define log_PURPLE   RGBCOLOR(160, 32,240)
#define log_GRAY     RGBCOLOR(190,190,190)
#define log_BROWN    RGBCOLOR(165, 42, 42)
#define log_CORNSILK RGBCOLOR(255,248,220)
#define log_IVORY    RGBCOLOR(255,255,240)

/*---------------------------------------------------------------------------*/

struct log_state_def
  {
    RGB color;
    char *name;
  };

struct log_state_transition
  {
    U16 state;
    U64 time;
  };

struct log_context
  {
    char *prog_name;
    char *name;
    int pnum;
    int nb_states;
    struct log_state_def *states;
    struct log_state_transition *buf, *end, *ptr;
    U16 current_state;
  };

extern void log_setup (/* context, prog_name, pnum, nb_states, max_nb_trans*/);
extern void log_define_state (/* context, state, name, color */);
extern void log_start (/* context, state */);
extern void log_transition (/* context, state */);
extern void log_stop (/* context */);
extern void log_cleanup (/* context */);

/*---------------------------------------------------------------------------*/

#endif
