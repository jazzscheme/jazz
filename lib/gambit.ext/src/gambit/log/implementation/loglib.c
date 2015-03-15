/* File: "loglib.c"   (c) 1997-1998, Marc Feeley */

/* Library to produce state logs. */

/* To compile:  gcc -O -c loglib.c */

#include <stdio.h>
#include <stdlib.h>
#include "arith64.h"
#include "loglib.h"

//#define USE_RDTSC
//#define USE_Performance


/*---------------------------------------------------------------------------*/

/* Error processing. */


static void fatal_error (msg)
char *msg;
{
  fprintf (stderr, "%s\n", msg);
  exit (1);
}


/*---------------------------------------------------------------------------*/

/* Elapsed real time. */


#ifndef USE_RDTSC
#ifndef USE_Performance
#ifndef USE_time
#ifndef USE_ftime
#ifndef USE_gettimeofday
#ifndef USE_TickCount
#define USE_gettimeofday
#endif
#endif
#endif
#endif
#endif
#endif

#ifdef USE_time
#include <time.h>
#endif

#ifdef USE_ftime
#include <sys/timeb.h>
#endif

#ifdef USE_gettimeofday
#include <sys/time.h>
#endif


#ifdef USE_Performance
LARGE_INTEGER frequency;
#endif


static U64 abs_real_time ()
{
  U64 result;

#ifdef USE_RDTSC

  __asm__ __volatile__("rdtsc": "=A" (result));
  result = result * 10 / 34;

#endif

#ifdef USE_Performance

  LARGE_INTEGER counter;
  QueryPerformanceCounter(&counter);
  result = counter.QuadPart * 1000000000 / frequency.QuadPart;

#endif
  
#ifdef USE_time

  time_t t;

  time (&t);

  result = U64_mul_U32 (U64_init (0, t), 1000000);

#endif

#ifdef USE_ftime

  struct timeb tb;

  ftime (&tb);

  result = U64_mul_U32 (U64_add_U32 (U64_mul_U32 (U64_init (0, tb.time),
                                                  1000),
                                     tb.millitm),
                        1000);

#endif

#ifdef USE_gettimeofday

  /*
   * Each call to "abs_real_time" takes roughly from 3 to 10 usecs of
   * real time on current (1997) machines:
   *
   *  3.7 usecs  SunOS 5.5 (150Mhz SPARC)
   *  7.1 usecs  OSF1 V4.0 (133Mhz DEC Alpha)
   * 10.1 usecs  Linux 2.0.26 (200Mhz Pentium)
   */

  struct timeval tv;

  if (gettimeofday (&tv, NULL) == 0)
    result = U64_add_U32 (U64_mul_U32 (U64_init (0, tv.tv_sec),
                                       1000000),
                          tv.tv_usec);
  else
    result = U64_init (0, 0);

#endif

#ifdef USE_TickCount

  result = U64_mul_U32 (U64_init (0, (U32)TickCount ()),
                        (U32)(1<<12) * 1000000 / 60);
  U64_shift_right (&result, 12);

#endif

  return result;
}


static void init_abs_real_time ()
{
#ifdef USE_Performance
  QueryPerformanceFrequency(&frequency);
#endif

  /* Some systems (e.g. DJGPP) need a first call to define the time origin. */
  abs_real_time (); /* reset origin */
}


/*---------------------------------------------------------------------------*/

/* Writing of log files. */


static void write_U8 (f, n)
FILE *f;
U8 n;
{
  putc (n, f);
}


static void write_U16 (f, n)
FILE *f;
U16 n;
{
  putc (n>>8, f);
  putc (n & ~(~0<<8), f);
}


static void write_U32 (f, n)
FILE *f;
U32 n;
{
  putc (n>>24, f);
  putc ((n>>16) & ~(~0<<8), f);
  putc ((n>>8) & ~(~0<<8), f);
  putc (n & ~(~0<<8), f);
}


static void write_U64 (f, n)
FILE *f;
U64 n;
{
  U32 n2 = U64_shift_right (&n, 32);
  U32 n1 = U64_shift_right (&n, 32);
  write_U32 (f, n1);
  write_U32 (f, n2);
}


static void write_string (f, str)
FILE *f;
char *str;
{
  while (*str != '\0')
    putc (*str++, f);
  putc ('\n', f);
}


/*---------------------------------------------------------------------------*/

/* Log related data for one process. */


static void log_dump (context)
struct log_context *context;
{
  char filename[1024];
  FILE *f;
  int i;
  struct log_state_transition *ptr;

  sprintf (filename, "%s.log%03d", context->prog_name, context->pnum);

  f = fopen (filename, "wb");
  if (f == NULL)
    fatal_error ("Can't open log file");

  write_string (f, context->name);
  write_U32 (f, context->nb_states);

  for (i=0; i<context->nb_states; i++)
    {
      if (context->states[i].name == NULL)
        fatal_error ("Some states are not defined");
      write_U32 (f, context->states[i].color);
      write_string (f, context->states[i].name);
    }

  write_U32 (f, context->ptr - context->buf);

  ptr = context->buf;
  while (ptr < context->ptr)
    {
      write_U16 (f, ptr->state);
      write_U64 (f, ptr->time);
      ptr++;
    }

  fclose (f);
}


/*---------------------------------------------------------------------------*/

/* User callable routines. */


void log_setup (context, prog_name, ctx_name, pnum, nb_states, max_nb_trans)
struct log_context *context;
char *prog_name;
char *ctx_name;
int pnum;
int nb_states;
int max_nb_trans;
{
  int i;

  context->prog_name = prog_name;
  context->name = ctx_name;
  context->pnum = pnum;
  context->nb_states = nb_states;

  context->states = malloc (nb_states * sizeof (struct log_state_def));
  if (context->states == NULL)
    fatal_error ("log_setup could not allocate state table");

  for (i=0; i<nb_states; i++)
    context->states[i].name = NULL;

  if (max_nb_trans == 0)
    {
      context->buf = NULL;
      context->ptr = NULL;
    }
  else
    {
      context->buf = malloc (max_nb_trans *
                             sizeof (struct log_state_transition));
      if (context->buf == NULL)
        fatal_error ("log_setup could not allocate log buffer");

      context->end = context->buf + max_nb_trans;
      context->ptr = context->buf;
    }
}


void log_define_state (context, state, name, color)
struct log_context *context;
U16 state;
char *name;
RGB color;
{
  if (state >= context->nb_states)
    fatal_error ("state out of range");

  if (context->states[state].name != NULL)
    fatal_error ("can't redefine a state");

  context->states[state].color = color;
  context->states[state].name = name;
}


void log_start (context, state)
struct log_context *context;
U16 state;
{
  init_abs_real_time ();
  log_transition (context, state);
}


U16 log_state (context)
struct log_context *context;
{
  struct log_state_transition *p = context->ptr;
  if (p != NULL)
      return (p-1)->state;
  else
      return 0;
}


void log_transition (context, state)
struct log_context *context;
U16 state;
{
  struct log_state_transition *p = context->ptr;
  if (p != NULL)
    {
      if (p < context->end) /* don't overflow log buffer */
        {
          p->state = state;
          p->time = abs_real_time ();
          context->ptr = p+1;
        }
      context->current_state = state;
    }
}


void log_stop (context)
struct log_context *context;
{
  log_transition (context, 0);
  if (context->buf != NULL)
    log_dump (context);
}


void log_cleanup (context)
struct log_context *context;
{
  if (context->buf != NULL)
    free (context->buf);
}


/*---------------------------------------------------------------------------*/
