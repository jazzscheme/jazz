/* File: "demo1.c"   (c) 1997-1998, Marc Feeley */

/* Demo to produce a state log to visualize the "fibonacci" procedure. */

/* To compile:  gcc -o demo1 demo1.c loglib.o */

#include <stdio.h>
#include "loglib.h"

/*---------------------------------------------------------------------------*/

/* Code for a process */


struct log_context context;


void waste_time ()
{
  int i;
  for (i=0; i<100; i++) ;
}


int fib (x, depth)
{
  int a, b;
  waste_time ();
  log_transition (&context, depth);
  if (x < 2)
    return 1;
  else
    {
      a = fib (x-1, depth+1);
      log_transition (&context, depth);
      b = fib (x-2, depth+1);
      log_transition (&context, depth);
      return a+b;
    }
}


int main (argc, argv)
int argc;
char *argv[];
{
  log_setup (&context, argv[0], 0, 16, 1000000);
  log_define_state (&context, 0, "idle", log_BLACK);
  log_define_state (&context, 1, "depth 1", log_RED);
  log_define_state (&context, 2, "depth 2", log_GREEN);
  log_define_state (&context, 3, "depth 3", log_BLUE);
  log_define_state (&context, 4, "depth 4", log_YELLOW);
  log_define_state (&context, 5, "depth 5", log_MAGENTA);
  log_define_state (&context, 6, "depth 6", log_CYAN);
  log_define_state (&context, 7, "depth 7", log_LAVENDER);
  log_define_state (&context, 8, "depth 8", log_PINK);
  log_define_state (&context, 9, "depth 9", log_GOLD);
  log_define_state (&context, 10, "depth 10", log_TOMATO);
  log_define_state (&context, 11, "depth 11", log_PURPLE);
  log_define_state (&context, 12, "depth 12", log_GRAY);
  log_define_state (&context, 13, "depth 13", log_BROWN);
  log_define_state (&context, 14, "depth 14", log_CORNSILK);
  log_define_state (&context, 15, "depth 15", log_IVORY);

  log_start (&context, 0);

  {
    int n = fib (15, 1);
    log_transition (&context, 0);
    printf ("fib (15) = %d\n", n);
  }

  log_stop (&context);

  log_cleanup (&context);
}


/*---------------------------------------------------------------------------*/
