/* File: "demo3.c" */

/* Demo to produce a simple state log */

/* To compile:  gcc -o demo3 demo3.c loglib.o */

#include <stdio.h>
#include "loglib.h"

/*---------------------------------------------------------------------------*/

struct log_context context;


void waste_time ()
{
  int i;
  for (i=0; i<1000000; i++) ;
}


int main (argc, argv)
int argc;
char *argv[];
{
  log_setup (&context, argv[0], 0, 3, 1000000);

  log_define_state (&context, 0, "state 0", log_RED);
  log_define_state (&context, 1, "state 1", log_GREEN);
  log_define_state (&context, 2, "state 2", log_BLUE);

  log_start (&context, 0);

  waste_time ();
  waste_time ();

  log_transition (&context, 1);

  waste_time ();
  waste_time ();
  waste_time ();

  log_transition (&context, 0);

  waste_time ();

  log_transition (&context, 2);

  waste_time ();
  waste_time ();

  log_stop (&context);

  log_cleanup (&context);
}


/*---------------------------------------------------------------------------*/
