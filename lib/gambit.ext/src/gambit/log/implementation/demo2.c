/* File: "demo2.c"   (c) 1997-1998, Marc Feeley */

/* Demo to produce a state log for a program with concurrent processes. */

/* To compile:  gcc -o demo2 demo2.c loglib.o */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>

#include "loglib.h"

/*---------------------------------------------------------------------------*/

/* Define state identifiers */

#define NB_STATES 3
#define STATE_IDLE 0
#define STATE_WORK 1
#define STATE_WAIT 2


/* Code for a process */

struct log_context context;

void waste_time ()
{
  int i;
  for (i=0; i<100; i++) ;
}

void process (pnum, prog_name)
int pnum;
char *prog_name;
{
  log_setup (&context, prog_name, pnum, NB_STATES, 1000000);
  log_define_state (&context, STATE_IDLE, "idle", log_BLACK);
  log_define_state (&context, STATE_WORK, "working", log_GREEN);
  log_define_state (&context, STATE_WAIT, "waiting", log_RED);

  log_start (&context, STATE_IDLE);

  {
    int i;
    for (i=0; i<10000; i++)
      {
        waste_time ();
        log_transition (&context, STATE_WORK);
        waste_time ();
        log_transition (&context, STATE_WAIT);
        waste_time ();
        log_transition (&context, STATE_IDLE);
      }
  }

  log_stop (&context);

  log_cleanup (&context);
}


/* Main */

#define N 4

int main (argc, argv)
int argc;
char *argv[];
{
  pid_t pid[N];
  int pnum;

  for (pnum=1; pnum<N; pnum++) /* start N-1 processes */
    {
      pid[pnum] = fork ();
      if (pid[pnum] == 0)
        {
          process (pnum, argv[0]);
          exit (0);
        }
    }

  process (0, argv[0]); /* start process 0 */

  for (pnum=1; pnum<N; pnum++)
    waitpid (pid[pnum], NULL, 0);
}


/*---------------------------------------------------------------------------*/
