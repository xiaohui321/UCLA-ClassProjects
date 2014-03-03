// UCLA CS 111 Lab 1 command interface

#include <stdbool.h>

typedef struct command *command_t;
typedef struct command_stream *command_stream_t;


struct command_stream{
  struct command_stream* next;
  struct command_stream* prev;
  command_t theCommand;
};

/* Create a command stream from GETBYTE and ARG.  A reader of
   the command stream will invoke GETBYTE (ARG) to get the next byte.
   GETBYTE will return the next input byte, or a negative number
   (setting errno) on failure.  */
command_stream_t make_command_stream (int (*getbyte) (void *), void *arg);

/* Read a command from STREAM; return it, or NULL on EOF.  If there is
   an error, report the error and exit instead of returning.  */
command_t read_command_stream (command_stream_t stream);

/* Print a command to stdout, for debugging.  */
void print_command (command_t);

/* Execute commands in parallel.  */
command_t  execute_command_in_parallel (command_stream_t);

/* Execute a command normally */
void execute_command (command_t);

/* Return the exit status of a command, which must have previously
   been executed.  Wait for the command, if it is not already finished.  */
int command_status (command_t);
