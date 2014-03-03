// UCLA CS 111 Lab 1 command execution
//author: xiaohui,zhou

#include "alloc.h"
#include "command.h"
#include "command-internals.h"
#include <error.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <unistd.h>
#include <string.h>

int command_status (command_t c){
  return c->status;
}

/* Execute a simple command */
void execute_simple_command(command_t c){
  
  pid_t pid = fork();
  
  if (pid < 0){
    perror("ERROR:unable to fork");
    exit(EXIT_FAILURE);
  }
  
  /*child*/
  if (pid == 0){
    if(c->u.word[0][0] ==':')
      exit(EXIT_SUCCESS);

    /*I/O*/
    if (c->input != NULL){
      int input = open(c->input,O_RDONLY);
      if (input < 0)
	perror("ERROR: can't open input file");
      if(dup2(input,0)<0)
	perror("ERROR: can't dup input");
      close(input);
    }
    if (c->output != NULL){
      int output = open(c->output,O_WRONLY|O_TRUNC|O_CREAT, 
			S_IRUSR|S_IRGRP|S_IWGRP|S_IWUSR);
      if(output < 0)
	perror("ERROR: can't open output file");

      if(dup2(output,1)<0)
	perror("ERROR: can't dup output");
      close(output);
    }
    
    if( execvp(c->u.word[0],c->u.word) < 0){
      perror("ERROR:cant successfully call execvp");
      exit(EXIT_FAILURE);
    }
  }else{
    /*parent*/
    int status;
    if( waitpid(pid,&status,0) < 0){
      perror("ERROR:serious mistake when waiting child return");
      exit(EXIT_FAILURE);
    }
    
    if (WIFEXITED(status) == 0){
      perror("ERROR:cant successfully exit the program");
      exit(EXIT_FAILURE);
    }
    else
      c->status = WEXITSTATUS(status);
    // printf("status = %d; wifexited= %d; wexitstatus=%d; c->status = %d\n",
    //   status,WIFEXITED(status), WEXITSTATUS(status),c->status);
  }
}

/* Execute a pipe command */
void execute_pipe_command(command_t c){
 
  int pipefd[2];
 
  if (pipe(pipefd) < 0){
    perror("ERROR:unable to pipe");
    exit(EXIT_FAILURE);
  }

  /*child:first command*/
  pid_t pid_0 = fork();

  if (pid_0 < 0){
    perror("ERROR:unable to fork");
    exit(EXIT_FAILURE);
  }

  if (pid_0 == 0){
    dup2(pipefd[1],1);
    close(pipefd[0]);
    execute_command(c->u.command[0]);
    exit(c->u.command[0]->status);
  }

 /*child:second command*/
  pid_t pid_1 = fork();  
    if (pid_0 < 0){
    perror("ERROR:unable to fork");
    exit(EXIT_FAILURE);
    }
 
  if (pid_1 == 0){
    dup2(pipefd[0],0);
    close(pipefd[1]);
    execute_command(c->u.command[1]);
    exit(c->u.command[1]->status);
  }
  
  close(pipefd[0]);
  close(pipefd[1]);
 
  /*parent of these 2 subcommands*/
  int status_0, status_1;
  waitpid(pid_0,&status_0,0);
  waitpid(pid_1,&status_1,0);
  if (WIFEXITED(status_0) == 0 || WIFEXITED(status_1) == 0){
      perror("ERROR:cant successfully exit the program");
      exit(EXIT_FAILURE);
    }
    else
      c->status = WEXITSTATUS(status_1);
}

/*execute a command without parallelization*/
void execute_command (command_t c){
    /* do normal executing*/
    switch(c->type){
    case SIMPLE_COMMAND:
      execute_simple_command(c);
      break;
    case AND_COMMAND:
      execute_simple_command(c->u.command[0]);
      /*if the first subcommand failed*/
      if(c->u.command[0]->status == EXIT_FAILURE){
	//fprintf(stderr,"AND COMMAND:first subcommand failed.\n");	      
	c->status = c->u.command[0]->status;
      }else{
	/*otherwise the first subcommand succeed*/
	execute_simple_command(c->u.command[1]);
	c->status = c->u.command[1]->status;
      }
      break;
    case OR_COMMAND:
      execute_simple_command(c->u.command[0]);
      /*if the first subcommand succeed*/
      if(c->u.command[0]->status == EXIT_SUCCESS){
	c->status = c->u.command[0]->status;
      }else{
	/*otherwise the first subcommand failed*/
	execute_simple_command(c->u.command[1]);
	c->status = c->u.command[1]->status;
      }
      break;
    case SEQUENCE_COMMAND:
      execute_simple_command(c->u.command[0]);
      execute_simple_command(c->u.command[1]);
      c->status = c->u.command[1]->status;
      break;
    case PIPE_COMMAND:
      execute_pipe_command(c);
      break;
    case SUBSHELL_COMMAND:
      /*I/O*/
      if (c->input != NULL){
	int input = open(c->input,O_RDONLY);
	if (input < 0)
	  perror("ERROR: can't open input file");
	if(dup2(input,0)<0)
	  perror("ERROR: can't dup input");
	close(input);
      }
      if (c->output != NULL){
	int output = open(c->output,O_WRONLY|O_TRUNC|O_CREAT, 
			  S_IRUSR|S_IRGRP|S_IWGRP|S_IWUSR);
	if(output < 0)
	  perror("ERROR: can't open output file");
	
	if(dup2(output,1)<0)
	  perror("ERROR: can't dup output");
	close(output);
      }
      
      execute_command(c->u.subshell_command);
      c->status = c->u.subshell_command->status;
      break; 
    default:
      error (1, 0, "ERROR: wrong command type!");
    }
}


/* return the number of commands in stream */
int get_size_of_command_stream (command_stream_t s)
{
  if(s == NULL)
    return 0;
  else
    return ( 1 + get_size_of_command_stream(s->next));
}

/* extract the output into "output" string */
void extract_output(command_t cmd, char*** output, int* size_output){
 
  if(cmd->output){
    (*size_output)++;
    (*output) = checked_realloc((*output),sizeof(char*)*(*size_output + 1));
    (*output)[*size_output -1 ]=cmd->output;
  }

  if(cmd->type != SUBSHELL_COMMAND && cmd->type != SIMPLE_COMMAND){
    extract_output(cmd->u.command[0],output,size_output);
    extract_output(cmd->u.command[1],output,size_output);
  }
}

/* extract the input into "input" string */
void extract_input(command_t cmd, char*** input, int* size_input){
  if(cmd->input){
    (*size_input)++;
    (*input) = checked_realloc((*input),sizeof(char*)*(*size_input + 1));
    (*input)[*size_input - 1 ]=cmd->input;
  }

  if(cmd->type != SUBSHELL_COMMAND && cmd->type != SIMPLE_COMMAND){
    extract_input(cmd->u.command[0],input,size_input);
    extract_input(cmd->u.command[1],input,size_input);
  }
}

/* check if a&b are dependent */
bool dependent(char** curr_input, command_t prev_cmd){
  char **prev_output = checked_malloc(sizeof(char*));
  int prev_output_size=0;
  extract_output(prev_cmd,&prev_output,&prev_output_size);
  int i,j;
  /*
  //print prev output
    for(i=0; i<prev_output_size;i++)
	printf("%s",prev_output[i]);
      printf("\n");
  */  
  i=j=0;
  while(curr_input[i] != NULL){
    j=0;
    while(prev_output[j] != NULL){
      if(!strcmp(curr_input[i],prev_output[j]))
	return true;
      j++;
    }
    i++;
  }
  return false;
}

void  execute_sequence_command_in_parallel(command_t c){

  /*child:first command*/
  pid_t pid_0 = fork();

  if (pid_0 < 0){
    perror("ERROR:unable to fork");
    exit(EXIT_FAILURE);
  }

  if (pid_0 == 0){
    if (c->u.command[0]->type ==SEQUENCE_COMMAND)
      execute_sequence_command_in_parallel(c->u.command[0]);
    else
      execute_command(c->u.command[0]);
    exit(c->u.command[0]->status);
  }
  /*child:first command*/
  pid_t pid_1 = fork();

  if (pid_1 < 0){
    perror("ERROR:unable to fork");
    exit(EXIT_FAILURE);
  }

  if (pid_1 == 0){
    if (c->u.command[0]->type ==SEQUENCE_COMMAND)
      execute_sequence_command_in_parallel(c->u.command[0]);
    else
      execute_command(c->u.command[0]);
    exit(c->u.command[0]->status);
  }
 
  /*parent of these 2 subcommands*/
  int status_0, status_1;
  waitpid(pid_0,&status_0,0);
  waitpid(pid_1,&status_1,0);
  if (WIFEXITED(status_0) == 0 || WIFEXITED(status_1) == 0){
      perror("ERROR:cant successfully exit the program");
      exit(EXIT_FAILURE);
    }
    else
      c->status = WEXITSTATUS(status_1);
}

/* Execute commands in parallel.  */
command_t execute_command_in_parallel (command_stream_t s)
{
  int size_command = get_size_of_command_stream(s->next);
  if(size_command == 0)
    return NULL;
  
  /* store the current level of a command in the dependency graph */
  int *level = checked_malloc(sizeof(int) * size_command);
  memset(level,0,sizeof(int) * size_command);
  
  /* a 2D array showing the dependency relationship */
   int **graph = checked_malloc(sizeof(int *) * size_command);
   int i,j;
   for(i = 0; i < size_command; i++){
    graph[i]=checked_malloc(sizeof(int) * size_command);
    memset(graph[i],0,sizeof(int) * size_command);
  }
  command_stream_t current_cmd,prev_cmd;
  current_cmd=prev_cmd=s->next;
  int index_current=0;
  
  while(current_cmd!=NULL){
    char **curr_input = checked_malloc(sizeof(char*));
    int curr_in_size=0;
    extract_input(current_cmd->theCommand,&curr_input,&curr_in_size);
  
    /*
    //print current input
    for(i=0; i<curr_in_size;i++)
	printf("%s",curr_input[i]);
      printf("\n");
    */
    for(i=0;i<index_current;i++){
      if(dependent(curr_input,prev_cmd->theCommand)){
	graph[index_current][i]=1;
	level[index_current]++;
      }
      prev_cmd=prev_cmd->next;
    }
    current_cmd=current_cmd->next;
    prev_cmd=s->next;
    index_current++;
  }
  
  /*
  // print dependency graph
  for(i=0; i<size_command;i++){
    printf("%d",level[i]);
      for(j=0; j<size_command;j++)
	printf("\t%d",graph[i][j]);
      printf("\n");
  }
  */

  int commands_left = size_command;
  pid_t *pids = checked_malloc(sizeof(pid_t)*size_command);
  int *status = checked_malloc(sizeof(int)*size_command);
  
  for(i=0;i<size_command;i++)
    pids[i]=-2;
  /*
    pid values:
     -3   have finished
     -2   havent started yet
     -1   error when forking
     0    child
     +    parent
  */
  while(commands_left >0)
    {
      current_cmd=s;
      /* execute commands */
      for(i=0;i<size_command;i++)
	{
	  if(current_cmd == NULL)
	    break;
	  current_cmd = current_cmd->next;
	  if(pids[i]==-2 && level[i]==0)
	    {
	      commands_left--;
	      pids[i] = fork();
	      
	      if(pids[i] == -1)
		perror("ERROR: can't fork");
	      if(pids[i] == 0){
		/*child*/
		if(current_cmd->theCommand->type == SEQUENCE_COMMAND)
		  execute_sequence_command_in_parallel(current_cmd->theCommand);
		else
		  execute_command(current_cmd->theCommand);
		exit(current_cmd->theCommand->status);
	      }
	    }

	}
    
      current_cmd = s;
      /* parent: wait */
      for(i=0;i<size_command;i++)
	{
	  current_cmd = current_cmd->next;
	  if(pids[i]>0)
	    {
	      if(waitpid(pids[i],&status[i],0) < 0){
		perror("ERROR:serious mistake when waiting child return");
		exit(EXIT_FAILURE);
	      }
	      if (WIFEXITED(status[i]) == 0){
		perror("ERROR:cant successfully exit the program");
		exit(EXIT_FAILURE);
	      }
	      current_cmd->theCommand->status = WEXITSTATUS(status[i]);
	
	      for(j=0;j<size_command;j++){
		if(graph[j][i]){
		  graph[j][i]=0;
		  --level[j];
		}
	      }
	      pids[i]=-3;
	    }
	}
    }
  
  return current_cmd->theCommand;
}


