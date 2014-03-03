// UCLA CS 111 Lab 1 command reading

#include "command.h"
#include "command-internals.h"
#include <error.h>

/* FIXME: You may need to add #include directives, macro definitions,
   static function definitions, etc.  */
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <ctype.h>
#include <error.h>
#include <stdio.h>
#include "alloc.h"

/* FIXME: Define the type 'struct command_stream' here.  This should
   complete the incomplete type declaration in command.h.  */   
   
typedef enum{
    WORD,
    AND,
    OR,
    SUBSHELL_LEFT,
    SUBSHELL_RIGHT,
    INPUT,
    OUTPUT,
    PIPELINE,
    NEW_LINE,
    SEMICOLON,
  }token_type;   

struct token{
  struct token *next;
  token_type    type;
  char         *chars;
}; 

/*check if a char is a word type*/
bool isWordChar(char a){
  if( isalnum(a) || 
      a == '!'  || a =='%' || a == '+' || a == '-' ||
      a == ','  || a =='.' || a == '/' || a == ':' ||      
      a == '@'  || a =='^' || a == '_' )
    return true;
  else
    return false;
}

/*get the next word*/
void getWord(int (*get_next_byte) (void *), void *get_next_byte_argument, 
	     char *c, char **the_word){
  char a[100];
  int i = 0;
  while(isWordChar(*c)){
      a[i]=*c;
      i++;
      *c=(char)get_next_byte(get_next_byte_argument);
    }
  a[i]='\0';
  
  *the_word= (char*) checked_malloc(1 + i*sizeof(char)); 
  strcpy(*the_word,a);
}

/*add one token to the token link list*/
void addToken(char *words, token_type type, struct token **current_token){
  struct token *newToken = (struct token*) checked_malloc(sizeof(struct token));
  newToken->type  = type;
  newToken->chars = words;
  newToken->next  = NULL;
  if (*current_token != NULL)
    (*current_token)->next = newToken;
  *current_token  = newToken;
} 

/* build a simple command only consist of words*/
command_t construct_simple_command(struct token **current_token)
{
  command_t newCommand_t  = (struct command*) checked_malloc(sizeof (struct command));
  char** buffer           = checked_malloc(2*sizeof (char*));
  int i=0;
  while((*current_token) != NULL && (*current_token)->type == WORD){
    buffer         =  checked_realloc(buffer,(i+2)*sizeof(char*));
    buffer[i]      = (*current_token)->chars;
    *current_token = (*current_token)->next;
    i++;
  }
  buffer[i]='\0';
    
  newCommand_t->type   = SIMPLE_COMMAND;
  newCommand_t->status = -1;
  newCommand_t->input  = NULL;
  newCommand_t->output = NULL;
  newCommand_t->u.word = buffer;
  return newCommand_t;
}


command_t build_command(command_t a, command_t b, enum command_type type){
 
 command_t compound_command_t =(struct command*) 
                                 checked_malloc(sizeof (struct command));
  /*
  if( type == PIPE_COMMAND)
    if(a->type == AND_COMMAND || a->type == OR_COMMAND || a->type == SEQUENCE_COMMAND){
      command_t *current_command = checked_malloc(sizeof (struct command));;
      *current_command = a->u.command[1];
      while(true)
	{
	  if((*current_command)->type != AND_COMMAND && 
	     (*current_command)->type != OR_COMMAND && 
	     (*current_command)->type != SEQUENCE_COMMAND){ 
	    *current_command = build_command(*current_command,b,type);
	    return a;
	  }else{
	    if((*current_command)==NULL)
	      error(1,0,"ERROR! #000 Wrong structure");
	    *current_command = (*current_command)->u.command[1];
	  }
	}
    }
  */
  compound_command_t->status = -1;
  compound_command_t->input  = NULL;  
  compound_command_t->output = NULL;

  if(   type == SEQUENCE_COMMAND ||
	b->type == SUBSHELL_COMMAND || 
	b->type == SIMPLE_COMMAND){
    compound_command_t->type   = type;
    compound_command_t->u.command[0]= a;
    compound_command_t->u.command[1]= b;
  }else if( b->type == AND_COMMAND ||
	    b->type == OR_COMMAND ||
	    b->type == SEQUENCE_COMMAND){
    compound_command_t->type   = b->type;
    compound_command_t->u.command[0]= build_command(a,b->u.command[0],type);
    compound_command_t->u.command[1]= b->u.command[1];
  }else if ( a->type == AND_COMMAND ||
	    a->type == OR_COMMAND ||
	 a->type == SEQUENCE_COMMAND){
    compound_command_t->type   = a->type;
    compound_command_t->u.command[0]= a->u.command[0];
    compound_command_t->u.command[1]= build_command(a->u.command[1],b,type);
  }else{
    compound_command_t->type   = type;
    compound_command_t->u.command[0]= a;
    compound_command_t->u.command[1]= b;
  }
  return compound_command_t;
}

/* build complete command*/
command_t construct_complete_command(struct token **current_token, 
				     int *current_line_number )
{
  if(*current_token == NULL)
    return NULL;

  /*remove char '\n' */
  while((*current_token) != NULL && (*current_token)->type == NEW_LINE){
    ++ *current_line_number;
    *current_token = (*current_token)->next;
    
    if(*current_token == NULL)
      return NULL;
  }

  //  if((*current_token)->type != WORD)
  //error(1,0,"ERROR! #001 Wrong grammar in line:%d\n",*current_line_number);
 
  command_t new_command_t;    
  command_t new_subcommand_t;
  /*build simple command or subshell command w/o I/O */
  switch((*current_token)->type){
    case WORD:
      new_command_t = construct_simple_command(current_token);  
      break;
    case SUBSHELL_LEFT:
      *current_token = (*current_token)->next;
      new_subcommand_t = construct_complete_command(current_token,current_line_number);
      if(new_subcommand_t == NULL || (*current_token)->type != SUBSHELL_RIGHT )
	error(1,0,"ERROR! #002  Wrong subshell command in line:%d\n",
	      *current_line_number);
      
      *current_token = (*current_token)->next;

      new_command_t         = checked_malloc (sizeof (struct command));
      new_command_t->type   = SUBSHELL_COMMAND;
      new_command_t->status = -1;
      new_command_t->u.subshell_command = new_subcommand_t;
      break;
    default:
      error(1,0,"ERROR! #003  Wrong grammar in line:%d\n",*current_line_number);
      break;
    }
  
  if(*current_token == NULL)
    return new_command_t;
  
  /*add I/O if available*/
   while((*current_token)->type == INPUT || (*current_token)->type == OUTPUT){
     if((*current_token)->type == INPUT){
       *current_token = (*current_token)->next;
       if((*current_token)->type !=  WORD ||(*current_token)->chars == NULL)
	 error(1,0,"ERROR! #004 Wrong grammar in line:%d\n",*current_line_number);
       new_command_t->input  = (*current_token)->chars;
     }else{
       *current_token = (*current_token)->next;
       if((*current_token)->type !=  WORD ||(*current_token)->chars == NULL)
	 error(1,0,"ERROR! #004 Wrong grammar in line:%d\n",*current_line_number);
       new_command_t->output = (*current_token)->chars;
    }

    *current_token = (*current_token)->next;
    
    if(*current_token == NULL)
      return new_command_t;
     }
   

  if ((*current_token)->type == SUBSHELL_RIGHT)
    return new_command_t;

  /*if continuing with a char '\n', return this command*/
  if((*current_token)->type == NEW_LINE){
    ++ *current_line_number;
    *current_token = (*current_token)->next;
    return new_command_t;
  }
  
  /*then it is a compound command*/
  enum command_type compound_command_type;
 
  switch((*current_token)->type){
  case AND:
    compound_command_type = AND_COMMAND;
    break;
  case OR:
    compound_command_type = OR_COMMAND;
    break;
  case PIPELINE:
    compound_command_type = PIPE_COMMAND;
    break;
  case SEMICOLON:
    compound_command_type = SEQUENCE_COMMAND;
    break;
  default:
    error(1,0,"ERROR! #005 Wrong grammar in line:%d\n",*current_line_number);
  } 

  *current_token = (*current_token)->next;

  command_t second_command_t;
  second_command_t =construct_complete_command(current_token,current_line_number);

  if(second_command_t == NULL)
    error(1,0,"ERROR! #006 Wrong grammar in line:%d\n",*current_line_number);
  
  return build_command(new_command_t,second_command_t,compound_command_type);
}


/* Create a command stream from GETBYTE and ARG.  A reader of
   the command stream will invoke GETBYTE (ARG) to get the next byte.
   GETBYTE will return the next input byte, or a negative number
   (setting errno) on failure.  */
command_stream_t make_command_stream (int (*get_next_byte) (void *),
				      void *get_next_byte_argument)
{
  int current_line_number =1;
  struct token *head_token=NULL, *current_token=NULL;
  char c = (char) get_next_byte(get_next_byte_argument);
  char *current_word=NULL;
  int bracket_number = 0;

  while(c == ' ' || c =='\t')
	  c = (char) get_next_byte(get_next_byte_argument);

  /*load all chars and then make a token link list*/
  while(c != EOF ){
    if(isWordChar(c)== true){
	  getWord(get_next_byte,get_next_byte_argument,&c,&current_word);
	  addToken(current_word,WORD,&current_token);
    }
    else
      switch(c){
      case '|':
	c = (char) get_next_byte(get_next_byte_argument);
	if(c !='|')
	  addToken(&c,PIPELINE,&current_token);
	else{
	  char b[2] = "||";
	  addToken(b,OR,&current_token);
	  c = (char) get_next_byte(get_next_byte_argument);
	  if(c =='|')
	    error(1,0,"ERROR! #008 invalid grammar in line:%d\n",current_line_number);
	}
	break;
      case '&':  
	c = (char) get_next_byte(get_next_byte_argument);
	if(c =='&'){
	  char b[2] = "&&";
	  addToken(b,AND,&current_token);
	  c = (char) get_next_byte(get_next_byte_argument);
	  if(c =='&')
	    error(1,0,"ERROR! #009 invalid grammar in line:%d\n",current_line_number);
	}
	else
	  error(1,0,"ERROR! #010 invalid grammar in line:%d\n",current_line_number);
	break;
      case '\n':
	current_line_number++;
	addToken(&c,NEW_LINE,&current_token);
	c = (char) get_next_byte(get_next_byte_argument);
	break;
      case ';':   
	addToken(&c,SEMICOLON,&current_token);
	c = (char) get_next_byte(get_next_byte_argument);
	break;
      case '(':
	bracket_number++;
	addToken(&c,SUBSHELL_LEFT,&current_token);
	c = (char) get_next_byte(get_next_byte_argument);
	break;
      case ')': 
	bracket_number--;
	addToken(&c,SUBSHELL_RIGHT,&current_token);
	c = (char) get_next_byte(get_next_byte_argument);
	break;
      case '<':
	addToken(&c,INPUT,&current_token);
	c = (char) get_next_byte(get_next_byte_argument);
	break;
      case '>':
	addToken(&c,OUTPUT,&current_token);
	c = (char) get_next_byte(get_next_byte_argument);
	break;
      case '#':
	//Current Behavior:simply delete all comments
	while(c !='\n' && c!=EOF)
	  c = (char) get_next_byte(get_next_byte_argument);
	break;
      default:
	error(1,0, "ERROR! #011 Unsupported char \'%c\' in line: %d\n", 
	      c,current_line_number);
      }
    while(c == ' ' || c =='\t')
      c = (char) get_next_byte(get_next_byte_argument);
    
    if(head_token==NULL)
      head_token = current_token;
    
    if (bracket_number < 0)
      error(1,0,"ERROR! #012 Wrong Number of '(' and ')'in line: %d\n",
	    current_line_number);
  }
  
  if (bracket_number != 0)
    error(1,0,"ERROR! #013 Wrong Number of '(' and ')'in line: %d\n",current_line_number);
  
  /*build a command stream from tokens*/
  current_token = head_token;
  current_line_number = 1;
  
  command_stream_t stream_head    = (command_stream_t) 
                                     checked_malloc(sizeof (struct command_stream)); ;
  command_stream_t stream_current = NULL;
  
  stream_head->next=NULL;
  
  while(current_token !=NULL)
    {
      command_t new_command=construct_complete_command(&current_token,
						       &current_line_number);
      if(new_command == NULL) break;
      command_stream_t new_stream = (command_stream_t) 
	                             checked_malloc(sizeof (struct command_stream)); 
      new_stream->next       = NULL;
      new_stream->prev       = stream_current;      
      new_stream->theCommand = new_command;
      
      if (stream_head->next == NULL){
	stream_head->next    = new_stream;
	stream_current       = new_stream;
      }
      else{
	stream_current->next = new_stream;
	stream_current       = new_stream;
      }
    }
  return stream_head;
}

/* Read a command from S; return it, or NULL on EOF.  If there is
   an error, report the error and exit instead of returning.  */
command_t read_command_stream (command_stream_t s)
{
  if( s->next==NULL)
    return NULL;
  
  command_stream_t current_stream  = s->next;
  command_t command = current_stream->theCommand;
  if(current_stream->next == NULL)
    s->next = NULL;
  else{
    s->next->next->prev=s;
    s->next=s->next->next;
  }
  free(current_stream);

  return command;
}
/*
 structure of command stream:
 
 --------------------
 | stream _head     |
 | thecommand=NULL  |
 | prev = NULL      |
 | next             |
 -----\--------------
       \       ^
  |-----v------|--|       |--------------|
  |  stream 1  |  |       |  stream 2    |
  |  command 1 |  |       |  command 2   |
  |  prev = ----  |<---------prev        |
  |               |       |              |
  |  next---------------->|  next-------------->
  |---------------|       |--------------|

*/
