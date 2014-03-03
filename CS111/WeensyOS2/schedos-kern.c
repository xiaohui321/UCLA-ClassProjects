#include "schedos-kern.h"
#include "x86.h"
#include "lib.h"

/*****************************************************************************
 * schedos-kern
 *
 *   This is the schedos's kernel.
 *   It sets up process descriptors for the 4 applications, then runs
 *   them in some schedule.
 *
 *****************************************************************************/

// The program loader loads 4 processes, starting at PROC1_START, allocating
// 1 MB to each process.
// Each process's stack grows down from the top of its memory space.
// (But note that SchedOS processes, like MiniprocOS processes, are not fully
// isolated: any process could modify any part of memory.)

#define NPROCS		5
#define PROC1_START	0x200000
#define PROC_SIZE	0x100000

//please make sure the sum of tickets is smaller than or equals to the max num, 
//as the functions below will not check it
#define MAX_TICKET_NUM 80
#define P_1_INIT_TICKETS_NUM 40
#define P_2_INIT_TICKETS_NUM 20
#define P_3_INIT_TICKETS_NUM 10
#define P_4_INIT_TICKETS_NUM 10

static int current_total_ticket_num;
static pid_t tickets[MAX_TICKET_NUM];

void remove_tickets(pid_t pid){
  int i,j;
  for(i = current_total_ticket_num - 1 ; i >= 0;  i--)
    if(tickets[i] == pid){
      current_total_ticket_num -- ;
      for(j = i; j < current_total_ticket_num; j++)
	tickets[j] = tickets[j+1];
    }
}

void tickets_init(){
  current_total_ticket_num = 0;

  current_total_ticket_num += P_1_INIT_TICKETS_NUM;
  int i;
  for(i=0; i<current_total_ticket_num; i++)
    tickets[i] = 1;

  current_total_ticket_num += P_2_INIT_TICKETS_NUM;
  for(; i<current_total_ticket_num; i++)
    tickets[i] = 2;

  current_total_ticket_num += P_3_INIT_TICKETS_NUM;
  for(; i<current_total_ticket_num; i++)
    tickets[i] = 3;

  current_total_ticket_num += P_4_INIT_TICKETS_NUM;
  for(; i<current_total_ticket_num; i++)
    tickets[i] = 4;
}

static int random_seed;
//this function does not really generate a random number

int random(){
  int old_random=random_seed;
  random_seed += 12345;
  return old_random *(old_random + 17);
}

pid_t select_a_ticket(){
  return tickets[random()%current_total_ticket_num];
}


// +---------+-----------------------+--------+---------------------+---------/
// | Base    | Kernel         Kernel | Shared | App 0         App 0 | App 1
// | Memory  | Code + Data     Stack | Data   | Code + Data   Stack | Code ...
// +---------+-----------------------+--------+---------------------+---------/
// 0x0    0x100000               0x198000 0x200000              0x300000
//
// The program loader puts each application's starting instruction pointer
// at the very top of its stack.
//
// System-wide global variables shared among the kernel and the four
// applications are stored in memory from 0x198000 to 0x200000.  Currently
// there is just one variable there, 'cursorpos', which occupies the four
// bytes of memory 0x198000-0x198003.  You can add more variables by defining
// their addresses in schedos-symbols.ld; make sure they do not overlap!


// A process descriptor for each process.
// Note that proc_array[0] is never used.
// The first application process descriptor is proc_array[1].
static process_t proc_array[NPROCS];

// A pointer to the currently running process.
// This is kept up to date by the run() function, in mpos-x86.c.
process_t *current;

// The preferred scheduling algorithm.
int scheduling_algorithm;

/*****************************************************************************
 * start
 *
 *   Initialize the hardware and process descriptors, then run
 *   the first process.
 *
 *****************************************************************************/

void
start(void)
{
	int i;

	// Set up hardware (schedos-x86.c)
	segments_init();
	interrupt_controller_init(0);
	console_clear();

	tickets_init();
	random_seed = 11;
	// Initialize process descriptors as empty
	memset(proc_array, 0, sizeof(proc_array));
	for (i = 0; i < NPROCS; i++) {
		proc_array[i].p_pid = i;
		proc_array[i].p_state = P_EMPTY;
	}

	// Set up process descriptors (the proc_array[])
	for (i = 1; i < NPROCS; i++) {
		process_t *proc = &proc_array[i];
		uint32_t stack_ptr = PROC1_START + i * PROC_SIZE;

		// Initialize the process descriptor
		special_registers_init(proc);

		// Set ESP
		proc->p_registers.reg_esp = stack_ptr;

		// Load process and set EIP, based on ELF image
		program_loader(i - 1, &proc->p_registers.reg_eip);

		// Mark the process as runnable!
		proc->p_state = P_RUNNABLE;

		proc->p_priority = 0;
		proc->p_proportion = 1;
		proc->p_time_executed = 0;
	}

	// Initialize the cursor-position shared variable to point to the
	// console's first character (the upper left).
	cursorpos = (uint16_t *) 0xB8000;
	lock =(uint32_t) 0;
	// Initialize the scheduling algorithm.
	scheduling_algorithm = 4;

	// Switch to the first process.
	proc_array[1].p_time_executed ++;
	run(&proc_array[1]);

	// Should never get here!
	while (1)
		/* do nothing */;
}



/*****************************************************************************
 * interrupt
 *
 *   This is the weensy interrupt and system call handler.
 *   The current handler handles 4 different system calls (two of which
 *   do nothing), plus the clock interrupt.
 *
 *   Note that we will never receive clock interrupts while in the kernel.
 *
 *****************************************************************************/

void
interrupt(registers_t *reg)
{
	// Save the current process's register state
	// into its process descriptor
	current->p_registers = *reg;

	switch (reg->reg_intno) {

	case INT_SYS_YIELD:
		// The 'sys_yield' system call asks the kernel to schedule
		// the next process.
		schedule();

	case INT_SYS_EXIT:
		// 'sys_exit' exits the current process: it is marked as
		// non-runnable.
		// The application stored its exit status in the %eax register
		// before calling the system call.  The %eax register has
		// changed by now, but we can read the application's value
		// out of the 'reg' argument.
		// (This shows you how to transfer arguments to system calls!)
	        remove_tickets(current->p_pid);
		current->p_state = P_ZOMBIE;
		current->p_exit_status = reg->reg_eax;
		schedule();

	case INT_SYS_USER1:
	        /*set the priority for the current process*/
	        current->p_priority = reg->reg_eax;
	        run(current);

	case INT_SYS_USER2:
		/* set the proportion value. */
	        current->p_proportion = reg->reg_eax;
		run(current);

	case INT_CLOCK:
		// A clock interrupt occurred (so an application exhausted its
		// time quantum).
		// Switch to the next runnable process.
		schedule();

	default:
		while (1)
			/* do nothing */;

	}
}



/*****************************************************************************
 * schedule
 *
 *   This is the weensy process scheduler.
 *   It picks a runnable process, then context-switches to that process.
 *   If there are no runnable processes, it spins forever.
 *
 *   This function implements multiple scheduling algorithms, depending on
 *   the value of 'scheduling_algorithm'.  We've provided one; in the problem
 *   set you will provide at least one more.
 *
 *****************************************************************************/

void
schedule(void)
{
	pid_t pid = current->p_pid;
	pid_t current_process;
	pid_t final_process = 9999;
	pid_t i;
	int highest_priority;
	int current_priority;
	float highest_proportion = 9999;
	float current_proportion;
	if (scheduling_algorithm == 0)
	  /* round robin */
	  while (1) {
	    pid = (pid + 1) % NPROCS;
	    
	    // Run the selected process, but skip
	    // non-runnable processes.
	    // Note that the 'run' function does not return.
	    if (proc_array[pid].p_state == P_RUNNABLE)
	      run(&proc_array[pid]);
	  }
	else if (scheduling_algorithm == 1)
	  /* pre-defined priority */
	  for(i = 1; i <NPROCS;i++){
	    if (proc_array[i].p_state == P_RUNNABLE)
              run(&proc_array[i]);
	  }
	else if(scheduling_algorithm == 2){
	  /* 4A: program-defined priprity */
	  highest_priority = 9999;
	  for(i = 1;i <=NPROCS;i++){
	    current_process = (i + pid)% NPROCS;
	    current_priority = proc_array[current_process].p_priority;
	    if(proc_array[current_process].p_state == P_RUNNABLE 
	       && current_priority < highest_priority){
	      highest_priority = current_priority;
	      final_process = current_process;
	    }
	  }
	  if(final_process != 9999)
	    run(&proc_array[final_process]);
	}
	else if(scheduling_algorithm ==3){
	  /* 4B: proportional-share */ 

	  for(i = 1; i <=NPROCS; i++){
	    current_process = (i + pid)% NPROCS;
	    current_proportion = (float)proc_array[current_process].p_time_executed/(float)proc_array[current_process].p_proportion;
	    	    
	    if(proc_array[current_process].p_state == P_RUNNABLE && 
	       current_proportion <= highest_proportion){
	      highest_proportion = current_proportion;
	      final_process = current_process;
	    }
	  }
	  if(final_process != 9999){
	    proc_array[final_process].p_time_executed ++;
	    run(&proc_array[final_process]);
	  }
	}else if(scheduling_algorithm ==4){
	  /* 7: lottery */
	  int j = 1;
	  while(1){
	    j++;
	    final_process = select_a_ticket();
	    if(proc_array[final_process].p_state == P_RUNNABLE)
	      run(&proc_array[final_process]);
	    if (j > 10){
	      scheduling_algorithm = 0;
	      break;
	    }
	  }	  
	  //continue with round robin if lottery failed to continue
	  while (1) {
	    pid = (pid + 1) % NPROCS;
	    
	    if (proc_array[pid].p_state == P_RUNNABLE)
	      run(&proc_array[pid]);
	  }
	}else{
	  // If we get here, we are running an unknown scheduling algorithm.
	  cursorpos = console_printf(cursorpos, 0x100,
			   "\nUnknown scheduling algorithm %d\n", scheduling_algorithm);
	}
	
	while (1);
}
