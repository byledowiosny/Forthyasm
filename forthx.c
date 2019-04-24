/* Forthx C-Stub */

#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <signal.h>
#include <sys/ioctl.h>
#include <termios.h>
#include <sys/ptrace.h>
#include <sys/wait.h>

/* Standard I/O Constants */

unsigned long sdin          = STDIN_FILENO;
unsigned long sdout         = STDOUT_FILENO;
unsigned long sderr         = STDERR_FILENO;

/* Disk I/O Constants */

unsigned long o_rdonly      = O_RDONLY;
unsigned long o_wronly      = O_WRONLY;
unsigned long o_rdwr        = O_RDWR;
unsigned long o_creat       = O_CREAT;
unsigned long o_trunc       = O_TRUNC;
unsigned long o_append      = O_APPEND;
unsigned long seek_set      = SEEK_SET;
unsigned long seek_cur      = SEEK_CUR;
unsigned long seek_end      = SEEK_END;

/* Signal Processing Constants */

unsigned long sigint        = SIGINT;
unsigned long sigsegv       = SIGSEGV;
unsigned long sigstop       = SIGSTOP;
unsigned long sigtstp       = SIGTSTP;
unsigned long sigfpe        = SIGFPE;
unsigned long sigwinch      = SIGWINCH;

unsigned long sa_resethand  = SA_RESETHAND;
unsigned long sa_restart    = SA_RESTART;
unsigned long sa_nodefer    = SA_NODEFER;
unsigned long sa_siginfo    = SA_SIGINFO;
unsigned long sa_onstack    = SA_ONSTACK;

/* ioctl() Constants */

unsigned long tiocgwinsz    = TIOCGWINSZ;
unsigned long fionread      = FIONREAD;
unsigned long tcsaflush     = TCSAFLUSH;

unsigned long tciflush      = TCIFLUSH;
unsigned long tcoflush      = TCOFLUSH;
unsigned long tcioflush     = TCIOFLUSH;

/* termios Constants */
/* Note that since these quantities function as bitmasks
  in 32-bit fields, they are specified as unsigned ints. */

unsigned long brkint        = BRKINT;
unsigned long icrnl         = ICRNL;
unsigned long ixon          = IXON;
unsigned long opost         = OPOST;
unsigned long cs8           = CS8;
unsigned long icanon        = ICANON;
unsigned long echo          = ECHO;
unsigned long iexten        = IEXTEN;
unsigned long isig          = ISIG;

/* ptrace Constants */

unsigned long pt_traceme    = PTRACE_TRACEME;
unsigned long pt_peektext   = PTRACE_PEEKTEXT;
unsigned long pt_poketext   = PTRACE_POKETEXT;
unsigned long pt_cont       = PTRACE_CONT;
unsigned long pt_singlestep = PTRACE_SINGLESTEP;
unsigned long pt_getregs    = PTRACE_GETREGS;
unsigned long pt_setregs    = PTRACE_SETREGS;
unsigned long pt_attach     = PTRACE_ATTACH;
unsigned long pt_detach     = PTRACE_DETACH;
unsigned long pt_seize      = PTRACE_SEIZE;
unsigned long pt_interrupt  = PTRACE_INTERRUPT;
unsigned long wstopped      = WSTOPPED;
unsigned long p_pid         = P_PID;

void _forthx();

/* ************ */

int main()
{
  _forthx();
  return 0;
}

