
{ Threads }
const
  GLPT_THREAD_DEFAULT_STACK_SIZE = 4096;

type
  pGLPT_Thread = ^GLPT_Thread;
  GLPT_ThreadFunction = function (userData: pointer): integer;
  GLPT_Thread_ID = ptrint;
  GLPT_Thread = record
    start: GLPT_ThreadFunction;
    id: GLPT_Thread_ID;
    handle: pointer;
    name: string;
    userData: pointer;
    status: integer;
    stacksize: longword;
  end;

function GLPT_CreateThread(start: GLPT_ThreadFunction; 
                           name: string = ''; 
                           userData: pointer = nil; 
                           stacksize: integer = GLPT_THREAD_DEFAULT_STACK_SIZE): pGLPT_Thread;
procedure GLPT_DetachThread(thread: pGLPT_Thread);
procedure GLPT_WaitThread(var thread: pGLPT_Thread; var status: integer);
function GLPT_ThreadID: GLPT_Thread_ID;
function GLPT_RunThread(thread: pGLPT_Thread): pointer; cdecl;

{ Mutex }

type
  pGLPT_Mutex = ^GLPT_Mutex;
  GLPT_Mutex = record
    {$IFDEF DARWIN}
      handle: pthread_mutex_t;
    {$ENDIF}
  end;

function GLPT_CreateMutex: pGLPT_Mutex;
procedure GLPT_DestroyMutex(var mutex: pGLPT_Mutex);
function GLPT_LockMutex(mutex: pGLPT_Mutex): integer;
function GLPT_UnlockMutex(mutex: pGLPT_Mutex): integer;
function GLPT_TryLockMutex(mutex: pGLPT_Mutex): integer;

{ Condition Variable }

type
  pGLPT_Condition = ^GLPT_Condition;
  GLPT_Condition = record
    {$IFDEF DARWIN}
      handle: pthread_cond_t;
    {$ENDIF}
  end;

function GLPT_CreateCondition: pGLPT_Condition;
procedure GLPT_DestroyCondition(var condition: pGLPT_Condition);
function GLPT_ConditionSignal(condition: pGLPT_Condition): integer;
function GLPT_ConditionBroadcast(condition: pGLPT_Condition): integer;
function GLPT_ConditionWaitTimeout(condition: pGLPT_Condition; mutex: pGLPT_Mutex; ms: longint): integer;
function GLPT_ConditionWait(condition: pGLPT_Condition; mutex: pGLPT_Mutex): integer;

{ Semaphore }

type
  pGLPT_Semaphore = ^GLPT_Semaphore;
  GLPT_Semaphore = record
    {$IFDEF DARWIN}
    handle: psem_t;
    name: string;
    {$ENDIF}
  end;

function GLPT_CreateSemaphore(initial_value: integer = 0): pGLPT_Semaphore;
procedure GLPT_DestroySemaphore(var sem: pGLPT_Semaphore);
function GLPT_SemaphoreTryWait(sem: pGLPT_Semaphore): integer;
function GLPT_SemaphoreWait(sem: pGLPT_Semaphore): integer;
function GLPT_SemaphoreWaitTimeout(sem: pGLPT_Semaphore; timeout: longint): integer;
function GLPT_SemaphoreValue(sem: pGLPT_Semaphore): integer;
function GLPT_SemaphorePost(sem: pGLPT_Semaphore): integer;
