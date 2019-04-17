{$mode objfpc}

program condition_variable;
uses
  GLPT;

var
  lock: pGLPT_Mutex;
  cond: pGLPT_Condition;
  condition_value:  boolean = false;

procedure error_callback(error: integer; description: string);
begin
  writeln(stderr, description);
end;

function threadA_callback (userData: pointer): integer;
begin
  GLPT_LockMutex(lock);
  writeln('thread A waiting...');
  while not condition_value do
    begin
      GLPT_ConditionWait(cond, lock);
      writeln('thread A got signal');
    end;
  GLPT_UnlockMutex(lock);
end;


function threadB_callback (userData: pointer): integer;
begin
  GLPT_LockMutex(lock);
  condition_value := true;
  writeln('thread B signaling');
  GLPT_ConditionSignal(cond);
  GLPT_UnlockMutex(lock);
end;

var
  window: pGLPTWindow;
  threadA, threadB: pGLPT_Thread;
  status: integer;
begin
  GLPT_SetErrorCallback(@error_callback);
  if not GLPT_Init then
    halt(-1);

  lock := GLPT_CreateMutex;
  cond := GLPT_CreateCondition;

  threadA := GLPT_CreateThread(@threadA_callback, 'A');
  threadB := GLPT_CreateThread(@threadB_callback, 'B');

  GLPT_WaitThread(threadA, status);
  GLPT_WaitThread(threadB, status);

  writeln('done');

  GLPT_Terminate;
end.
