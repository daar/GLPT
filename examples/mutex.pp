{$mode objfpc}

program mutex;
uses
  GLPT;

var
  lock: pGLPT_Mutex;

procedure error_callback(error: integer; description: string);
begin
  writeln(stderr, description);
end;

function threadA_callback (userData: pointer): integer;
var
  i: integer;
begin
  for i := 0 to 10 - 1 do
    begin
      GLPT_LockMutex(lock);
      writeln('thread A counter: ', i);
      GLPT_UnlockMutex(lock);
      GLPT_Delay(150);
    end;
  result := i;
end;

function threadB_callback (userData: pointer): integer;
var
  i: integer;
begin
  for i := 0 to 10 - 1 do
    begin
      GLPT_LockMutex(lock);
      writeln('thread B counter: ', i);
      GLPT_UnlockMutex(lock);
      GLPT_Delay(150);
    end;
  result := i;
end;

var
  threadA, threadB: pGLPT_Thread;
  status: integer;
begin
  GLPT_SetErrorCallback(@error_callback);
  if not GLPT_Init then
    halt(-1);

  lock := GLPT_CreateMutex;

  threadA := GLPT_CreateThread(@threadA_callback, 'A');
  threadB := GLPT_CreateThread(@threadB_callback, 'B');

  GLPT_WaitThread(threadA, status);
  GLPT_WaitThread(threadB, status);

  writeln('done');

  GLPT_Terminate;
end.
