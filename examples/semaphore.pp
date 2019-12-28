{$mode objfpc}

program semaphore;
uses
  GLPT;

var
  sem: pGLPT_Semaphore;
  value: integer = 0;

procedure error_callback(error: integer; description: string);
begin
  writeln(stderr, description);
  halt(-1);
end;

function thread_callback (userData: pointer): integer;
var
  i: integer;
begin
  for i := 0 to 10 do
    begin
      value += 1;
      GLPT_Delay(25);
    end;
  GLPT_SemaphorePost(sem);
end;

var
  thread: pGLPT_Thread;
begin
  GLPT_SetErrorCallback(@error_callback);
  if not GLPT_Init then
    halt(-1);

  sem := GLPT_CreateSemaphore(0);
  thread := GLPT_CreateThread(@thread_callback);
  
  writeln('waiting for semaphore...');
  GLPT_SemaphoreWait(sem);
  writeln('done');

  GLPT_DestroySemaphore(sem);
  GLPT_Terminate;
end.
