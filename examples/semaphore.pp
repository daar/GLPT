{$mode objfpc}

program semaphore;
uses
  GLPT;

var
  sem: pGLPT_Semaphore;
  thread: pGLPT_Thread;
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
  for i := 0 to 9 do
    begin
      value += 1;
      writeln(value);
      GLPT_Delay(100);
    end;
  GLPT_SemaphorePost(sem);
  result := 0;
end;

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
