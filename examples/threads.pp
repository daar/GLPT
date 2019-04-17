{$mode objfpc}

program threads;
uses
  GLPT;

function thread_callback (userData: pointer): integer;
var
  i: integer;
begin
  for i := 0 to 10 - 1 do
    begin
      writeln('thread counter: ', i);
      GLPT_Delay(150);
    end;
  result := i;
end;

procedure error_callback(error: integer; description: string);
begin
  writeln(stderr, description);
end;

var
  window: pGLPTWindow;
  thread: pGLPT_Thread;
  threadReturnValue: integer;
begin
  GLPT_SetErrorCallback(@error_callback);
  if not GLPT_Init then
    halt(-1);

  thread := GLPT_CreateThread(@thread_callback, 'TestThread');
  if thread = nil then
    writeln('GLPT_CreateThread failed: ', GLPT_GetLastError)
  else
    begin
      GLPT_WaitThread(thread, threadReturnValue);
      writeln('thread returned value: ', threadReturnValue);
    end;

  GLPT_Terminate;
end.
