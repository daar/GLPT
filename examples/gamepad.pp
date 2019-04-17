program simple;

{$mode objfpc}

uses
  SysUtils,
  GL, GLPT;

var
  window: pGLPTWindow;
  ratio: double;
  width: integer = 640;
  height: integer = 480;
  nbFrames: longint = 0;
  lastTime: double;
  rotate: double;

  procedure error_callback(error: integer; description: string);
  begin
    writeln(stderr, description);
  end;

  procedure event_callback(event: pGLPT_MessageRec);
  begin
    case event^.mcode of

      GLPT_MESSAGE_KEYPRESS:
      begin
        //writeln(event^.params.keyboard.keycode);
        if event^.params.keyboard.keycode = GLPT_KEY_ESCAPE then
          GLPT_SetWindowShouldClose(event^.win, True);
      end;

      GLPT_MESSAGE_MOUSEDOWN:
        writeln('GLPT_MESSAGE_MOUSEDOWN:',event^.params.mouse.buttons);
    end;
  end;

begin
  GLPT_SetErrorCallback(@error_callback);

  if not GLPT_Init([GLPT_FlagGamepad]) then
    halt(-1);

  window := GLPT_CreateWindow(GLPT_WINDOW_POS_CENTER, GLPT_WINDOW_POS_CENTER, width, height, 'Simple example', GLPT_GetDefaultContext);
  if window = nil then
  begin
    GLPT_Terminate;
    halt(-1);
  end;

  ratio := width / height;
  window^.event_callback := @event_callback;

  writeln('GLPT version: ', GLPT_GetVersionString);
  writeln('OpenGL version: ', glGetString(GL_VERSION));

  while not GLPT_WindowShouldClose(window) do
  begin
    GLPT_SwapBuffers(window);
    GLPT_PollEvents;
  end;

  GLPT_DestroyWindow(window);
  GLPT_Terminate;
end.
