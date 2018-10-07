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

  procedure write_FPS;
  var
    currentTime: double;
    fps: string;
  begin
    // measure FPS
    currentTime := GLPT_GetTime;
    inc(nbFrames);

    if currentTime - lastTime >= 1 then
    begin
      fps := format('[FPS: %3.0f]', [nbFrames / (currentTime - lastTime)]);

      writeln(GLPT_GetTime:0:3, ' ', fps);

      nbFrames := 0;
      lastTime := GLPT_GetTime;
    end;
  end;

  procedure error_callback(const error: integer; const description: string);
  begin
    writeln(stderr, description);
  end;

  procedure event_callback(event: pGLPT_MessageRec);
  begin
    case event^.mcode of

      GLPT_MESSAGE_KEYPRESS:
      begin
        writeln(event^.params.keyboard.keychar);

        if event^.params.keyboard.keychar = GLPT_KEY_ESCAPE then
          GLPT_SetWindowShouldClose(event^.win, True);
      end;

      GLPT_MESSAGE_MOUSEDOWN:
        writeln(event^.params.mouse.buttons);
    end;
  end;

begin
  GLPT_SetErrorCallback(@error_callback);

  if not GLPT_Init then
    halt(-1);

  window := GLPT_CreateWindow(0, 0, width, height, 'Simple example');
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
    write_FPS;

    glViewport(0, 0, width, height);
    glClear(GL_COLOR_BUFFER_BIT);

    glMatrixMode(GL_PROJECTION);
    glLoadIdentity;
    glOrtho(-ratio, ratio, -1, 1, 1, -1);
    glMatrixMode(GL_MODELVIEW);

    glLoadIdentity;
    rotate := (GLPT_GetTime * 50);
    rotate := rotate - int(rotate / 360) * 360;
    glRotatef(rotate, 0, 0, 1);

    glBegin(GL_TRIANGLES);
      glColor3f(1, 0, 0);
      glVertex3f(-0.6, -0.4, 0);
      glColor3f(0, 1, 0);
      glVertex3f(0.6, -0.4, 0);
      glColor3f(0, 0, 1);
      glVertex3f(0, 0.6, 0);
    glEnd;

    GLPT_SwapBuffers(window);
    GLPT_PollEvents;
  end;

  GLPT_DestroyWindow(window);

  GLPT_Terminate;
end.
