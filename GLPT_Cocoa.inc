function Cocoa_GetTime: double;
begin
  {$note implement this}
end;

function Cocoa_Terminate: boolean;
begin
  {$note implement this}

  exit(True);
end;

procedure Cocoa_DestroyWindow(var win: pGLPTwindow);
begin
  {$note implement this}
end;

function Cocoa_CreateWindow(win: pGLPTwindow; posx, posy, sizex, sizey: integer; title: PChar): boolean;
begin
  {$note implement this}

  exit(True);
end;

function Cocoa_MakeCurrent(win: pGLPTwindow): boolean;
begin
  {$note implement this}

  exit(True);
end;

procedure Cocoa_SwapBuffers(win: pGLPTwindow);
begin
  {$note implement this}
end;

procedure Cocoa_GetFrameBufferSize(win: pGLPTwindow; out width, height: integer);
begin
  {$note implement this}
end;

procedure Cocoa_PollEvents;
begin
  {$note implement this}
end;

procedure Cocoa_GetDisplayCoords(var dr: GLPTRect);
begin
  {$note implement this}
end;

function Cocoa_GetKeyboardShiftState: TShiftState;
var
  State: array[byte] of byte;
begin
  {$note implement this}

  Result := [];
  //if (State[VK_SHIFT] and 128) <> 0 then
  //  Include(Result, ssShift);
  //if (State[VK_MENU] and 128) <> 0 then
  //  Include(Result, ssAlt);
  //if (State[VK_CONTROL] and 128) <> 0 then
  //  Include(Result, ssCtrl);
  //if (State[VK_LBUTTON] and 128) <> 0 then
  //  Include(Result, ssLeft);
  //if (State[VK_RBUTTON] and 128) <> 0 then
  //  Include(Result, ssRight);
  //if (State[VK_MBUTTON] and 128) <> 0 then
  //  Include(Result, ssMiddle);
  //if (State[VK_CAPITAL] and 1) <> 0 then
  //  Include(Result, ssCaps);
  //if (State[VK_NUMLOCK] and 1) <> 0 then
  //  Include(Result, ssNum);
  //if (State[VK_SCROLL] and 1) <> 0 then
  //  Include(Result, ssScroll);
end;

function Cocoa_Init: boolean;
begin
  {$note implement this}

  exit(True);
end;

procedure Cocoa_SetCursor(cursor: byte);
begin
  {$note implement this}
end;
