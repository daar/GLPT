function X11_GetTime: double;
var
  now: timespec;
begin
  if clock_gettime(CLOCK_MONOTONIC, @now) <> 0 then
    exit(0);

  exit(now.tv_sec + now.tv_nsec / 1000000000);
end;

(*
 * Create an RGB, double-buffered window.
 * Return the window and context handles.
 *)
function X11_CreateWindow(win: pGLPTwindow; posx, posy, sizex, sizey: integer; title: PChar): boolean;
var
  attribs: array [0..10] of integer = (GLX_RGBA, GLX_RED_SIZE,
                                       1, GLX_GREEN_SIZE, 1, GLX_BLUE_SIZE, 1, GLX_DOUBLEBUFFER,
                                       GLX_DEPTH_SIZE, 1, None);
  scrnum: integer;
  attr: TXSetWindowAttributes;
  mask: cardinal;
  root: TWindow;
  visinfo: pXVisualInfo;
  sizehints: TXSizeHints;
begin
  win^.dpy := XOpenDisplay(nil);
  if win^.dpy = nil then
    begin
      glptError(GLPT_ERROR_PLATFORM, 'could not open display ' + title);
      exit(False);
    end;

  scrnum := DefaultScreen(win^.dpy);
  root := RootWindow(win^.dpy, scrnum);

  visinfo := glXChooseVisual(win^.dpy, scrnum, attribs);
  if visinfo = nil then
    begin
      glptError(GLPT_ERROR_PLATFORM, 'could not get an RGB, double-buffered visual');
      exit(False);
    end;

  //window attributes
  attr.background_pixel := 0;
  attr.border_pixel := 0;
  attr.colormap := XCreateColormap(win^.dpy, root, visinfo^.visual, AllocNone);
  attr.event_mask := StructureNotifyMask or ExposureMask or KeyPressMask or
                     KeyReleaseMask or ButtonPress or ButtonReleaseMask or Button1MotionMask or
                     PointerMotionMask;
  //XXX this is a bad way to get a borderless window!
  //   attr.override_redirect := fullscreen;
  attr.override_redirect := 0;
  mask := CWBackPixel or CWBorderPixel or CWColormap or CWEventMask or
          CWOverrideRedirect;

  win^.Xwin := XCreateWindow(win^.dpy, root, posx, posy,
               sizex - posx, sizey - posy, 0, visinfo^.depth, InputOutput, visinfo^.visual, mask, @attr);

  win^.ctx := glXCreateContext(win^.dpy, visinfo, nil, True);

  //register delete!
  win^.wmDeleteMessage := XInternAtom(win^.dpy, 'WM_DELETE_WINDOW', False);
  XSetWMProtocols(win^.dpy, win^.Xwin, @win^.wmDeleteMessage, 1);
  // Register

  //set hints and properties
  sizehints.x := posx;
  sizehints.y := posy;
  sizehints.width := sizex - posx;
  sizehints.height := sizey - posy;
  sizehints.flags := USSize or USPosition;
  XSetNormalHints(win^.dpy, win^.Xwin, @sizehints);
  XSetStandardProperties(win^.dpy, win^.Xwin, title, title,
                         None, nil, 0, @sizehints);

  if win^.ctx = nil then
    begin
      glptError(GLPT_ERROR_PLATFORM, 'glXCreateContext failed');
      exit(False);
    end;

  XFree(visinfo);

  XMapWindow(win^.dpy, win^.Xwin);
  glXMakeCurrent(win^.dpy, win^.Xwin, win^.ctx);

  exit(True);
end;

procedure X11_DestroyWindow(var win: pGLPTwindow);
begin
  //destroy window and context
  if win^.fscreen then
    begin
    end;

  glXMakeCurrent(win^.dpy, None, nil);
  glXDestroyContext(win^.dpy, win^.ctx);
  XDestroyWindow(win^.dpy, win^.Xwin);
  XCloseDisplay(win^.dpy);
end;

procedure X11_SwapBuffers(win: pGLPTwindow);
begin
  glXSwapBuffers(win^.dpy, win^.Xwin);
end;

procedure X11_GetFrameBufferSize(win: pGLPTwindow; out width, height: integer);
var
  attribs: TXWindowAttributes;
begin
  XGetWindowAttributes(win^.dpy, win^.Xwin, @attribs);

  width := attribs.width;
  height := attribs.height;
end;

procedure X11_PollEvents;
var
  buffer: string[10];
  event: TXEvent;
  win: pGLPTwindow;
  params: GLPT_MessageParams;
  r: integer;
  done: integer = 0;
begin
  win := pGLPTwindow(windowlist.first);

  while win <> nil do
  begin
    XAllowEvents(win^.dpy, AsyncBoth, CurrentTime);

    if XPending(win^.dpy) > 0 then
    begin
      XNextEvent(win^.dpy, @event);

      case event._type of 
        ClientMessage:
                       begin
                         if event.xclient.data.l[0] = win^.wmDeleteMessage then // quit!
                           done := 1;
                       end;
        Expose:
                begin
                  // Update event! Should do draw here.
                end;
        ConfigureNotify:
                         begin
                           params.rect.width := event.xconfigure.width;
                           params.rect.height := event.xconfigure.height;

                           glptPostMessage(win, GLPT_MESSAGE_RESIZE, params);
                         end;
        KeyPress,
        KeyRelease:
                    begin
                      r := XLookupString(@event.xkey, @buffer, sizeof(buffer), nil, nil);

                      params.keyboard.keychar := Ord(buffer[0]);
                      //params.keyboard.shiftstate := X11_GetKeyboardShiftState;

                      if event._type = KeyPress then
                        glptPostMessage(win, GLPT_MESSAGE_KEYPRESS, params);
                      if event._type = KeyRelease then
                        glptPostMessage(win, GLPT_MESSAGE_KEYRELEASE, params);
                    end;
        ButtonPress,
        ButtonRelease:
                       begin
                         params.mouse.x := event.xbutton.x;
                         params.mouse.y := event.xbutton.y;
                         //params.mouse.shiftstate := X11_GetKeyboardShiftState;
                         params.mouse.timestamp := X11_GetTime;

                         case event.xbutton.button of 
                           Button1: params.mouse.Buttons := GLPT_MOUSE_BUTTON_LEFT;
                           Button2: params.mouse.Buttons := GLPT_MOUSE_BUTTON_MIDDLE;
                           Button3: params.mouse.Buttons := GLPT_MOUSE_BUTTON_RIGHT;
                         end;

                         case event._type of 
                           ButtonPress: glptPostMessage(win, GLPT_MESSAGE_MOUSEDOWN, params);
                           ButtonRelease: glptPostMessage(win, GLPT_MESSAGE_MOUSEUP, params);
                         end;
                       end;
        MotionNotify:
                      begin
                      end;
      end;
    end;

    win := win^.next;
  end;
end;

//function X11_WindowFromHWND(hWnd: Windows.HWND): pGLPTwindow;
//var
//  wi: pWindow;
//begin
//  wi := windowlist.first;

//  while wi <> nil do
//    begin
//      if wi^.h_Wnd = hWnd then
//        exit(wi);

//      wi := wi^.next;
//    end;
//end;

//function X11_GetKeyboardShiftState: TShiftState;
//var
//  State: array[byte] of byte;
//begin
//  Windows.GetKeyboardState(State);

//  Result := [];
//  if (State[VK_SHIFT] and 128) <> 0 then
//    Include(Result, ssShift);
//  if (State[VK_MENU] and 128) <> 0 then
//    Include(Result, ssAlt);
//  if (State[VK_CONTROL] and 128) <> 0 then
//    Include(Result, ssCtrl);
//  if (State[VK_LBUTTON] and 128) <> 0 then
//    Include(Result, ssLeft);
//  if (State[VK_RBUTTON] and 128) <> 0 then
//    Include(Result, ssRight);
//  if (State[VK_MBUTTON] and 128) <> 0 then
//    Include(Result, ssMiddle);
//  if (State[VK_CAPITAL] and 1) <> 0 then
//    Include(Result, ssCaps);
//  if (State[VK_NUMLOCK] and 1) <> 0 then
//    Include(Result, ssNum);
//  if (State[VK_SCROLL] and 1) <> 0 then
//    Include(Result, ssScroll);
//end;

function X11_Init: boolean;
begin
  exit(True);
end;

function X11_Terminate: boolean;
begin
  exit(True);
end;

function X11_MakeCurrent(win: pGLPTwindow): boolean;
begin
  {$note implement this}
  exit(True);
end;

procedure X11_SetCursor(cursor: byte);
begin
  {$note implement this}
end;

procedure X11_GetDisplayCoords(var dr: GLPTRect);
var
  dpy: PDisplay;
  screen: PScreen;
begin
  dpy := XOpenDisplay(nil);
  screen := ScreenOfDisplay(dpy, 0);

  dr.left := 0;
  dr.right := screen^.width;
  dr.top := 0;
  dr.bottom := screen^.height;

  XCloseDisplay(dpy);
end;
