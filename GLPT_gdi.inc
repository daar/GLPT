var 
  windows_scancode_table: array[0..127] of GLPT_Scancode = (
    {  0           1             2             3             4           5             6             7 }
    {  8           9             A             B             C           D             E             F }
    GLPT_SCANCODE_UNKNOWN,   GLPT_SCANCODE_ESCAPE,    GLPT_SCANCODE_1,       GLPT_SCANCODE_2,       GLPT_SCANCODE_3,     GLPT_SCANCODE_4,       GLPT_SCANCODE_5,       GLPT_SCANCODE_6,     (* 0 *)
    GLPT_SCANCODE_7,       GLPT_SCANCODE_8,       GLPT_SCANCODE_9,       GLPT_SCANCODE_0,       GLPT_SCANCODE_MINUS,   GLPT_SCANCODE_EQUALS,    GLPT_SCANCODE_BACKSPACE,   GLPT_SCANCODE_TAB,   (* 0 *)

    GLPT_SCANCODE_Q,       GLPT_SCANCODE_W,       GLPT_SCANCODE_E,       GLPT_SCANCODE_R,       GLPT_SCANCODE_T,     GLPT_SCANCODE_Y,       GLPT_SCANCODE_U,       GLPT_SCANCODE_I,     (* 1 *)
    GLPT_SCANCODE_O,       GLPT_SCANCODE_P,       GLPT_SCANCODE_LEFTBRACKET, GLPT_SCANCODE_RIGHTBRACKET,  GLPT_SCANCODE_RETURN,  GLPT_SCANCODE_LCTRL,     GLPT_SCANCODE_A,       GLPT_SCANCODE_S,     (* 1 *)

    GLPT_SCANCODE_D,       GLPT_SCANCODE_F,       GLPT_SCANCODE_G,       GLPT_SCANCODE_H,       GLPT_SCANCODE_J,     GLPT_SCANCODE_K,       GLPT_SCANCODE_L,       GLPT_SCANCODE_SEMICOLON, (* 2 *)
    GLPT_SCANCODE_APOSTROPHE,  GLPT_SCANCODE_GRAVE,     GLPT_SCANCODE_LSHIFT,    GLPT_SCANCODE_BACKSLASH,   GLPT_SCANCODE_Z,     GLPT_SCANCODE_X,       GLPT_SCANCODE_C,       GLPT_SCANCODE_V,     (* 2 *)

    GLPT_SCANCODE_B,       GLPT_SCANCODE_N,       GLPT_SCANCODE_M,       GLPT_SCANCODE_COMMA,     GLPT_SCANCODE_PERIOD,  GLPT_SCANCODE_SLASH,     GLPT_SCANCODE_RSHIFT,    GLPT_SCANCODE_PRINTSCREEN,(* 3 *)
    GLPT_SCANCODE_LALT,      GLPT_SCANCODE_SPACE,     GLPT_SCANCODE_CAPSLOCK,    GLPT_SCANCODE_F1,      GLPT_SCANCODE_F2,    GLPT_SCANCODE_F3,      GLPT_SCANCODE_F4,      GLPT_SCANCODE_F5,    (* 3 *)

    GLPT_SCANCODE_F6,      GLPT_SCANCODE_F7,      GLPT_SCANCODE_F8,      GLPT_SCANCODE_F9,      GLPT_SCANCODE_F10,   GLPT_SCANCODE_NUMLOCKCLEAR,  GLPT_SCANCODE_SCROLLLOCK,  GLPT_SCANCODE_HOME,    (* 4 *)
    GLPT_SCANCODE_UP,      GLPT_SCANCODE_PAGEUP,    GLPT_SCANCODE_KP_MINUS,    GLPT_SCANCODE_LEFT,      GLPT_SCANCODE_KP_5,    GLPT_SCANCODE_RIGHT,     GLPT_SCANCODE_KP_PLUS,   GLPT_SCANCODE_END,   (* 4 *)

    GLPT_SCANCODE_DOWN,      GLPT_SCANCODE_PAGEDOWN,    GLPT_SCANCODE_INSERT,    GLPT_SCANCODE_DELETE,    GLPT_SCANCODE_UNKNOWN, GLPT_SCANCODE_UNKNOWN,   GLPT_SCANCODE_NONUSBACKSLASH,GLPT_SCANCODE_F11,   (* 5 *)
    GLPT_SCANCODE_F12,     GLPT_SCANCODE_PAUSE,     GLPT_SCANCODE_UNKNOWN,   GLPT_SCANCODE_LGUI,      GLPT_SCANCODE_RGUI,    GLPT_SCANCODE_APPLICATION, GLPT_SCANCODE_UNKNOWN,   GLPT_SCANCODE_UNKNOWN, (* 5 *)

    GLPT_SCANCODE_UNKNOWN,   GLPT_SCANCODE_UNKNOWN,   GLPT_SCANCODE_UNKNOWN,   GLPT_SCANCODE_UNKNOWN,   GLPT_SCANCODE_F13,   GLPT_SCANCODE_F14,     GLPT_SCANCODE_F15,     GLPT_SCANCODE_F16,   (* 6 *)
    GLPT_SCANCODE_F17,     GLPT_SCANCODE_F18,     GLPT_SCANCODE_F19,     GLPT_SCANCODE_UNKNOWN,   GLPT_SCANCODE_UNKNOWN, GLPT_SCANCODE_UNKNOWN,   GLPT_SCANCODE_UNKNOWN,   GLPT_SCANCODE_UNKNOWN, (* 6 *)
    
    GLPT_SCANCODE_INTERNATIONAL2,    GLPT_SCANCODE_UNKNOWN,   GLPT_SCANCODE_UNKNOWN,   GLPT_SCANCODE_INTERNATIONAL1,    GLPT_SCANCODE_UNKNOWN, GLPT_SCANCODE_UNKNOWN,   GLPT_SCANCODE_UNKNOWN,   GLPT_SCANCODE_UNKNOWN, (* 7 *)
    GLPT_SCANCODE_UNKNOWN,   GLPT_SCANCODE_INTERNATIONAL4,    GLPT_SCANCODE_UNKNOWN,   GLPT_SCANCODE_INTERNATIONAL5,    GLPT_SCANCODE_UNKNOWN, GLPT_SCANCODE_INTERNATIONAL3,    GLPT_SCANCODE_UNKNOWN,   GLPT_SCANCODE_UNKNOWN  (* 7 *)
  );

// SDL_windowskeyboard.c 

(*

void
WIN_UpdateKeymap()
{
    int i;
    SDL_Scancode scancode;
    SDL_Keycode keymap[SDL_NUM_SCANCODES];

    SDL_GetDefaultKeymap(keymap);

    for (i = 0; i < SDL_arraysize(windows_scancode_table); i++) {
        int vk;
        (* Make sure this scancode is a valid character scancode *)
        scancode = windows_scancode_table[i];
        if (scancode == SDL_SCANCODE_UNKNOWN ) {
            continue;
        }

        (* If this key is one of the non-mappable keys, ignore it *)
        (* Not mapping numbers fixes the French layout, giving numeric keycodes for the number keys, which is the expected behavior *)
        if ((keymap[scancode] & SDLK_SCANCODE_MASK) ||
            (*  scancode == SDL_SCANCODE_GRAVE || *) (* Uncomment this line to re-enable the behavior of not mapping the "`"(grave) key to the users actual keyboard layout *)
            (scancode >= SDL_SCANCODE_1 && scancode <= SDL_SCANCODE_0) ) {
            continue;
        }

        vk =  MapVirtualKey(i, MAPVK_VSC_TO_VK);
        if ( vk ) {
            int ch = (MapVirtualKey( vk, MAPVK_VK_TO_CHAR ) & 0x7FFF);
            if ( ch ) {
                if ( ch >= 'A' && ch <= 'Z' ) {
                    keymap[scancode] =  SDLK_a + ( ch - 'A' );
                } else {
                    keymap[scancode] = ch;
                }
            }
        }
    }

    SDL_SetKeymap(0, keymap, SDL_NUM_SCANCODES);
}

*)

var
  wc: Windows.TWNDCLASS;
  h_Instance: HINST;
  g_Cursor: LPCSTR = IDC_ARROW;

function gdi_GetTime: double;
begin
  exit(GetTickCount / 1000);
end;

function gdi_Terminate: boolean;
begin
  if (not Windows.UnregisterClass('GLPT class', h_Instance)) then
    begin
      glptError(GLPT_ERROR_PLATFORM, 'could not unregister class');
      exit(False);
    end;

  exit(True);
end;

procedure gdi_DestroyWindow(var win: pGLPTwindow);
begin
  //destroy window and context
  if win^.fscreen then
    begin
      ChangeDisplaySettings(devmode(nil^),0);
      showcursor(true);
    end;

  if win^.h_RC <> 0 then
    begin
      if (not wglMakeCurrent(win^.h_DC,0)) then
        glptError(GLPT_ERROR_PLATFORM, 'release of DC and RC failed');

      if (not wglDeleteContext(win^.h_RC)) then
        begin
          glptError(GLPT_ERROR_PLATFORM, 'release of Rendering Context failed');
          win^.h_RC := 0;
        end;
    end;

  if (win^.h_DC = 1) and (releaseDC(win^.h_Wnd, win^.h_DC) <> 0) then
    begin
      glptError(GLPT_ERROR_PLATFORM, 'release of Device Context failed');
      win^.h_DC := 0;
    end;

  if (win^.h_Wnd <> 0) and (not destroywindow(win^.h_Wnd)) then
    begin
      glptError(GLPT_ERROR_PLATFORM, 'could not release hWnd');
      win^.h_Wnd := 0;
    end;
end;

function gdi_CreateWindow(win: pGLPTwindow; posx, posy, sizex, sizey: integer; title: PChar): boolean;
const
  bits = 16;
var
  Pixelformat: longint;
  dwExStyle: dword;
  dwStyle: dword;
  pfd: pixelformatdescriptor;
  dmScreenSettings: Devmode;
  WindowRect: TRect;
begin
  WindowRect.Left := posx;
  WindowRect.Top := posy;
  WindowRect.Right := sizex;
  WindowRect.Bottom := sizey;

  if win^.fscreen then
    begin
      ZeroMemory(@dmScreenSettings, sizeof(dmScreenSettings));
      with dmScreensettings do
        begin
          dmSize := sizeof(dmScreenSettings);
          dmPelsWidth := sizex;
          dmPelsHeight := sizey;
          dmBitsPerPel := bits;
          dmFields := DM_BITSPERPEL or DM_PELSWIDTH or DM_PELSHEIGHT;
        end;

      if (ChangeDisplaySettings(dmScreenSettings, CDS_FULLSCREEN)) <> DISP_CHANGE_SUCCESSFUL then
        begin
          glptError(GLPT_ERROR_PLATFORM, 'fullscreen mode is not supported, switching to windowed mode instead');
          win^.fscreen := False;
        end;
    end;

  if win^.fscreen then
    begin
      dwExStyle := WS_EX_APPWINDOW;
      dwStyle := WS_POPUP or WS_CLIPSIBLINGS or WS_CLIPCHILDREN;
      Showcursor(False);
    end
  else
    begin
      dwExStyle := WS_EX_APPWINDOW or WS_EX_WINDOWEDGE;
      dwStyle := WS_OVERLAPPEDWINDOW or WS_CLIPSIBLINGS or WS_CLIPCHILDREN;
    end;

  AdjustWindowRectEx(WindowRect, dwStyle, False, dwExStyle);

  win^.h_Wnd := CreateWindowEx(dwExStyle,
                'GLPT class',
                Title,
                dwStyle,
                0, 0,
                WindowRect.Right - WindowRect.Left,
                WindowRect.Bottom - WindowRect.Top,
                0,
                0,
                hinstance,
                nil);
  if win^.h_Wnd = 0 then
    begin
      gdi_DestroyWindow(win);
      glptError(GLPT_ERROR_PLATFORM, 'window creation error');
      exit(False);
    end;

  with pfd do
    begin
      nSize := SizeOf(PIXELFORMATDESCRIPTOR);
      nVersion := 1;
      dwFlags := PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER;
      iPixelType := PFD_TYPE_RGBA;
      cColorBits := bits;
      cRedBits := 0;
      cRedShift := 0;
      cGreenBits := 0;
      cBlueBits := 0;
      cBlueShift := 0;
      cAlphaBits := 0;
      cAlphaShift := 0;
      cAccumBits := 0;
      cAccumRedBits := 0;
      cAccumGreenBits := 0;
      cAccumBlueBits := 0;
      cAccumAlphaBits := 0;
      cDepthBits := 16;
      cStencilBits := 0;
      cAuxBuffers := 0;
      iLayerType := PFD_MAIN_PLANE;
      bReserved := 0;
      dwLayerMask := 0;
      dwVisibleMask := 0;
      dwDamageMask := 0;
    end;

  win^.h_DC := GetDC(win^.h_Wnd);
  if win^.h_DC = 0 then
    begin
      gdi_DestroyWindow(win);
      glptError(GLPT_ERROR_PLATFORM, 'can''t create a GL device context');
      exit(False);
    end;

  PixelFormat := ChoosePixelFormat(win^.h_DC, @pfd);
  if PixelFormat = 0 then
    begin
      gdi_DestroyWindow(win);
      glptError(GLPT_ERROR_PLATFORM, 'can''t find a suitable PixelFormat');
      exit(False);
    end;

  if not SetPixelFormat(win^.h_DC, PixelFormat, @pfd) then
    begin
      gdi_DestroyWindow(win);
      glptError(GLPT_ERROR_PLATFORM, 'can''t set PixelFormat');
      exit(False);
    end;

  win^.h_RC := wglCreateContext(win^.h_DC);
  if win^.h_RC = 0 then
    begin
      gdi_DestroyWindow(win);
      glptError(GLPT_ERROR_PLATFORM, 'can''t create a GL rendering context');
      exit(False);
    end;

  if not wglMakeCurrent(win^.h_DC, win^.h_RC) then
    begin
      gdi_DestroyWindow(win);
      glptError(GLPT_ERROR_PLATFORM, 'can''t activate the GL rendering context');
      exit(False);
    end;

  ShowWindow(win^.h_Wnd, SW_SHOW);
  SetForegroundWindow(win^.h_Wnd);
  SetFocus(win^.h_Wnd);

  exit(True);
end;

function gdi_MakeCurrent(win: pGLPTwindow): boolean;
begin
  exit(wglMakeCurrent(win^.h_DC, win^.h_RC));
end;

procedure gdi_SwapBuffers(win: pGLPTwindow);
begin
  SwapBuffers(win^.h_DC);
end;

procedure gdi_GetFrameBufferSize(win: pGLPTwindow; out width, height: integer);
var
  area: RECT;
begin
  GetClientRect(win^.h_Wnd, @area);

  width := area.right;
  height := area.bottom;
end;

procedure gdi_PollEvents;
var
  msg: TMSG;
begin
  // check for messages
  while PeekMessage(msg, 0, 0, 0, PM_REMOVE) do
    begin
      TranslateMessage(@msg);
      DispatchMessage(@msg);
    end;
end;

procedure gdi_GetDisplayCoords(var dr: GLPTRect);
begin
  if not SystemParametersInfo(SPI_GETWORKAREA, 0, @dr, 0) then
  begin
    dr.left := 0;
    dr.right := 0;
    dr.top := 0;
    dr.bottom := 0;
  end;
end;

function gdi_WindowFromHWND(hWnd: Windows.HWND): pGLPTwindow;
var
  wi: pGLPTwindow;
begin
  wi := pGLPTwindow(windowlist.first);

  while wi <> nil do
    begin
      if wi^.h_Wnd = hWnd then
        exit(wi);

      wi := wi^.next;
    end;

  exit(nil);
end;

function gdi_GetKeyboardShiftState: TShiftState;
var
  State: array[byte] of byte;
begin
  Windows.GetKeyboardState(State);

  Result := [];
  if (State[VK_SHIFT] and 128) <> 0 then
    Include(Result, ssShift);
  if (State[VK_MENU] and 128) <> 0 then
    Include(Result, ssAlt);
  if (State[VK_CONTROL] and 128) <> 0 then
    Include(Result, ssCtrl);
  if (State[VK_LBUTTON] and 128) <> 0 then
    Include(Result, ssLeft);
  if (State[VK_RBUTTON] and 128) <> 0 then
    Include(Result, ssRight);
  if (State[VK_MBUTTON] and 128) <> 0 then
    Include(Result, ssMiddle);
  if (State[VK_CAPITAL] and 1) <> 0 then
    Include(Result, ssCaps);
  if (State[VK_NUMLOCK] and 1) <> 0 then
    Include(Result, ssNum);
  if (State[VK_SCROLL] and 1) <> 0 then
    Include(Result, ssScroll);
end;

function WndProc(hWnd: Windows.HWND; message: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  x, y: integer;
  win: pGLPTwindow;
  params: GLPT_MessageParams;
begin
  x := LOWORD(lParam);
  y := HIWORD(lParam);

  win := gdi_WindowFromHWND(hWnd);

  case message of

    WM_LBUTTONUP,
    WM_LBUTTONDOWN,
    WM_MBUTTONUP,
    WM_MBUTTONDOWN,
    WM_RBUTTONUP,
    WM_RBUTTONDOWN:
                    begin
                      params.mouse.x := x;
                      params.mouse.y := y;
                      params.mouse.shiftstate := gdi_GetKeyboardShiftState;
                      params.mouse.timestamp := gdi_GetTime;

                      case message of
                        WM_LBUTTONUP,
                        WM_LBUTTONDOWN:
                                        params.mouse.buttons := GLPT_MOUSE_BUTTON_LEFT;
                        WM_MBUTTONUP,
                        WM_MBUTTONDOWN:
                                        params.mouse.buttons := GLPT_MOUSE_BUTTON_MIDDLE;
                        WM_RBUTTONUP,
                        WM_RBUTTONDOWN:
                                        params.mouse.buttons := GLPT_MOUSE_BUTTON_RIGHT;
                      end;

                      case message of
                        WM_LBUTTONUP,
                        WM_MBUTTONUP,
                        WM_RBUTTONUP:
                                      glptPostMessage(win, GLPT_MESSAGE_MOUSEUP, params);
                        WM_LBUTTONDOWN,
                        WM_MBUTTONDOWN,
                        WM_RBUTTONDOWN:
                                        glptPostMessage(win, GLPT_MESSAGE_MOUSEDOWN, params);
                      end;
                    end;

    WM_MOUSEMOVE:
                  begin
                    params.mouse.x := x;
                    params.mouse.y := y;
                    params.mouse.shiftstate := gdi_GetKeyboardShiftState;
                    params.mouse.timestamp := gdi_GetTime;

                    glptPostMessage(win, GLPT_MESSAGE_MOUSEMOVE, params);
                  end;

    WM_KEYDOWN,
    WM_KEYUP:
              begin
                params.keyboard.shiftstate := gdi_GetKeyboardShiftState;

                if (ssShift in params.keyboard.shiftstate) or (ssCaps in params.keyboard.shiftstate) then
                  params.keyboard.keycode := wParam
                else
                  params.keyboard.keycode := Ord(LowerCase(Chr(wParam)));

                if message = WM_KEYDOWN then
                  glptPostMessage(win, GLPT_MESSAGE_KEYPRESS, params);
                if message = WM_KEYUP then
                  glptPostMessage(win, GLPT_MESSAGE_KEYRELEASE, params);
              end;

    WM_SIZE:
             begin
               params.rect.top := 0;
               params.rect.left := 0;
               params.rect.width  := smallint(lParam and $FFFF);
               params.rect.height := smallint((lParam and $FFFF0000) shr 16);

               glptPostMessage(win, GLPT_MESSAGE_RESIZE, params);
             end;
    //WM_PAINT:
    //          glptPostMessage(win, GLPT_MESSAGE_PAINT, params);
    WM_SETCURSOR:
                  SetCursor(LoadCursor(0, g_Cursor));
    else
      begin
        //writeln(message);

        if message = WM_PAINT then
          glptPostMessage(win, GLPT_MESSAGE_PAINT, params);

        exit(DefWindowProc(hWnd, message, wParam, lParam));
      end;
  end;

  exit(0);
end;

function gdi_Init: boolean;
begin
  h_Instance := GetModuleHandle(nil);

  with wc do
    begin
      style := CS_HREDRAW or CS_VREDRAW or CS_OWNDC;
      lpfnWndProc := @WndProc;
      cbClsExtra := 0;
      cbWndExtra := 0;
      hInstance := h_Instance;
      hIcon := LoadIcon(0, IDI_WINLOGO);
      hCursor := LoadCursor(0, g_Cursor);
      hbrBackground := 0;
      lpszMenuName := nil;
      lpszClassName := 'GLPT class';
    end;

  if Windows.RegisterClass(wc) = 0 then
    begin
      case GetLastError of
        ERROR_CLASS_ALREADY_EXISTS:
          glptError(GLPT_ERROR_PLATFORM, 'class already exists');
        ERROR_CLASS_DOES_NOT_EXIST:
          glptError(GLPT_ERROR_PLATFORM, 'class does not exists');
        ERROR_CLASS_HAS_WINDOWS:
          glptError(GLPT_ERROR_PLATFORM, 'class has windows');
      else
        glptError(GLPT_ERROR_PLATFORM, 'unknown class error');
      end;

      exit(False);
    end;

  exit(True);
end;

procedure gdi_SetCursor(cursor: byte);
begin
  case cursor of
     GLPT_ARROW_CURSOR:
       g_Cursor := IDC_ARROW;
     GLPT_IBEAM_CURSOR:
       g_Cursor := IDC_IBEAM;
     GLPT_CROSSHAIR_CURSOR:
       g_Cursor := IDC_CROSS;
     GLPT_HAND_CURSOR:
       g_Cursor := IDC_HAND;
     GLPT_HRESIZE_CURSOR:
       g_Cursor := IDC_SIZEWE;
     GLPT_VRESIZE_CURSOR:
       g_Cursor := IDC_SIZENS;
  else
    g_Cursor := IDC_ARROW;
  end;

  SetCursor(LoadCursor(h_Instance, g_Cursor));
end;

var
  WGLInitialized: boolean = false;
  // non-ARB wgl extensions
  wglSwapIntervalEXT: function(interval : Integer) : BOOL; stdcall;

procedure Load_WGL_EXT_swap_control;
begin
  if WGLInitialized then exit;
    WGLInitialized := true;

  Pointer(wglSwapIntervalEXT) := wglGetProcAddress('wglSwapIntervalEXT');
end;

procedure gdi_SetVSync(sync: boolean);
begin
  Load_WGL_EXT_swap_control;

  if  wglSwapIntervalEXT <> nil then
    if sync then
      wglSwapIntervalEXT(1)
    else
      wglSwapIntervalEXT(0);
end;
