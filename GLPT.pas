{ GLPT :: OpenGL Pascal Toolkit

  Copyright (c) 2018 Darius Blaszyk

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
}
unit GLPT;

{$mode objfpc}

interface

{$IFNDEF DARWIN}
uses
{$ENDIF}
{$IFDEF MSWINDOWS}
  Windows;
{$ENDIF}
{$IFDEF LINUX}
  Linux, UnixType, X, Xlib, xutil, GLX;
{$ENDIF}
{$IFDEF DARWIN}

{$ENDIF}

const
  //mouse buttons.
  GLPT_MOUSE_BUTTON_LEFT = $00000001;
  GLPT_MOUSE_BUTTON_MIDDLE = $00000002;
  GLPT_MOUSE_BUTTON_RIGHT = $00000003;

  //mouse button state.
  GLPT_RELEASE = $00000000;
  GLPT_PRESS = $00000001;
  GLPT_REPEAT = $00000002;

  //standard cursor shapes
  GLPT_ARROW_CURSOR = $00000001;
  GLPT_IBEAM_CURSOR = $00000002;
  GLPT_CROSSHAIR_CURSOR = $00000003;
  GLPT_HAND_CURSOR = $00000004;
  GLPT_HRESIZE_CURSOR = $00000005;
  GLPT_VRESIZE_CURSOR = $00000006;

  // messages
  GLPT_MESSAGE_PAINT = 1;
  GLPT_MESSAGE_ACTIVATE = 2;
  GLPT_MESSAGE_DEACTIVATE = 3;
  GLPT_MESSAGE_KEYPRESS = 4;
  GLPT_MESSAGE_KEYRELEASE = 5;
  GLPT_MESSAGE_KEYCHAR = 6;
  GLPT_MESSAGE_MOUSEDOWN = 7;
  GLPT_MESSAGE_MOUSEUP = 8;
  GLPT_MESSAGE_MOUSEMOVE = 9;
  GLPT_MESSAGE_DOUBLECLICK = 10;
  GLPT_MESSAGE_MOUSEENTER = 11;
  GLPT_MESSAGE_MOUSEEXIT = 12;
  GLPT_MESSAGE_CLOSE = 13;
  GLPT_MESSAGE_SCROLL = 14;
  GLPT_MESSAGE_RESIZE = 15;
  GLPT_MESSAGE_MOVE = 16;
  GLPT_MESSAGE_POPUPCLOSE = 17;
  GLPT_MESSAGE_HINTTIMER = 18;
  GLPT_MESSAGE_FREEME = 19;
  GLPT_MESSAGE_DROPENTER = 20;
  GLPT_MESSAGE_DROPEXIT = 21;
  GLPT_MESSAGE_HSCROLL = 22;
  GLPT_MESSAGE_ABOUT = 23;
  GLPT_MESSAGE_USER = 50000;
  GLPT_MESSAGE_KILLME = MaxInt;

  { Key codes  }
  GLPT_KEY_LBUTTON = 1;
  GLPT_KEY_RBUTTON = 2;
  GLPT_KEY_CANCEL = 3;
  GLPT_KEY_MBUTTON = 4;
  GLPT_KEY_XBUTTON1 = 5;
  GLPT_KEY_XBUTTON2 = 6;
  GLPT_KEY_BACK = 8;
  GLPT_KEY_TAB = 9;
  GLPT_KEY_CLEAR = 12;
  GLPT_KEY_RETURN = 13;
  GLPT_KEY_SHIFT = 16;
  GLPT_KEY_CONTROL = 17;
  GLPT_KEY_MENU = 18;
  GLPT_KEY_PAUSE = 19;
  GLPT_KEY_CAPITAL = 20;
  GLPT_KEY_KANA = 21;
  GLPT_KEY_HANGEUL = 21;
  GLPT_KEY_HANGUL = 21;
  GLPT_KEY_JUNJA = 23;
  GLPT_KEY_FINAL = 24;
  GLPT_KEY_HANJA = 25;
  GLPT_KEY_KANJI = 25;
  GLPT_KEY_ESCAPE = 27;
  GLPT_KEY_CONVERT = 28;
  GLPT_KEY_NONCONVERT = 29;
  GLPT_KEY_ACCEPT = 30;
  GLPT_KEY_MODECHANGE = 31;
  GLPT_KEY_SPACE = 32;
  GLPT_KEY_PRIOR = 33;
  GLPT_KEY_NEXT = 34;
  GLPT_KEY_END = 35;
  GLPT_KEY_HOME = 36;
  GLPT_KEY_LEFT = 37;
  GLPT_KEY_UP = 38;
  GLPT_KEY_RIGHT = 39;
  GLPT_KEY_DOWN = 40;
  GLPT_KEY_SELECT = 41;
  GLPT_KEY_PRINT = 42;
  GLPT_KEY_EXECUTE = 43;
  GLPT_KEY_SNAPSHOT = 44;
  GLPT_KEY_INSERT = 45;
  GLPT_KEY_DELETE = 46;
  GLPT_KEY_HELP = 47;
  GLPT_KEY_0 = 48;
  GLPT_KEY_1 = 49;
  GLPT_KEY_2 = 50;
  GLPT_KEY_3 = 51;
  GLPT_KEY_4 = 52;
  GLPT_KEY_5 = 53;
  GLPT_KEY_6 = 54;
  GLPT_KEY_7 = 55;
  GLPT_KEY_8 = 56;
  GLPT_KEY_9 = 57;
  GLPT_KEY_A = 65;
  GLPT_KEY_B = 66;
  GLPT_KEY_C = 67;
  GLPT_KEY_D = 68;
  GLPT_KEY_E = 69;
  GLPT_KEY_F = 70;
  GLPT_KEY_G = 71;
  GLPT_KEY_H = 72;
  GLPT_KEY_I = 73;
  GLPT_KEY_J = 74;
  GLPT_KEY_K = 75;
  GLPT_KEY_L = 76;
  GLPT_KEY_M = 77;
  GLPT_KEY_N = 78;
  GLPT_KEY_O = 79;
  GLPT_KEY_P = 80;
  GLPT_KEY_Q = 81;
  GLPT_KEY_R = 82;
  GLPT_KEY_S = 83;
  GLPT_KEY_T = 84;
  GLPT_KEY_U = 85;
  GLPT_KEY_V = 86;
  GLPT_KEY_W = 87;
  GLPT_KEY_X = 88;
  GLPT_KEY_Y = 89;
  GLPT_KEY_Z = 90;
  GLPT_KEY_LWIN = 91;
  GLPT_KEY_RWIN = 92;
  GLPT_KEY_APPS = 93;
  GLPT_KEY_SLEEP = 95;
  GLPT_KEY_NUMPAD0 = 96;
  GLPT_KEY_NUMPAD1 = 97;
  GLPT_KEY_NUMPAD2 = 98;
  GLPT_KEY_NUMPAD3 = 99;
  GLPT_KEY_NUMPAD4 = 100;
  GLPT_KEY_NUMPAD5 = 101;
  GLPT_KEY_NUMPAD6 = 102;
  GLPT_KEY_NUMPAD7 = 103;
  GLPT_KEY_NUMPAD8 = 104;
  GLPT_KEY_NUMPAD9 = 105;
  GLPT_KEY_MULTIPLY = 106;
  GLPT_KEY_ADD = 107;
  GLPT_KEY_SEPARATOR = 108;
  GLPT_KEY_SUBTRACT = 109;
  GLPT_KEY_DECIMAL = 110;
  GLPT_KEY_DIVIDE = 111;
  GLPT_KEY_F1 = 112;
  GLPT_KEY_F2 = 113;
  GLPT_KEY_F3 = 114;
  GLPT_KEY_F4 = 115;
  GLPT_KEY_F5 = 116;
  GLPT_KEY_F6 = 117;
  GLPT_KEY_F7 = 118;
  GLPT_KEY_F8 = 119;
  GLPT_KEY_F9 = 120;
  GLPT_KEY_F10 = 121;
  GLPT_KEY_F11 = 122;
  GLPT_KEY_F12 = 123;
  GLPT_KEY_F13 = 124;
  GLPT_KEY_F14 = 125;
  GLPT_KEY_F15 = 126;
  GLPT_KEY_F16 = 127;
  GLPT_KEY_F17 = 128;
  GLPT_KEY_F18 = 129;
  GLPT_KEY_F19 = 130;
  GLPT_KEY_F20 = 131;
  GLPT_KEY_F21 = 132;
  GLPT_KEY_F22 = 133;
  GLPT_KEY_F23 = 134;
  GLPT_KEY_F24 = 135;

  GLPT_KEY_NUMLOCK = 144;
  GLPT_KEY_SCROLL = 145;
  GLPT_KEY_OEM_NEC_EQUAL = 146;
  GLPT_KEY_OEM_FJ_JISHO = 146;
  GLPT_KEY_OEM_FJ_MASSHOU = 147;
  GLPT_KEY_OEM_FJ_TOUROKU = 148;
  GLPT_KEY_OEM_FJ_LOYA = 149;
  GLPT_KEY_OEM_FJ_ROYA = 150;
  GLPT_KEY_LSHIFT = 160;
  GLPT_KEY_LCONTROL = 162;
  GLPT_KEY_LMENU = 164;
  GLPT_KEY_RSHIFT = 161;
  GLPT_KEY_RCONTROL = 163;
  GLPT_KEY_RMENU = 165;
  GLPT_KEY_BROWSER_BACK = 166;
  GLPT_KEY_BROWSER_FORWARD = 167;
  GLPT_KEY_BROWSER_REFRESH = 168;
  GLPT_KEY_BROWSER_STOP = 169;
  GLPT_KEY_BROWSER_SEARCH = 170;
  GLPT_KEY_BROWSER_FAVORITES = 171;
  GLPT_KEY_BROWSER_HOME = 172;
  GLPT_KEY_VOLUME_MUTE = 173;
  GLPT_KEY_VOLUME_DOWN = 174;
  GLPT_KEY_VOLUME_UP = 175;
  GLPT_KEY_MEDIA_NEXT_TRACK = 176;
  GLPT_KEY_MEDIA_PREV_TRACK = 177;
  GLPT_KEY_MEDIA_STOP = 178;
  GLPT_KEY_MEDIA_PLAY_PAUSE = 179;
  GLPT_KEY_LAUNCH_MAIL = 180;
  GLPT_KEY_LAUNCH_MEDIA_SELECT = 181;
  GLPT_KEY_LAUNCH_APP1 = 182;
  GLPT_KEY_LAUNCH_APP2 = 183;
  GLPT_KEY_OEM_1 = 186;
  GLPT_KEY_OEM_PLUS = 187;
  GLPT_KEY_OEM_COMMA = 188;
  GLPT_KEY_OEM_MINUS = 189;
  GLPT_KEY_OEM_PERIOD = 190;
  GLPT_KEY_OEM_2 = 191;
  GLPT_KEY_OEM_3 = 192;
  GLPT_KEY_OEM_4 = 219;
  GLPT_KEY_OEM_5 = 220;
  GLPT_KEY_OEM_6 = 221;
  GLPT_KEY_OEM_7 = 222;
  GLPT_KEY_OEM_8 = 223;
  GLPT_KEY_OEM_AX = 225;
  GLPT_KEY_OEM_102 = 226;
  GLPT_KEY_ICO_HELP = 227;
  GLPT_KEY_ICO_00 = 228;

  GLPT_KEY_PROCESSKEY = 229;
  GLPT_KEY_ICO_CLEAR = 230;
  GLPT_KEY_PACKET = 231;
  GLPT_KEY_OEM_RESET = 233;
  GLPT_KEY_OEM_JUMP = 234;
  GLPT_KEY_OEM_PA1 = 235;
  GLPT_KEY_OEM_PA2 = 236;
  GLPT_KEY_OEM_PA3 = 237;
  GLPT_KEY_OEM_WSCTRL = 238;
  GLPT_KEY_OEM_CUSEL = 239;
  GLPT_KEY_OEM_ATTN = 240;
  GLPT_KEY_OEM_FINISH = 241;
  GLPT_KEY_OEM_COPY = 242;
  GLPT_KEY_OEM_AUTO = 243;
  GLPT_KEY_OEM_ENLW = 244;
  GLPT_KEY_OEM_BACKTAB = 245;
  GLPT_KEY_ATTN = 246;
  GLPT_KEY_CRSEL = 247;
  GLPT_KEY_EXSEL = 248;
  GLPT_KEY_EREOF = 249;
  GLPT_KEY_PLAY = 250;
  GLPT_KEY_ZOOM = 251;
  GLPT_KEY_NONAME = 252;
  GLPT_KEY_PA1 = 253;
  GLPT_KEY_OEM_CLEAR = 254;

type
  { Types used by standard events }
  TShiftStateEnum = (ssShift, ssAlt, ssCtrl,
    ssLeft, ssRight, ssMiddle, ssDouble,
    // Extra additions
    ssMeta, ssSuper, ssHyper, ssAltGr, ssCaps, ssNum,
    ssScroll, ssTriple, ssQuad, ssExtra1, ssExtra2);

{$packset 1}
  TShiftState = set of TShiftStateEnum;
{$packset default}

  pGLPT_MessageRec = ^GLPT_MessageRec;

  GLPT_EventCallback = procedure(msg: pGLPT_MessageRec);
  GLPT_ErrorCallback = procedure(const error: integer; const description: string);

  pGLPTwindow = ^GLPTwindow;

  GLPTWindow = record
    next: pGLPTwindow;        //< Next item in the window list
    prev: pGLPTwindow;        //< Previous item in the window list

    //Window settings and state

    fscreen: boolean;        //< Flag that indictes if the window is fullscreen or not
    shouldClose: boolean;    //< Flag indicating if the window should close

    //callback functions
    event_callback: GLPT_EventCallback;   //< event callback function for this window

{$IFDEF MSWINDOWS}
    h_Wnd: Windows.HWND;     //< GDI window handle
    h_DC: Windows.HDC;       //< GDI device context
    h_RC: HGLRC;             //< GDI rendering context
{$ENDIF}
{$IFDEF LINUX}
    dpy: pDisplay;            //< X11 display
    Xwin: TWindow;            //< X11 window
    ctx: GLXContext;          //< X11 GLX context
    wmDeleteMessage: TAtom;   //< X11 delete mesage callback
{$ENDIF}
{$IFDEF DARWIN}
{$ENDIF}
  end;

  GLPT_MsgParmMouse = record
    x: integer;                //< X position of the mouse
    y: integer;                //< Y position of the mouder
    buttons: word;             //< button state of the mouse
    shiftstate: TShiftState;   //< shift state of the keyboard
    delta: integer;            //< drag distance of mouse
    timestamp: TDateTime;      //< timestamp when event was generated
  end;

  GLPT_MsgParmKeyboard = record
    keychar: longint;          //< ASCII character fro keybord
    shiftstate: TShiftState;   //< shift state of the keyboard
  end;

  GLPT_MsgParmUser = record
    param1: pointer;   //< custom message 1
    param2: pointer;   //< custom message 2
    param3: pointer;   //< custom message 3
  end;

  GLPT_MsgParmRect = record
    top: integer;      //< top of rectangle being sent
    left: integer;     //< left of rectangle being sent
    width: integer;    //< width of rectangle being sent
    height: integer;   //< height of rectangle being sent
  end;

  pGLPT_MessageParams = ^GLPT_MessageParams;
  GLPT_MessageParams = record
    case integer of
      0: (mouse: GLPT_MsgParmMouse);         //< mouse event record
      1: (keyboard: GLPT_MsgParmKeyboard);   //< keyboard event record
      2: (rect: GLPT_MsgParmRect);           //< rectangel event record
      3: (user: GLPT_MsgParmUser);           //< used defined event record
  end;

  GLPT_MessageRec = record
    next: pGLPT_MessageRec;       //< next item in the list
    prev: pGLPT_MessageRec;       //< previous item in the list

    win: pGLPTwindow;             //< GLPT window that has sent message
    mcode: integer;               //< message code
    params: GLPT_MessageParams;   //< event data
  end;

  GLPTRect = record
    left: longint;     //< left position of rectangle
    top: longint;      //< top position of rectangle
    right: longint;    //< right position of rectangle
    bottom: longint;   //< bottom position of rectangle
  end;

{
   This function returns the last occurred error message. Please note that also the error callback function returns the
   same error message.
   @return the error string
}
function GLPT_GetLastError: string;

{
   This function returns the value of the time elapsed since GLPT was initialized.
   @return the time in seconds
}
function GLPT_GetTime: double;

{
   Initializes the GLPT library
   @return True if successfull otherwise False
}
function GLPT_Init: boolean;

{
   Terminates the GLPT library
   @return True if successfull otherwise False
}
function GLPT_Terminate: boolean;

{
   This function checks if the internal state variable
   @param win: the window that should be checked
   @return the value of the internal state variable
}
function GLPT_WindowShouldClose(win: pGLPTwindow): boolean;

{
   This procedure terminates the main event loop
   @param win: the window that should be closed
   @param Value: the value that should be given to the internal state variable
}
procedure GLPT_SetWindowShouldClose(win: pGLPTwindow; Value: boolean);

{
   Creates a window and its associated context.
   @param posx: the x position of the window
   @param posy: the y position of the window
   @param sizex: the width of the window
   @param sizey: the height of the window
   @param title: the title of the window
   @return a reference to the created window
}
function GLPT_CreateWindow(posx, posy, sizex, sizey: integer; title: PChar): pGLPTwindow;

{
   Destroys a window and its associated context.
   @param win: the reference to the window being destroyed
}
procedure GLPT_DestroyWindow(win: pGLPTwindow);

{
   Make the window active
   @param win: the window that should be made active
   @return True if successfull otherwise False
}
function GLPT_MakeCurrent(win: pGLPTwindow): boolean;

{
   Swaps the front and back buffers of the specified window.
   @param win: the reference to the window
}
procedure GLPT_SwapBuffers(win: pGLPTwindow);

{
   Retrieves the size of the framebuffer of the specified window.
   @param win: the reference to the window
   @param width: the width of the window
   @param height: the height of the window
}
procedure GLPT_GetFrameBufferSize(win: pGLPTwindow; out width, height: integer);

{
   Sets the error callback procedure
   @param pointer to error callback:
}
procedure GLPT_SetErrorCallback(errorCallback: GLPT_ErrorCallback);

{
   This procedure will poll for any pending events and put them in
   the eventlist. Next the eventlist is checked and if needed the event
   callback function is called.
}
procedure GLPT_PollEvents;

{
   Set the cursor to a predefined one
   @param cursor: the index of the cursor
}
procedure GLPT_SetCursor(cursor: byte);

{
   Retrieve the display coordinate size
   @param dr: the variable holding the display coordinates
}
procedure GLPT_GetDisplayCoords(var dr: GLPTRect);

implementation

const
  GLPT_ERROR_NONE = 1;
  GLPT_ERROR_PLATFORM = 2;
  GLPT_ERROR_UNKNOWN = 255;

type
  pLink = ^Link;

  Link = record
    next: pLink;
    prev: pLink;
  end;

  ListBase = record
    first: pLink;
    last: pLink;
  end;

  GLPT_error = record
    error: integer;
    msg: string;
  end;

var
  msglist: ListBase;

  errfunc: GLPT_ErrorCallback = nil;
  windowlist: ListBase;
  current_win: pGLPTwindow;

  inittime: double = 0;
  lasterr: GLPT_error;

//***  Error handling  *************************************************************************************************

procedure glptError(const error: integer; const msg: string);
var
  errmsg: string;
begin
  case error of
    GLPT_ERROR_NONE: errmsg := 'GLPT_ERROR_PLATFORM : ' + msg;
    GLPT_ERROR_PLATFORM: errmsg := 'GLPT_ERROR_PLATFORM : ' + msg;
    GLPT_ERROR_UNKNOWN: errmsg := 'GLPT_ERROR_UNKNOWN : ' + msg;
    else
      errmsg := 'GLPT_ERROR_UNKNOWN : ' + msg;
  end;

  //store the last error
  lasterr.error := error;
  lasterr.msg := msg;

  if assigned(errfunc) then
    errfunc(error, errmsg);
end;

//***  Memory handling  ************************************************************************************************

function calloc(size: ptruint): pointer;
var
  itm: pointer;
begin
  GetMem(itm, size);
  FillByte(itm^, size, 0);
  exit(itm);
end;

procedure add_tail(var list: listbase; item: pLink);
begin
  item^.next := nil;
  item^.prev := list.last;

  if list.last <> nil then
    list.last^.next := item;

  if list.first = nil then
    list.first := item;

  list.last := item;
end;

procedure remove_link(var list: listbase; item: pLink);
begin
  if item^.next <> nil then
    item^.next^.prev := item^.prev;

  if item^.prev <> nil then
    item^.prev^.next := item^.next;

  if list.last = item then
    list.last := item^.prev;

  if list.first = item then
    list.first := item^.next;
end;

//***  Messaging  ******************************************************************************************************

procedure glptPostMessage(win: pGLPTwindow; MsgCode: integer; Params: GLPT_MessageParams);
var
  msg: pGLPT_MessageRec;
begin
  msg := calloc(sizeof(GLPT_MessageRec));

  msg^.win := win;

  msg^.mcode := MsgCode;
  move(Params, msg^.params, sizeof(GLPT_MessageParams));

  add_tail(msglist, pLink(msg));
end;

function glptReadLastMessage: pGLPT_MessageRec;
begin
  exit(pGLPT_MessageRec(msglist.last));
end;

function glptReadFirstMessage: pGLPT_MessageRec;
begin
  exit(pGLPT_MessageRec(msglist.first));
end;

procedure glptDeleteMessage(msg: pGLPT_MessageRec);
begin
  remove_link(msglist, pLink(msg));
  freemem(msg);
end;

//***  Native functions  ***********************************************************************************************

{$IFDEF MSWINDOWS}
  {$i GLPT_gdi.inc}
{$ENDIF}
{$IFDEF LINUX}
  {$i GLPT_X11.inc}
{$ENDIF}
{$IFDEF DARWIN}
  {$i GLPT_Cocoa.inc}
{$ENDIF}

//***  API functions  **************************************************************************************************

function GLPT_GetLastError: string;
begin
  exit(lasterr.msg);
end;

function GLPT_GetTime: double;
begin
{$IFDEF MSWINDOWS}
  exit(gdi_GetTime - inittime);
{$ENDIF}
{$IFDEF LINUX}
  exit(X11_GetTime - inittime);
{$ENDIF}
{$IFDEF DARWIN}
  exit(Cocoa_GetTime - inittime);
{$ENDIF}
end;

function GLPT_Init: boolean;
begin
  //reset the last error
  lasterr.error := GLPT_ERROR_NONE;
  lasterr.msg := '';

  inittime := GLPT_GetTime;

{$IFDEF MSWINDOWS}
  exit(gdi_Init);
{$ENDIF}
{$IFDEF LINUX}
  exit(X11_Init);
{$ENDIF}
{$IFDEF DARWIN}
  exit(Cocoa_Init);
{$ENDIF}
end;

function GLPT_Terminate: boolean;
var
  win: pGLPTwindow;
begin
  //destroy any windows that are not destroyed yet
  while windowlist.first <> nil do
  begin
    win := pGLPTwindow(windowlist.last);

    GLPT_DestroyWindow(win);
  end;

{$IFDEF MSWINDOWS}
  exit(gdi_Terminate);
{$ENDIF}
{$IFDEF LINUX}
  exit(X11_Terminate);
{$ENDIF}
{$IFDEF DARWIN}
  exit(Cocoa_Terminate);
{$ENDIF}
end;

function GLPT_WindowShouldClose(win: pGLPTwindow): boolean;
begin
  exit(win^.shouldClose);
end;

procedure GLPT_SetWindowShouldClose(win: pGLPTwindow; value: boolean);
begin
  win^.shouldClose := value;
end;

function GLPT_CreateWindow(posx, posy, sizex, sizey: integer; title: PChar): pGLPTwindow;
var
  win: pGLPTwindow = nil;
  res: boolean = False;
begin
  win := calloc(sizeof(GLPTWindow));
  current_win := win;

{$IFDEF MSWINDOWS}
  res := gdi_CreateWindow(win, posx, posy, sizex, sizey, title);
{$ENDIF}
{$IFDEF LINUX}
  res := X11_CreateWindow(win, posx, posy, sizex, sizey, title);
{$ENDIF}
{$IFDEF DARWIN}
  res := Cocoa_CreateWindow(win, posx, posy, sizex, sizey, title);
{$ENDIF}

  if res then
  begin
    add_tail(windowlist, pLink(win));
    exit(win);
  end
  else
  begin
    freemem(win);
    exit(nil);
  end;
end;

procedure GLPT_DestroyWindow(win: pGLPTwindow);
begin
{$IFDEF MSWINDOWS}
  gdi_DestroyWindow(win);
{$ENDIF}
{$IFDEF LINUX}
  X11_DestroyWindow(win);
{$ENDIF}
{$IFDEF DARWIN}
  Cocoa_DestroyWindow(win);
{$ENDIF}

  //remove and free window object
  remove_link(windowlist, pLink(win));
  freemem(win);
end;

function GLPT_MakeCurrent(win: pGLPTwindow): boolean;
begin
  current_win := win;

{$IFDEF MSWINDOWS}
  exit(gdi_MakeCurrent(win));
{$ENDIF}
{$IFDEF LINUX}
  exit(X11_MakeCurrent(win));
{$ENDIF}
{$IFDEF DARWIN}
  exit(Cocoa_MakeCurrent(win));
{$ENDIF}
end;

procedure GLPT_SwapBuffers(win: pGLPTwindow);
begin
{$IFDEF MSWINDOWS}
  gdi_SwapBuffers(win);
{$ENDIF}
{$IFDEF LINUX}
  X11_SwapBuffers(win);
{$ENDIF}
{$IFDEF DARWIN}
  Cocoa_SwapBuffers(win);
{$ENDIF}
end;

procedure GLPT_GetFrameBufferSize(win: pGLPTwindow; out width, height: integer);
begin
{$IFDEF MSWINDOWS}
  gdi_GetFrameBufferSize(win, width, height);
{$ENDIF}
{$IFDEF LINUX}
  X11_GetFrameBufferSize(win, width, height);
{$ENDIF}
{$IFDEF Darwin}
  Cocoa_GetFrameBufferSize(win, width, height);
{$ENDIF}
end;

procedure GLPT_SetErrorCallback(errorCallback: GLPT_ErrorCallback);
begin
  errfunc := errorCallback;
end;

procedure GLPT_PollEvents;
var
  event: pGLPT_MessageRec;
  win: pGLPTwindow;
begin
{$IFDEF MSWINDOWS}
  gdi_PollEvents;
{$ENDIF}
{$IFDEF LINUX}
  X11_PollEvents;
{$ENDIF}
{$IFDEF DARWIN}
  Cocoa_PollEvents;
{$ENDIF}

  event := glptReadFirstMessage;

  if event <> nil then
  begin
    win := event^.win;
    if win > nil then
      if win^.event_callback <> nil then
        win^.event_callback(event);

    glptDeleteMessage(event);
  end;
end;

procedure GLPT_SetCursor(cursor: byte);
begin
{$IFDEF MSWINDOWS}
  gdi_SetCursor(cursor);
{$ENDIF}
{$IFDEF LINUX}
  X11_SetCursor(cursor);
{$ENDIF}
{$IFDEF DARWIN}
  Cocoa_SetCursor(cursor);
{$ENDIF}
end;

procedure GLPT_GetDisplayCoords(var dr: GLPTRect);
begin
{$IFDEF MSWINDOWS}
  gdi_GetDisplayCoords(dr);
{$ENDIF}
{$IFDEF LINUX}
  X11_GetDisplayCoords(dr);
{$ENDIF}
{$IFDEF DARWIN}
  Cocoa_GetDisplayCoords(dr);
{$ENDIF}
end;

end.
