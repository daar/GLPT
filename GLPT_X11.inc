var
  linux_scancode_table: array[0..235] of GLPT_Scancode = (
    {  0 }    GLPT_SCANCODE_UNKNOWN,
    {  1 }    GLPT_SCANCODE_ESCAPE,
    {  2 }    GLPT_SCANCODE_1,
    {  3 }    GLPT_SCANCODE_2,
    {  4 }    GLPT_SCANCODE_3,
    {  5 }    GLPT_SCANCODE_4,
    {  6 }    GLPT_SCANCODE_5,
    {  7 }    GLPT_SCANCODE_6,
    {  8 }    GLPT_SCANCODE_7,
    {  9 }    GLPT_SCANCODE_8,
    {  10 }    GLPT_SCANCODE_9,
    {  11 }    GLPT_SCANCODE_0,
    {  12 }    GLPT_SCANCODE_MINUS,
    {  13 }    GLPT_SCANCODE_EQUALS,
    {  14 }    GLPT_SCANCODE_BACKSPACE,
    {  15 }    GLPT_SCANCODE_TAB,
    {  16 }    GLPT_SCANCODE_Q,
    {  17 }    GLPT_SCANCODE_W,
    {  18 }    GLPT_SCANCODE_E,
    {  19 }    GLPT_SCANCODE_R,
    {  20 }    GLPT_SCANCODE_T,
    {  21 }    GLPT_SCANCODE_Y,
    {  22 }    GLPT_SCANCODE_U,
    {  23 }    GLPT_SCANCODE_I,
    {  24 }    GLPT_SCANCODE_O,
    {  25 }    GLPT_SCANCODE_P,
    {  26 }    GLPT_SCANCODE_LEFTBRACKET,
    {  27 }    GLPT_SCANCODE_RIGHTBRACKET,
    {  28 }    GLPT_SCANCODE_RETURN,
    {  29 }    GLPT_SCANCODE_LCTRL,
    {  30 }    GLPT_SCANCODE_A,
    {  31 }    GLPT_SCANCODE_S,
    {  32 }    GLPT_SCANCODE_D,
    {  33 }    GLPT_SCANCODE_F,
    {  34 }    GLPT_SCANCODE_G,
    {  35 }    GLPT_SCANCODE_H,
    {  36 }    GLPT_SCANCODE_J,
    {  37 }    GLPT_SCANCODE_K,
    {  38 }    GLPT_SCANCODE_L,
    {  39 }    GLPT_SCANCODE_SEMICOLON,
    {  40 }    GLPT_SCANCODE_APOSTROPHE,
    {  41 }    GLPT_SCANCODE_GRAVE,
    {  42 }    GLPT_SCANCODE_LSHIFT,
    {  43 }    GLPT_SCANCODE_BACKSLASH,
    {  44 }    GLPT_SCANCODE_Z,
    {  45 }    GLPT_SCANCODE_X,
    {  46 }    GLPT_SCANCODE_C,
    {  47 }    GLPT_SCANCODE_V,
    {  48 }    GLPT_SCANCODE_B,
    {  49 }    GLPT_SCANCODE_N,
    {  50 }    GLPT_SCANCODE_M,
    {  51 }    GLPT_SCANCODE_COMMA,
    {  52 }    GLPT_SCANCODE_PERIOD,
    {  53 }    GLPT_SCANCODE_SLASH,
    {  54 }    GLPT_SCANCODE_RSHIFT,
    {  55 }    GLPT_SCANCODE_KP_MULTIPLY,
    {  56 }    GLPT_SCANCODE_LALT,
    {  57 }    GLPT_SCANCODE_SPACE,
    {  58 }    GLPT_SCANCODE_CAPSLOCK,
    {  59 }    GLPT_SCANCODE_F1,
    {  60 }    GLPT_SCANCODE_F2,
    {  61 }    GLPT_SCANCODE_F3,
    {  62 }    GLPT_SCANCODE_F4,
    {  63 }    GLPT_SCANCODE_F5,
    {  64 }    GLPT_SCANCODE_F6,
    {  65 }    GLPT_SCANCODE_F7,
    {  66 }    GLPT_SCANCODE_F8,
    {  67 }    GLPT_SCANCODE_F9,
    {  68 }    GLPT_SCANCODE_F10,
    {  69 }    GLPT_SCANCODE_NUMLOCKCLEAR,
    {  70 }    GLPT_SCANCODE_SCROLLLOCK,
    {  71 }    GLPT_SCANCODE_KP_7,
    {  72 }    GLPT_SCANCODE_KP_8,
    {  73 }    GLPT_SCANCODE_KP_9,
    {  74 }    GLPT_SCANCODE_KP_MINUS,
    {  75 }    GLPT_SCANCODE_KP_4,
    {  76 }    GLPT_SCANCODE_KP_5,
    {  77 }    GLPT_SCANCODE_KP_6,
    {  78 }    GLPT_SCANCODE_KP_PLUS,
    {  79 }    GLPT_SCANCODE_KP_1,
    {  80 }    GLPT_SCANCODE_KP_2,
    {  81 }    GLPT_SCANCODE_KP_3,
    {  82 }    GLPT_SCANCODE_KP_0,
    {  83 }    GLPT_SCANCODE_KP_PERIOD,
    0,
    {  85 }    GLPT_SCANCODE_UNKNOWN, { KEY_ZENKAKUHANKAKU }
    {  86 }    GLPT_SCANCODE_NONUSBACKSLASH, { KEY_102ND }
    {  87 }    GLPT_SCANCODE_F11,
    {  88 }    GLPT_SCANCODE_F12,
    {  89 }    GLPT_SCANCODE_INTERNATIONAL1, { KEY_RO }
    {  90 }    GLPT_SCANCODE_LANG3, { KEY_KATAKANA }
    {  91 }    GLPT_SCANCODE_LANG4, { KEY_HIRAGANA }
    {  92 }    GLPT_SCANCODE_INTERNATIONAL4, { KEY_HENKAN }
    {  93 }    GLPT_SCANCODE_INTERNATIONAL2, { KEY_KATAKANAHIRAGANA }
    {  94 }    GLPT_SCANCODE_INTERNATIONAL5, { KEY_MUHENKAN }
    {  95 }    GLPT_SCANCODE_INTERNATIONAL5, { KEY_KPJPCOMMA }
    {  96 }    GLPT_SCANCODE_KP_ENTER,
    {  97 }    GLPT_SCANCODE_RCTRL,
    {  98 }    GLPT_SCANCODE_KP_DIVIDE,
    {  99 }    GLPT_SCANCODE_SYSREQ,
    {  100 }    GLPT_SCANCODE_RALT,
    {  101 }    GLPT_SCANCODE_UNKNOWN, { KEY_LINEFEED }
    {  102 }    GLPT_SCANCODE_HOME,
    {  103 }    GLPT_SCANCODE_UP,
    {  104 }    GLPT_SCANCODE_PAGEUP,
    {  105 }    GLPT_SCANCODE_LEFT,
    {  106 }    GLPT_SCANCODE_RIGHT,
    {  107 }    GLPT_SCANCODE_END,
    {  108 }    GLPT_SCANCODE_DOWN,
    {  109 }    GLPT_SCANCODE_PAGEDOWN,
    {  110 }    GLPT_SCANCODE_INSERT,
    {  111 }    GLPT_SCANCODE_DELETE,
    {  112 }    GLPT_SCANCODE_UNKNOWN, { KEY_MACRO }
    {  113 }    GLPT_SCANCODE_MUTE,
    {  114 }    GLPT_SCANCODE_VOLUMEDOWN,
    {  115 }    GLPT_SCANCODE_VOLUMEUP,
    {  116 }    GLPT_SCANCODE_POWER,
    {  117 }    GLPT_SCANCODE_KP_EQUALS,
    {  118 }    GLPT_SCANCODE_KP_PLUSMINUS,
    {  119 }    GLPT_SCANCODE_PAUSE,
    0,
    {  121 }    GLPT_SCANCODE_KP_COMMA,
    {  122 }    GLPT_SCANCODE_LANG1, { KEY_HANGUEL }
    {  123 }    GLPT_SCANCODE_LANG2, { KEY_HANJA }
    {  124 }    GLPT_SCANCODE_INTERNATIONAL3, { KEY_YEN }
    {  125 }    GLPT_SCANCODE_LGUI,
    {  126 }    GLPT_SCANCODE_RGUI,
    {  127 }    GLPT_SCANCODE_UNKNOWN, { KEY_COMPOSE }
    {  128 }    GLPT_SCANCODE_STOP,
    {  129 }    GLPT_SCANCODE_AGAIN,
    {  130 }    GLPT_SCANCODE_UNKNOWN, { KEY_PROPS }
    {  131 }    GLPT_SCANCODE_UNDO,
    {  132 }    GLPT_SCANCODE_UNKNOWN, { KEY_FRONT }
    {  133 }    GLPT_SCANCODE_COPY,
    {  134 }    GLPT_SCANCODE_UNKNOWN, { KEY_OPEN }
    {  135 }    GLPT_SCANCODE_PASTE,
    {  136 }    GLPT_SCANCODE_FIND,
    {  137 }    GLPT_SCANCODE_CUT,
    {  138 }    GLPT_SCANCODE_HELP,
    {  139 }    GLPT_SCANCODE_MENU,
    {  140 }    GLPT_SCANCODE_CALCULATOR,
    {  141 }    GLPT_SCANCODE_UNKNOWN, { KEY_SETUP }
    {  142 }    GLPT_SCANCODE_SLEEP,
    {  143 }    GLPT_SCANCODE_UNKNOWN, { KEY_WAKEUP }
    {  144 }    GLPT_SCANCODE_UNKNOWN, { KEY_FILE }
    {  145 }    GLPT_SCANCODE_UNKNOWN, { KEY_SENDFILE }
    {  146 }    GLPT_SCANCODE_UNKNOWN, { KEY_DELETEFILE }
    {  147 }    GLPT_SCANCODE_UNKNOWN, { KEY_XFER }
    {  148 }    GLPT_SCANCODE_UNKNOWN, { KEY_PROG1 }
    {  149 }    GLPT_SCANCODE_UNKNOWN, { KEY_PROG2 }
    {  150 }    GLPT_SCANCODE_UNKNOWN, { KEY_WWW }
    {  151 }    GLPT_SCANCODE_UNKNOWN, { KEY_MSDOS }
    {  152 }    GLPT_SCANCODE_UNKNOWN, { KEY_COFFEE }
    {  153 }    GLPT_SCANCODE_UNKNOWN, { KEY_DIRECTION }
    {  154 }    GLPT_SCANCODE_UNKNOWN, { KEY_CYCLEWINDOWS }
    {  155 }    GLPT_SCANCODE_MAIL,
    {  156 }    GLPT_SCANCODE_AC_BOOKMARKS,
    {  157 }    GLPT_SCANCODE_COMPUTER,
    {  158 }    GLPT_SCANCODE_AC_BACK,
    {  159 }    GLPT_SCANCODE_AC_FORWARD,
    {  160 }    GLPT_SCANCODE_UNKNOWN, { KEY_CLOSECD }
    {  161 }    GLPT_SCANCODE_EJECT, { KEY_EJECTCD }
    {  162 }    GLPT_SCANCODE_UNKNOWN, { KEY_EJECTCLOSECD }
    {  163 }    GLPT_SCANCODE_AUDIONEXT, { KEY_NEXTSONG }
    {  164 }    GLPT_SCANCODE_AUDIOPLAY, { KEY_PLAYPAUSE }
    {  165 }    GLPT_SCANCODE_AUDIOPREV, { KEY_PREVIOUSSONG }
    {  166 }    GLPT_SCANCODE_UNKNOWN, { KEY_STOPCD }
    {  167 }    GLPT_SCANCODE_UNKNOWN, { KEY_RECORD }
    {  168 }    GLPT_SCANCODE_UNKNOWN, { KEY_REWIND }
    {  169 }    GLPT_SCANCODE_UNKNOWN, { KEY_PHONE }
    {  170 }    GLPT_SCANCODE_UNKNOWN, { KEY_ISO }
    {  171 }    GLPT_SCANCODE_UNKNOWN, { KEY_CONFIG }
    {  172 }    GLPT_SCANCODE_AC_HOME,
    {  173 }    GLPT_SCANCODE_AC_REFRESH,
    {  174 }    GLPT_SCANCODE_UNKNOWN, { KEY_EXIT }
    {  175 }    GLPT_SCANCODE_UNKNOWN, { KEY_MOVE }
    {  176 }    GLPT_SCANCODE_UNKNOWN, { KEY_EDIT }
    {  177 }    GLPT_SCANCODE_UNKNOWN, { KEY_SCROLLUP }
    {  178 }    GLPT_SCANCODE_UNKNOWN, { KEY_SCROLLDOWN }
    {  179 }    GLPT_SCANCODE_KP_LEFTPAREN,
    {  180 }    GLPT_SCANCODE_KP_RIGHTPAREN,
    {  181 }    GLPT_SCANCODE_UNKNOWN, { KEY_NEW }
    {  182 }    GLPT_SCANCODE_UNKNOWN, { KEY_REDO }
    {  183 }    GLPT_SCANCODE_F13,
    {  184 }    GLPT_SCANCODE_F14,
    {  185 }    GLPT_SCANCODE_F15,
    {  186 }    GLPT_SCANCODE_F16,
    {  187 }    GLPT_SCANCODE_F17,
    {  188 }    GLPT_SCANCODE_F18,
    {  189 }    GLPT_SCANCODE_F19,
    {  190 }    GLPT_SCANCODE_F20,
    {  191 }    GLPT_SCANCODE_F21,
    {  192 }    GLPT_SCANCODE_F22,
    {  193 }    GLPT_SCANCODE_F23,
    {  194 }    GLPT_SCANCODE_F24,
    0, 0, 0, 0,
    {  200 }    GLPT_SCANCODE_UNKNOWN, { KEY_PLAYCD }
    {  201 }    GLPT_SCANCODE_UNKNOWN, { KEY_PAUSECD }
    {  202 }    GLPT_SCANCODE_UNKNOWN, { KEY_PROG3 }
    {  203 }    GLPT_SCANCODE_UNKNOWN, { KEY_PROG4 }
    0,
    {  205 }    GLPT_SCANCODE_UNKNOWN, { KEY_SUSPEND }
    {  206 }    GLPT_SCANCODE_UNKNOWN, { KEY_CLOSE }
    {  207 }    GLPT_SCANCODE_UNKNOWN, { KEY_PLAY }
    {  208 }    GLPT_SCANCODE_UNKNOWN, { KEY_FASTFORWARD }
    {  209 }    GLPT_SCANCODE_UNKNOWN, { KEY_BASSBOOST }
    {  210 }    GLPT_SCANCODE_UNKNOWN, { KEY_PRINT }
    {  211 }    GLPT_SCANCODE_UNKNOWN, { KEY_HP }
    {  212 }    GLPT_SCANCODE_UNKNOWN, { KEY_CAMERA }
    {  213 }    GLPT_SCANCODE_UNKNOWN, { KEY_SOUND }
    {  214 }    GLPT_SCANCODE_UNKNOWN, { KEY_QUESTION }
    {  215 }    GLPT_SCANCODE_UNKNOWN, { KEY_EMAIL }
    {  216 }    GLPT_SCANCODE_UNKNOWN, { KEY_CHAT }
    {  217 }    GLPT_SCANCODE_AC_SEARCH,
    {  218 }    GLPT_SCANCODE_UNKNOWN, { KEY_CONNECT }
    {  219 }    GLPT_SCANCODE_UNKNOWN, { KEY_FINANCE }
    {  220 }    GLPT_SCANCODE_UNKNOWN, { KEY_SPORT }
    {  221 }    GLPT_SCANCODE_UNKNOWN, { KEY_SHOP }
    {  222 }    GLPT_SCANCODE_ALTERASE,
    {  223 }    GLPT_SCANCODE_CANCEL,
    {  224 }    GLPT_SCANCODE_BRIGHTNESSDOWN,
    {  225 }    GLPT_SCANCODE_BRIGHTNESSUP,
    {  226 }    GLPT_SCANCODE_UNKNOWN, { KEY_MEDIA }
    {  227 }    GLPT_SCANCODE_DISPLAYSWITCH, { KEY_SWITCHVIDEOMODE }
    {  228 }    GLPT_SCANCODE_KBDILLUMTOGGLE,
    {  229 }    GLPT_SCANCODE_KBDILLUMDOWN,
    {  230 }    GLPT_SCANCODE_KBDILLUMUP,
    {  231 }    GLPT_SCANCODE_UNKNOWN, { KEY_SEND }
    {  232 }    GLPT_SCANCODE_UNKNOWN, { KEY_REPLY }
    {  233 }    GLPT_SCANCODE_UNKNOWN, { KEY_FORWARDMAIL }
    {  234 }    GLPT_SCANCODE_UNKNOWN, { KEY_SAVE }
    {  235 }    GLPT_SCANCODE_UNKNOWN, { KEY_DOCUMENTS }
    {  236 }    GLPT_SCANCODE_UNKNOWN { KEY_BATTERY }
);

// GLPT_x11keyboard.c

(*

void
X11_UpdateKeymap(_THIS)
{
    GLPT_VideoData *data = (GLPT_VideoData ) _this->driverdata;
    int i;
    GLPT_Scancode scancode;
    GLPT_Keycode keymap[GLPT_NUM_SCANCODES];
    unsigned char group = 0;

    GLPT_GetDefaultKeymap(keymap);
    
#if GLPT_VIDEO_DRIVER_X11_HAS_XKBKEYCODETOKEYSYM
    {
        XkbStateRec state;
        if (X11_XkbGetState(data->display, XkbUseCoreKbd, &state) == Success) {
            group = state.group;
        }
    }
#endif


    for (i = 0; i < GLPT_arraysize(data->key_layout); i++) {
        Uint32 key;

        { Make sure this is a valid scancode }
        scancode = data->key_layout[i];
        if (scancode == GLPT_SCANCODE_UNKNOWN) {
            continue;
        }

        { See if there is a UCS keycode for this scancode }
        key = X11_KeyCodeToUcs4(data->display, (KeyCode)i, group);
        if (key) {
            keymap[scancode] = key;
        } else {
            GLPT_Scancode keyScancode = X11_KeyCodeToSDLScancode(data->display, (KeyCode)i);

            switch (keyScancode) {
                case GLPT_SCANCODE_RETURN:
                    keymap[scancode] = SDLK_RETURN;
                    break;
                case GLPT_SCANCODE_ESCAPE:
                    keymap[scancode] = SDLK_ESCAPE;
                    break;
                case GLPT_SCANCODE_BACKSPACE:
                    keymap[scancode] = SDLK_BACKSPACE;
                    break;
                case GLPT_SCANCODE_TAB:
                    keymap[scancode] = SDLK_TAB;
                    break;
                case GLPT_SCANCODE_DELETE:
                    keymap[scancode] = SDLK_DELETE;
                    break;
                default:
                    keymap[scancode] = GLPT_SCANCODE_TO_KEYCODE(keyScancode);
                    break;
            }
        }
    }
    GLPT_SetKeymap(0, keymap, GLPT_NUM_SCANCODES);
}

*)

function ConvertShiftState(AState: Cardinal): TShiftState;
begin
  Result := [];
  if (AState and Button1Mask) <> 0 then
    Include(Result, ssLeft);
  if (AState and Button2Mask) <> 0 then
    Include(Result, ssMiddle);
  if (AState and Button3Mask) <> 0 then
    Include(Result, ssRight);
  if (AState and ShiftMask) <> 0 then
    Include(Result, ssShift);
  if (AState and LockMask) <> 0 then
    Include(Result, ssCaps);
  if (AState and ControlMask) <> 0 then
    Include(Result, ssCtrl);
  if (AState and Mod1Mask) <> 0 then
    Include(Result, ssAlt);
  { Numlock is often permanently enabled after booting, so this always gives
    use the incorrect ShiftState. So we simply stop checking ssNum as part
    of the ShiftState. }
//  if (AState and Mod2Mask) <> 0 then
//    Include(Result, ssNum);
  { NOTE: Mod3Mask is normally unused for some reason }
  if (AState and Mod4Mask) <> 0 then   { aka "Windows key" }
    Include(Result, ssSuper);
  if (AState and Mod5Mask) <> 0 then
    Include(Result, ssScroll);
end;

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
                           GLPT_SetWindowShouldClose(win, True);
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

                      params.keyboard.keycode := Ord(buffer[0]);
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
                         repeat
                           //
                         until not XCheckTypedWindowEvent(win^.dpy, event.xmotion.window, X.MotionNotify, @event);

                         params.mouse.x          := event.xmotion.x;
                         params.mouse.y          := event.xmotion.y;
                         params.mouse.Buttons    := (event.xmotion.state and $FF00) shr 8;
                         params.mouse.shiftstate := ConvertShiftState(event.xmotion.state);

                         glptPostMessage(win, GLPT_MESSAGE_MOUSEMOVE, params);
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

//  exit(ConvertShiftState(State));
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
