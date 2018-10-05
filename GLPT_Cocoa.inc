
type
  TCocoaWindow = objcclass (NSWindow)
    ref: pGLPTwindow;
  end;

type
  TCocoaApp = objcclass (NSApplication)
    procedure poll; message 'poll';
  end;

function Cocoa_GetKeyboardShiftState: TShiftState; forward;
function Cocoa_GetTime: double; forward;

procedure TCocoaApp.poll;
var
  event: NSEvent;
  keychar: string;
  params: GLPT_MessageParams;
  win: pGLPTwindow = nil;
  pool: NSAutoreleasePool;
begin
  pool := NSAutoreleasePool.alloc.init;
  event := nextEventMatchingMask_untilDate_inMode_dequeue(NSAnyEventMask, nil, NSDefaultRunLoopMode, true);
  if event <> nil then
    begin
      if event.window <> nil then
        win := TCocoaWindow(event.window).ref;

      case event.type_ of
        NSMouseMoved:
          begin
            params.mouse.x := trunc(event.locationInWindow.x);
            params.mouse.y := trunc(event.locationInWindow.y);
            params.mouse.shiftstate := Cocoa_GetKeyboardShiftState;
            params.mouse.timestamp := Cocoa_GetTime;
            glptPostMessage(win, GLPT_MESSAGE_MOUSEMOVE, params);
          end;
        NSLeftMouseDown, NSRightMouseDown:
          begin
            params.mouse.x := trunc(event.locationInWindow.x);
            params.mouse.y := trunc(event.locationInWindow.y);
            params.mouse.shiftstate := Cocoa_GetKeyboardShiftState;
            params.mouse.timestamp := Cocoa_GetTime;
            case event.type_ of
              NSLeftMouseDown:
                params.mouse.buttons := GLPT_MOUSE_BUTTON_LEFT;
              NSRightMouseDown:
                params.mouse.buttons := GLPT_MOUSE_BUTTON_RIGHT;
            end;
            glptPostMessage(win, GLPT_MESSAGE_MOUSEDOWN, params);
          end;
        NSLeftMouseUp, NSRightMouseUp:
          begin
            params.mouse.x := trunc(event.locationInWindow.x);
            params.mouse.y := trunc(event.locationInWindow.y);
            params.mouse.shiftstate := Cocoa_GetKeyboardShiftState;
            params.mouse.timestamp := Cocoa_GetTime;
            case event.type_ of
              NSLeftMouseUp:
                params.mouse.buttons := GLPT_MOUSE_BUTTON_LEFT;
              NSRightMouseUp:
                params.mouse.buttons := GLPT_MOUSE_BUTTON_RIGHT;
            end;
            glptPostMessage(win, GLPT_MESSAGE_MOUSEUP, params);
          end;
        NSKeyDown, NSKeyUp:
          begin
            keychar := event.charactersIgnoringModifiers.UTF8String;
            keychar := Lowercase(keychar);
            params.keyboard.keychar := Ord(byte(keychar[1]));
            params.keyboard.shiftstate := Cocoa_GetKeyboardShiftState;
            case event.type_ of
              NSKeyDown:
                glptPostMessage(win, GLPT_MESSAGE_KEYPRESS, params);
              NSKeyUp:
                glptPostMessage(win, GLPT_MESSAGE_KEYRELEASE, params);
            end;
          end;
        otherwise
          ;
      end;

      sendEvent(event);
      updateWindows;
    end;
  pool.release;
end;

{=============================================}
{@! ___OPENGL VIEW___ } 
{=============================================}

type
  TOpenGLView = objcclass (NSView)
    public
      function initWithFrame(frameRect: NSRect): id; override;
      function isOpaque: Boolean; override;
    private
      openGLContext: NSOpenGLContext;
      
      function defaultPixelFormat: NSOpenGLPixelFormat; message 'defaultPixelFormat';
      procedure viewDidMoveToWindow; override;
      procedure frameChanged (sender: NSNotification); message 'frameChanged:';
      procedure drawRect(dirtyRect: NSRect); override;
      procedure reshape; message 'reshape';
  end;

// note: setValues_forParameter in RTL headers is parsed wrong
type
  NSOpenGLContext_Fixed = objccategory external (NSOpenGLContext)
    procedure setValues_forParameter_fixed (vals: pointer; param: NSOpenGLContextParameter); overload; message 'setValues:forParameter:';
  end;

procedure TOpenGLView.viewDidMoveToWindow;
var
  swapInterval: integer = 1;
  opacity: integer = 0;
begin
  inherited viewDidMoveToWindow;
  
  if openGLContext = nil then
    begin
      openGLContext := NSOpenGLContext.alloc.initWithFormat_shareContext(defaultPixelFormat, nil);
      if openGLContext = nil then
        glptError(GLPT_ERROR_PLATFORM, 'invalid NSOpenGLContext');
      openGLContext.makeCurrentContext;
      openGLContext.setView(self);

      openGLContext.setValues_forParameter_fixed(@swapInterval, NSOpenGLCPSwapInterval);

      if not isOpaque then
        openGLContext.setValues_forParameter_fixed(@opacity, NSOpenGLCPSurfaceOpacity);
    end;
  if window = nil then
    openGLContext.clearDrawable;
end;

procedure TOpenGLView.frameChanged (sender: NSNotification);
begin
  if openGLContext <> nil then
    reshape;
end;

procedure TOpenGLView.drawRect(dirtyRect: NSRect);
begin 
  openGLContext.flushBuffer;
end;

procedure TOpenGLView.reshape;
begin
  openGLContext.update;
end;

function TOpenGLView.isOpaque: Boolean;
begin
  // return false to make the view transparent
  result := window.backgroundColor.alphaComponent > 0;
end;

function TOpenGLView.defaultPixelFormat: NSOpenGLPixelFormat;
const
  NSOpenGLPFAOpenGLProfile = 99 { available in 10_7 };
const
  NSOpenGLProfileVersionLegacy = $1000 { available in 10_7 };
  NSOpenGLProfileVersion3_2Core = $3200 { available in 10_7 };
  NSOpenGLProfileVersion4_1Core = $4100 { available in 10_10 };
var
  attributes: array[0..4] of NSOpenGLPixelFormatAttribute;
begin
  attributes[0] := NSOpenGLPFAWindow;
  attributes[1] := NSOpenGLPFADoubleBuffer;
  //NSOpenGLPFAColorSize, 32
  //NSOpenGLPFADepthSize, 32
  // todo: invalid NSOpenGLPixelFormat with core profile.
  // this is a bug but I don't know how to fix it right now
  attributes[2] := NSOpenGLPFAOpenGLProfile;
  attributes[3] := NSOpenGLProfileVersionLegacy;
  attributes[4] := 0;

  result := NSOpenGLPixelFormat.alloc.initWithAttributes(@attributes).autorelease;
  if result = nil then
    glptError(GLPT_ERROR_PLATFORM, 'invalid NSOpenGLPixelFormat');
end;

function TOpenGLView.initWithFrame(frameRect: NSRect): id;
begin
  result := inherited initWithFrame(frameRect);
  if result <> nil then
    begin
      NSNotificationCenter(NSNotificationCenter.defaultCenter).addObserver_selector_name_object(result, objcselector('frameChanged:'), NSViewGlobalFrameDidChangeNotification, nil);
    end;
end;

{=============================================}
{@! ___GLTP___ } 
{=============================================}

function Cocoa_GetTime: double;
begin
  result := NSDate.date.timeIntervalSince1970;
end;

function Cocoa_Terminate: boolean;
begin
  NSApp.terminate(nil);
  exit(True);
end;

procedure Cocoa_DestroyWindow(var win: pGLPTwindow);
begin
  win^.ref.close;
  win^.ref := nil;
end;

function Cocoa_CreateWindow(win: pGLPTwindow; posx, posy, sizex, sizey: integer; title: PChar): boolean;
var
  window: TCocoaWindow;
  openGLView: TOpenGLView;
begin
  window := TCocoaWindow.alloc.initWithContentRect_styleMask_backing_defer(NSMakeRect(posx, posy, sizex, sizey), NSTitledWindowMask + NSClosableWindowMask + NSMiniaturizableWindowMask + NSResizableWindowMask, NSBackingStoreBuffered, false);
  window.setTitle(NSSTR(title));
  window.ref := win;

  openGLView := TOpenGLView.alloc.initWithFrame(window.contentView.bounds);
  window.setContentView(openGLView);
  openGLView.release;

  // todo: how do we stop beeping on mouse down??
  window.makeFirstResponder(openGLView);

  window.makeKeyAndOrderFront(NSApplication.sharedApplication);

  win^.context := openGLView.openGLContext;
  win^.ref := window;

  exit(True);
end;

function Cocoa_MakeCurrent(win: pGLPTwindow): boolean;
begin
  win^.context.makeCurrentContext;

  exit(True);
end;

procedure Cocoa_SwapBuffers(win: pGLPTwindow);
begin
  win^.ref.contentView.display;
end;

procedure Cocoa_GetFrameBufferSize(win: pGLPTwindow; out width, height: integer);
begin
  width:= trunc(win^.ref.contentView.bounds.size.width);
  height:= trunc(win^.ref.contentView.bounds.size.height);
end;

procedure Cocoa_PollEvents;
begin
  // note: do we really need to call on main thread? isn't the first thread "main"?
  //TCocoaApp(TCocoaApp.sharedApplication).poll;
  NSApplication.sharedApplication.performSelectorOnMainThread_withObject_waitUntilDone(objcselector('poll'), nil, true);
end;

procedure Cocoa_GetDisplayCoords(var dr: GLPTRect);
var
  screenFrame: NSRect;
begin
  screenFrame := NSScreen.mainScreen.frame;

  dr.top := trunc(NSMinY(screenFrame));
  dr.left := trunc(NSMinX(screenFrame));
  dr.bottom := trunc(NSMaxY(screenFrame));
  dr.right := trunc(NSMaxX(screenFrame));
end;

function Cocoa_GetKeyboardShiftState: TShiftState;
var
  modifierFlags: NSUInteger;
begin
  modifierFlags := NSApp.currentEvent.modifierFlags;
  Result := [];

  if modifierFlags = (modifierFlags or NSShiftKeyMask) then
    Include(Result, ssShift);

  if modifierFlags = (modifierFlags or NSControlKeyMask) then
    Include(Result, ssCtrl);

  if modifierFlags = (modifierFlags or NSAlternateKeyMask) then
    Include(Result, ssAlt);

  if modifierFlags = (modifierFlags or NSCommandKeyMask) then
    Include(Result, ssSuper);
end;


procedure SetupMainMenu;

  procedure AddMenu (menu: NSMenu);
  var
    menuItem: NSMenuItem;
  begin
    menuItem := NSMenuItem.alloc.initWithTitle_action_keyEquivalent(menu.title, nil, NSSTR('')).autorelease;
    menuItem.setSubmenu(menu);
    NSApp.mainMenu.addItem(menuItem);
  end;

var
  mainMenu: NSMenu;
  appleMenu: NSMenu;
  windowMenu: NSMenu;
begin

  // main menu
  mainMenu := NSMenu.alloc.init.autorelease;
  NSApp.setMainMenu(mainMenu);

  // apple menu
  appleMenu := NSMenu.alloc.initWithTitle(NSSTR('')).autorelease;
  appleMenu.addItemWithTitle_action_keyEquivalent(NSSTR('Quit'), objcselector('terminate:'), NSSTR('q'));

  AddMenu(appleMenu);

  // window menu
  windowMenu := NSMenu.alloc.initWithTitle(NSSTR('Window')).autorelease;
  windowMenu.addItemWithTitle_action_keyEquivalent(NSSTR('Minimize'), objcselector('performMiniaturize:'), NSSTR('m'));
  windowMenu.addItemWithTitle_action_keyEquivalent(NSSTR('Zoom'), objcselector('performZoom:'), NSSTR(''));

  AddMenu(windowMenu);
end;

function Cocoa_Init: boolean;
var
  pool: NSAutoreleasePool;
  app: TCocoaApp;
begin  
  pool := NSAutoreleasePool.alloc.init;

  app := TCocoaApp(TCocoaApp.sharedApplication);

  if NSApp.mainMenu = nil then
    SetupMainMenu;
  app.finishLaunching;

  NSApp.activateIgnoringOtherApps(true);

  pool.release;

  exit(True);
end;

procedure Cocoa_SetCursor(cursor: byte);
begin
  case cursor of
    GLPT_ARROW_CURSOR:
      NSCursor.arrowCursor.set_;
    GLPT_IBEAM_CURSOR:
      NSCursor.IBeamCursor.set_;
    GLPT_CROSSHAIR_CURSOR:
      NSCursor.crosshairCursor.set_;
    GLPT_HAND_CURSOR:
      NSCursor.openHandCursor.set_;
    GLPT_HRESIZE_CURSOR:
      NSCursor.resizeLeftRightCursor.set_;
    GLPT_VRESIZE_CURSOR:
      NSCursor.resizeUpDownCursor.set_;
    otherwise
      halt(-1);
  end;
end;
