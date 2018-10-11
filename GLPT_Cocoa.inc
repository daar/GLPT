
{=============================================}
{@! ___COCOA UTILS___ } 
{=============================================}

function NSSTR(str: string): NSString; overload;
begin
  result := NSString.stringWithCString_length(@str[1], length(str));
end;

{=============================================}
{@! ___COCOA WINDOW___ } 
{=============================================}

type
  TCocoaWindow = objcclass (NSWindow)
    private
      ref: pGLPTwindow;
      procedure close; override;
      procedure becomeKeyWindow; override;
      procedure resignKeyWindow; override;
      procedure doCommandBySelector(aSelector: SEL); override;
  end;

procedure TCocoaWindow.close;
begin
  ref^.shouldClose := true;
  inherited;
end;

procedure TCocoaWindow.doCommandBySelector(aSelector: SEL);
begin
  // do nothing to prevent beeping
end;

procedure TCocoaWindow.becomeKeyWindow;
var
  params: GLPT_MessageParams;
begin
  inherited;
  glptPostMessage(ref, GLPT_MESSAGE_ACTIVATE, params);      
end;

procedure TCocoaWindow.resignKeyWindow;
var
  params: GLPT_MessageParams;
begin
  inherited;
  glptPostMessage(ref, GLPT_MESSAGE_DEACTIVATE, params);
end;

{=============================================}
{@! ___COCOA APP___ } 
{=============================================}

type
  TCocoaApp = objcclass (NSApplication)
    procedure poll; message 'poll';
  end;

type
  TCocoaAppDelegate = objcclass (NSObject, NSApplicationDelegateProtocol)
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
  event := nextEventMatchingMask_untilDate_inMode_dequeue(NSAnyEventMask, {NSDate.distantPast}nil, NSDefaultRunLoopMode, true);
  if event <> nil then
    begin
      //writeln(event.description.utf8string);

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
      trackingArea: NSTrackingArea;

      function defaultPixelFormat: NSOpenGLPixelFormat; message 'defaultPixelFormat';
      function windowRef: pGLPTwindow; message 'windowRef';

      procedure viewDidMoveToWindow; override;
      procedure mouseEntered(theEvent: NSEvent); override;
      procedure mouseExited(theEvent: NSEvent); override;
      procedure doCommandBySelector(aSelector: SEL); override;
      procedure updateTrackingAreas; override;

      procedure frameChanged (sender: NSNotification); message 'frameChanged:';
      procedure drawRect(dirtyRect: NSRect); override;
      procedure reshape; message 'reshape';
  end;

// note: setValues_forParameter in RTL headers is parsed wrong
type
  NSOpenGLContext_Fixed = objccategory external (NSOpenGLContext)
    procedure setValues_forParameter_fixed (vals: pointer; param: NSOpenGLContextParameter); overload; message 'setValues:forParameter:';
  end;

function TOpenGLView.windowRef: pGLPTwindow;
begin
  result := TCocoaWindow(window).ref;
end;

procedure TOpenGLView.updateTrackingAreas;
begin 
  if trackingArea <> nil then
    removeTrackingArea(trackingArea);
  trackingArea := NSTrackingArea.alloc.initWithRect_options_owner_userInfo(bounds, NSTrackingMouseEnteredAndExited + NSTrackingActiveAlways, self, nil).autorelease;
  addTrackingArea(trackingArea);
end;

procedure TOpenGLView.doCommandBySelector(aSelector: SEL);
begin
  // do nothing to prevent beeping
end;

procedure TOpenGLView.mouseEntered(theEvent: NSEvent);
var
   params: GLPT_MessageParams;
begin
  glptPostMessage(windowRef, GLPT_MESSAGE_MOUSEENTER, params);      
end;

procedure TOpenGLView.mouseExited(theEvent: NSEvent);
var
   params: GLPT_MessageParams;
begin
  glptPostMessage(windowRef, GLPT_MESSAGE_MOUSEEXIT, params);   
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
var
  params: GLPT_MessageParams;
begin
  openGLContext.update;

  if window <> nil then
    begin
      params.rect.top := 0;
      params.rect.left := 0;
      params.rect.width  := trunc(bounds.size.width);
      params.rect.height := trunc(bounds.size.height);
      glptPostMessage(windowRef, GLPT_MESSAGE_RESIZE, params);      
    end;
end;

function TOpenGLView.isOpaque: Boolean;
begin
  // return false to make the view transparent
  result := window.backgroundColor.alphaComponent > 0;
end;

function TOpenGLView.defaultPixelFormat: NSOpenGLPixelFormat;
function Inc (var i: integer): integer;
begin
  i += 1;
  result := i;
end;
const
  NSOpenGLPFAOpenGLProfile = 99 { available in 10_7 };
const
  NSOpenGLProfileVersionLegacy = $1000 { available in 10_7 };
  NSOpenGLProfileVersion3_2Core = $3200 { available in 10_7 };
  NSOpenGLProfileVersion4_1Core = $4100 { available in 10_10 };
var
  attributes: array[0..4] of NSOpenGLPixelFormatAttribute;
  i: integer = -1;
  context: GLPT_Context;
begin
  context := TCocoaWindow(window).ref^.context;

  // note: implement this?
  //if (_this->gl_config.accelerated >= 0) {
  //    if (_this->gl_config.accelerated) {
  //        attr[i++] = NSOpenGLPFAAccelerated;
  //    } else {
  //        attr[i++] = NSOpenGLPFARendererID;
  //        attr[i++] = kCGLRendererGenericFloatID;
  //    }
  //}

  if context.doubleBuffer then
    attributes[Inc(i)] := NSOpenGLPFADoubleBuffer;
  attributes[Inc(i)] := NSOpenGLPFAColorSize;
  attributes[Inc(i)] := context.colorSize;
  attributes[Inc(i)] := NSOpenGLPFADepthSize;
  attributes[Inc(i)] := context.depthSize;
  attributes[Inc(i)] := NSOpenGLPFAStencilSize;
  attributes[Inc(i)] := context.stencilSize;
  attributes[Inc(i)] := NSOpenGLPFAOpenGLProfile;
  // note: we can only specify "legacy" or "core" on mac and the system will decide what version we actually get
  if context.profile = GLPT_CONTEXT_PROFILE_LEGACY then
    attributes[Inc(i)] := NSOpenGLProfileVersionLegacy
  else if context.profile = GLPT_CONTEXT_PROFILE_CORE then
    begin
      if context.majorVersion = 3 then
        attributes[Inc(i)] := NSOpenGLProfileVersion3_2Core
      else if context.majorVersion = 4 then
        attributes[Inc(i)] := NSOpenGLProfileVersion4_1Core
      else
        glptError(GLPT_ERROR_PLATFORM, 'invalid core profile major version');
    end
  else
    glptError(GLPT_ERROR_PLATFORM, 'invalid context profile');
  attributes[Inc(i)] := 0;

  result := NSOpenGLPixelFormat.alloc.initWithAttributes(@attributes).autorelease;
  if result = nil then
    glptError(GLPT_ERROR_PLATFORM, 'invalid NSOpenGLPixelFormat');
end;

function TOpenGLView.initWithFrame(frameRect: NSRect): id;
begin
  result := inherited initWithFrame(frameRect);
  if result <> nil then
    NSNotificationCenter(NSNotificationCenter.defaultCenter).addObserver_selector_name_object(result, objcselector('frameChanged:'), NSViewGlobalFrameDidChangeNotification, nil);
end;

{=============================================}
{@! ___GLPT___ } 
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
const
  NSWindowCollectionBehaviorFullScreenPrimary = 1 shl 7 { available in 10_7 };
  NSWindowCollectionBehaviorFullScreenAuxiliary = 1 shl 8 { available in 10_7 };
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
  //window.makeFirstResponder(openGLView);

  window.setCollectionBehavior(NSWindowCollectionBehaviorFullScreenPrimary);
  window.setAcceptsMouseMovedEvents(true);
  window.makeKeyAndOrderFront(nil);

  win^.glcontext := openGLView.openGLContext;
  win^.ref := window;

  exit(True);
end;

function Cocoa_MakeCurrent(win: pGLPTwindow): boolean;
begin
  win^.glcontext.makeCurrentContext;

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
  NSApp.performSelectorOnMainThread_withObject_waitUntilDone(objcselector('poll'), nil, true);
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
  appleMenu.addItemWithTitle_action_keyEquivalent(NSSTR('Quit '+NSProcessInfo.processInfo.processName.UTF8String), objcselector('terminate:'), NSSTR('q'));

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
  delegate: TCocoaAppDelegate;
begin  
  // https://hero.handmade.network/forums/code-discussion/t/1409-main_game_loop_on_os_x

  pool := NSAutoreleasePool.alloc.init;

  app := TCocoaApp(TCocoaApp.sharedApplication);
  
  delegate := TCocoaAppDelegate.alloc.init;
  app.setDelegate(delegate);

  NSApp.setActivationPolicy(NSApplicationActivationPolicyRegular);
  NSApp.activateIgnoringOtherApps(true);

  if NSApp.mainMenu = nil then
    SetupMainMenu;
  app.finishLaunching;

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
      begin
        NSCursor.arrowCursor.set_;
        glptError(GLPT_ERROR_PLATFORM, 'cursor is not supported, using GLPT_ARROW_CURSOR instead.');
      end;
  end;
end;
