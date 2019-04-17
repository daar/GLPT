{ Parsed from IOKit.framework IOHIDManager.h }

{$ifndef _IOKIT_HID_IOHIDMANAGER_H_}
{$ifdef TYPES}
type
  IOHIDManagerOptions = cuint32;
  IOHIDManagerOptionsPtr = ^IOHIDManagerOptions;

const
  kIOHIDManagerOptionNone = $0;
  kIOHIDManagerOptionUsePersistentProperties = $1;
  kIOHIDManagerOptionDoNotLoadProperties = $2;
  kIOHIDManagerOptionDoNotSaveProperties = $4;

type
  IOHIDManagerRef = pointer;
{$endif}

{$ifdef FUNCTIONS}
function IOHIDManagerCreate(allocator: CFAllocatorRef; options: IOOptionBits): IOHIDManagerRef; cdecl; external;
function IOHIDManagerOpen(manager: IOHIDManagerRef; options: IOOptionBits): IOReturn; cdecl; external;
function IOHIDManagerClose(manager: IOHIDManagerRef; options: IOOptionBits): IOReturn; cdecl; external;
function IOHIDManagerGetProperty(manager: IOHIDManagerRef; key: CFStringRef): CFTypeRef; cdecl; external;
function IOHIDManagerSetProperty(manager: IOHIDManagerRef; key: CFStringRef; value: CFTypeRef): Boolean; cdecl; external;
procedure IOHIDManagerScheduleWithRunLoop(manager: IOHIDManagerRef; runLoop: CFRunLoopRef; runLoopMode: CFStringRef); cdecl; external;
procedure IOHIDManagerUnscheduleFromRunLoop(manager: IOHIDManagerRef; runLoop: CFRunLoopRef; runLoopMode: CFStringRef); cdecl; external;
procedure IOHIDManagerSetDeviceMatching(manager: IOHIDManagerRef; matching: CFDictionaryRef); cdecl; external;
procedure IOHIDManagerSetDeviceMatchingMultiple(manager: IOHIDManagerRef; multiple: CFArrayRef); cdecl; external;
function IOHIDManagerCopyDevices(manager: IOHIDManagerRef): CFSetRef; cdecl; external;
procedure IOHIDManagerRegisterDeviceMatchingCallback(manager: IOHIDManagerRef; callback: IOHIDDeviceCallback; context: pointer); cdecl; external;
procedure IOHIDManagerRegisterDeviceRemovalCallback(manager: IOHIDManagerRef; callback: IOHIDDeviceCallback; context: pointer); cdecl; external;
procedure IOHIDManagerRegisterInputReportCallback(manager: IOHIDManagerRef; callback: IOHIDReportCallback; context: pointer); cdecl; external;
procedure IOHIDManagerRegisterInputValueCallback(manager: IOHIDManagerRef; callback: IOHIDValueCallback; context: pointer); cdecl; external;
procedure IOHIDManagerSetInputValueMatching(manager: IOHIDManagerRef; matching: CFDictionaryRef); cdecl; external;
procedure IOHIDManagerSetInputValueMatchingMultiple(manager: IOHIDManagerRef; multiple: CFArrayRef); cdecl; external;
procedure IOHIDManagerSaveToPropertyDomain(manager: IOHIDManagerRef; applicationID: CFStringRef; userName: CFStringRef; hostName: CFStringRef; options: IOOptionBits); cdecl; external;
{$endif}

{$endif}
