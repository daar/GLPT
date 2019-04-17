{ Parsed from IOKit.framework IOHIDQueue.h }

{$ifndef _IOKIT_HID_IOHIDQUEUE_USER_H}
{$ifdef TYPES}
type
  IOHIDQueueRef = OpaqueType;
{$endif}

{$ifdef FUNCTIONS}
function IOHIDQueueGetTypeID: CFTypeID; cdecl; external;
function IOHIDQueueCreate(allocator: CFAllocatorRef; device: IOHIDDeviceRef; depth: CFIndex; options: IOOptionBits): IOHIDQueueRef; cdecl; external;
function IOHIDQueueGetDevice(queue: IOHIDQueueRef): IOHIDDeviceRef; cdecl; external;
function IOHIDQueueGetDepth(queue: IOHIDQueueRef): CFIndex; cdecl; external;
procedure IOHIDQueueSetDepth(queue: IOHIDQueueRef; depth: CFIndex); cdecl; external;
procedure IOHIDQueueAddElement(queue: IOHIDQueueRef; element: IOHIDElementRef); cdecl; external;
procedure IOHIDQueueRemoveElement(queue: IOHIDQueueRef; element: IOHIDElementRef); cdecl; external;
function IOHIDQueueContainsElement(queue: IOHIDQueueRef; element: IOHIDElementRef): Boolean; cdecl; external;
procedure IOHIDQueueStart(queue: IOHIDQueueRef); cdecl; external;
procedure IOHIDQueueStop(queue: IOHIDQueueRef); cdecl; external;
procedure IOHIDQueueScheduleWithRunLoop(queue: IOHIDQueueRef; runLoop: CFRunLoopRef; runLoopMode: CFStringRef); cdecl; external;
procedure IOHIDQueueUnscheduleFromRunLoop(queue: IOHIDQueueRef; runLoop: CFRunLoopRef; runLoopMode: CFStringRef); cdecl; external;
procedure IOHIDQueueRegisterValueAvailableCallback(queue: IOHIDQueueRef; callback: IOHIDCallback; context: pointer); cdecl; external;
function IOHIDQueueCopyNextValue(queue: IOHIDQueueRef): IOHIDValueRef; cdecl; external;
function IOHIDQueueCopyNextValueWithTimeout(queue: IOHIDQueueRef; timeout: CFTimeInterval): IOHIDValueRef; cdecl; external;
{$endif}

{$endif}
