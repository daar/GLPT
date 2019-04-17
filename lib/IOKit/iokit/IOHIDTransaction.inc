{ Parsed from IOKit.framework IOHIDTransaction.h }

{$ifndef _IOKIT_HID_IOHIDTRANSACTION_USER_H}
{$ifdef TYPES}
type
  IOHIDTransactionRef = OpaqueType;
{$endif}

{$ifdef FUNCTIONS}
function IOHIDTransactionGetTypeID: CFTypeID; cdecl; external;
function IOHIDTransactionCreate(allocator: CFAllocatorRef; device: IOHIDDeviceRef; direction: IOHIDTransactionDirectionType; options: IOOptionBits): IOHIDTransactionRef; cdecl; external;
function IOHIDTransactionGetDevice(transaction: IOHIDTransactionRef): IOHIDDeviceRef; cdecl; external;
function IOHIDTransactionGetDirection(transaction: IOHIDTransactionRef): IOHIDTransactionDirectionType; cdecl; external;
procedure IOHIDTransactionSetDirection(transaction: IOHIDTransactionRef; direction: IOHIDTransactionDirectionType); cdecl; external;
procedure IOHIDTransactionAddElement(transaction: IOHIDTransactionRef; element: IOHIDElementRef); cdecl; external;
procedure IOHIDTransactionRemoveElement(transaction: IOHIDTransactionRef; element: IOHIDElementRef); cdecl; external;
function IOHIDTransactionContainsElement(transaction: IOHIDTransactionRef; element: IOHIDElementRef): Boolean; cdecl; external;
procedure IOHIDTransactionScheduleWithRunLoop(transaction: IOHIDTransactionRef; runLoop: CFRunLoopRef; runLoopMode: CFStringRef); cdecl; external;
procedure IOHIDTransactionUnscheduleFromRunLoop(transaction: IOHIDTransactionRef; runLoop: CFRunLoopRef; runLoopMode: CFStringRef); cdecl; external;
procedure IOHIDTransactionSetValue(transaction: IOHIDTransactionRef; element: IOHIDElementRef; value: IOHIDValueRef; options: IOOptionBits); cdecl; external;
function IOHIDTransactionGetValue(transaction: IOHIDTransactionRef; element: IOHIDElementRef; options: IOOptionBits): IOHIDValueRef; cdecl; external;
function IOHIDTransactionCommit(transaction: IOHIDTransactionRef): IOReturn; cdecl; external;
function IOHIDTransactionCommitWithCallback(transaction: IOHIDTransactionRef; timeout: CFTimeInterval; callback: IOHIDCallback; context: pointer): IOReturn; cdecl; external;
procedure IOHIDTransactionClear(transaction: IOHIDTransactionRef); cdecl; external;
{$endif}

{$endif}
