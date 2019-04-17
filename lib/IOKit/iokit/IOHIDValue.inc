{ Parsed from IOKit.framework IOHIDValue.h }

{$ifndef _IOKIT_HID_IOHIDELEMENTEVENT_H}
{$ifdef FUNCTIONS}
function IOHIDValueGetTypeID: CFTypeID; cdecl; external;
function IOHIDValueCreateWithIntegerValue(allocator: CFAllocatorRef; element: IOHIDElementRef; timeStamp: cuint64; value: CFIndex): IOHIDValueRef; cdecl; external;
function IOHIDValueCreateWithBytes(allocator: CFAllocatorRef; element: IOHIDElementRef; timeStamp: cuint64; bytes: pbyte; length: CFIndex): IOHIDValueRef; cdecl; external;
function IOHIDValueCreateWithBytesNoCopy(allocator: CFAllocatorRef; element: IOHIDElementRef; timeStamp: cuint64; bytes: pbyte; length: CFIndex): IOHIDValueRef; cdecl; external;
function IOHIDValueGetElement(value: IOHIDValueRef): IOHIDElementRef; cdecl; external;
function IOHIDValueGetTimeStamp(value: IOHIDValueRef): cuint64; cdecl; external;
function IOHIDValueGetLength(value: IOHIDValueRef): CFIndex; cdecl; external;
function IOHIDValueGetBytePtr(value: IOHIDValueRef): pbyte; cdecl; external;
function IOHIDValueGetIntegerValue(value: IOHIDValueRef): CFIndex; cdecl; external;
function IOHIDValueGetScaledValue(value: IOHIDValueRef; type_: IOHIDValueScaleType): double_t; cdecl; external;
{$endif}

{$endif}
