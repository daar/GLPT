{ Parsed from IOKit.framework IOHIDElement.h }

{$ifndef _IOKIT_HID_IOHIDELEMENT_USER_H}
{$ifdef FUNCTIONS}
function IOHIDElementGetTypeID: CFTypeID; cdecl; external;
function IOHIDElementCreateWithDictionary(allocator: CFAllocatorRef; dictionary: CFDictionaryRef): IOHIDElementRef; cdecl; external;
function IOHIDElementGetDevice(element: IOHIDElementRef): IOHIDDeviceRef; cdecl; external;
function IOHIDElementGetParent(element: IOHIDElementRef): IOHIDElementRef; cdecl; external;
function IOHIDElementGetChildren(element: IOHIDElementRef): CFArrayRef; cdecl; external;
procedure IOHIDElementAttach(element: IOHIDElementRef; toAttach: IOHIDElementRef); cdecl; external;
procedure IOHIDElementDetach(element: IOHIDElementRef; toDetach: IOHIDElementRef); cdecl; external;
function IOHIDElementCopyAttached(element: IOHIDElementRef): CFArrayRef; cdecl; external;
function IOHIDElementGetCookie(element: IOHIDElementRef): IOHIDElementCookie; cdecl; external;
function IOHIDElementGetType(element: IOHIDElementRef): IOHIDElementType; cdecl; external;
function IOHIDElementGetCollectionType(element: IOHIDElementRef): IOHIDElementCollectionType; cdecl; external;
function IOHIDElementGetUsagePage(element: IOHIDElementRef): cuint32; cdecl; external;
function IOHIDElementGetUsage(element: IOHIDElementRef): cuint32; cdecl; external;
function IOHIDElementIsVirtual(element: IOHIDElementRef): Boolean; cdecl; external;
function IOHIDElementIsRelative(element: IOHIDElementRef): Boolean; cdecl; external;
function IOHIDElementIsWrapping(element: IOHIDElementRef): Boolean; cdecl; external;
function IOHIDElementIsArray(element: IOHIDElementRef): Boolean; cdecl; external;
function IOHIDElementIsNonLinear(element: IOHIDElementRef): Boolean; cdecl; external;
function IOHIDElementHasPreferredState(element: IOHIDElementRef): Boolean; cdecl; external;
function IOHIDElementHasNullState(element: IOHIDElementRef): Boolean; cdecl; external;
function IOHIDElementGetName(element: IOHIDElementRef): CFStringRef; cdecl; external;
function IOHIDElementGetReportID(element: IOHIDElementRef): cuint32; cdecl; external;
function IOHIDElementGetReportSize(element: IOHIDElementRef): cuint32; cdecl; external;
function IOHIDElementGetReportCount(element: IOHIDElementRef): cuint32; cdecl; external;
function IOHIDElementGetUnit(element: IOHIDElementRef): cuint32; cdecl; external;
function IOHIDElementGetUnitExponent(element: IOHIDElementRef): cuint32; cdecl; external;
function IOHIDElementGetLogicalMin(element: IOHIDElementRef): CFIndex; cdecl; external;
function IOHIDElementGetLogicalMax(element: IOHIDElementRef): CFIndex; cdecl; external;
function IOHIDElementGetPhysicalMin(element: IOHIDElementRef): CFIndex; cdecl; external;
function IOHIDElementGetPhysicalMax(element: IOHIDElementRef): CFIndex; cdecl; external;
function IOHIDElementGetProperty(element: IOHIDElementRef; key: CFStringRef): CFTypeRef; cdecl; external;
function IOHIDElementSetProperty(element: IOHIDElementRef; key: CFStringRef; property_: CFTypeRef): Boolean; cdecl; external;
{$endif}

{$endif}
