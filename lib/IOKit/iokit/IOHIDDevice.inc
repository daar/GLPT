{ Parsed from IOKit.framework IOHIDDevice.h }

{$ifndef _IOKIT_HID_IOHIDDEVICE_USER_H}
{$ifdef FUNCTIONS}
function IOHIDDeviceGetTypeID: CFTypeID; cdecl; external;
function IOHIDDeviceCreate(allocator: CFAllocatorRef; service: io_service_t): IOHIDDeviceRef; cdecl; external;
function IOHIDDeviceGetService(device: IOHIDDeviceRef): io_service_t; cdecl; external;
function IOHIDDeviceOpen(device: IOHIDDeviceRef; options: IOOptionBits): IOReturn; cdecl; external;
function IOHIDDeviceClose(device: IOHIDDeviceRef; options: IOOptionBits): IOReturn; cdecl; external;
function IOHIDDeviceConformsTo(device: IOHIDDeviceRef; usagePage: cuint32; usage: cuint32): Boolean; cdecl; external;
function IOHIDDeviceGetProperty(device: IOHIDDeviceRef; key: CFStringRef): CFTypeRef; cdecl; external;
function IOHIDDeviceSetProperty(device: IOHIDDeviceRef; key: CFStringRef; property_: CFTypeRef): Boolean; cdecl; external;
function IOHIDDeviceCopyMatchingElements(device: IOHIDDeviceRef; matching: CFDictionaryRef; options: IOOptionBits): CFArrayRef; cdecl; external;
procedure IOHIDDeviceScheduleWithRunLoop(device: IOHIDDeviceRef; runLoop: CFRunLoopRef; runLoopMode: CFStringRef); cdecl; external;
procedure IOHIDDeviceUnscheduleFromRunLoop(device: IOHIDDeviceRef; runLoop: CFRunLoopRef; runLoopMode: CFStringRef); cdecl; external;
procedure IOHIDDeviceRegisterRemovalCallback(device: IOHIDDeviceRef; callback: IOHIDCallback; context: pointer); cdecl; external;
procedure IOHIDDeviceRegisterInputValueCallback(device: IOHIDDeviceRef; callback: IOHIDValueCallback; context: pointer); cdecl; external;
procedure IOHIDDeviceRegisterInputReportCallback(device: IOHIDDeviceRef; report: pbyte; reportLength: CFIndex; callback: IOHIDReportCallback; context: pointer); cdecl; external;
procedure IOHIDDeviceRegisterInputReportWithTimeStampCallback(device: IOHIDDeviceRef; report: pbyte; reportLength: CFIndex; callback: IOHIDReportWithTimeStampCallback; context: pointer); cdecl; external;
procedure IOHIDDeviceSetInputValueMatching(device: IOHIDDeviceRef; matching: CFDictionaryRef); cdecl; external;
procedure IOHIDDeviceSetInputValueMatchingMultiple(device: IOHIDDeviceRef; multiple: CFArrayRef); cdecl; external;
function IOHIDDeviceSetValue(device: IOHIDDeviceRef; element: IOHIDElementRef; value: IOHIDValueRef): IOReturn; cdecl; external;
function IOHIDDeviceSetValueMultiple(device: IOHIDDeviceRef; multiple: CFDictionaryRef): IOReturn; cdecl; external;
function IOHIDDeviceSetValueWithCallback(device: IOHIDDeviceRef; element: IOHIDElementRef; value: IOHIDValueRef; timeout: CFTimeInterval; callback: IOHIDValueCallback; context: pointer): IOReturn; cdecl; external;
function IOHIDDeviceSetValueMultipleWithCallback(device: IOHIDDeviceRef; multiple: CFDictionaryRef; timeout: CFTimeInterval; callback: IOHIDValueMultipleCallback; context: pointer): IOReturn; cdecl; external;
function IOHIDDeviceGetValue(device: IOHIDDeviceRef; element: IOHIDElementRef; pValue: IOHIDValueRef): IOReturn; cdecl; external;
function IOHIDDeviceCopyValueMultiple(device: IOHIDDeviceRef; elements: CFArrayRef; pMultiple: CFDictionaryRefPtr): IOReturn; cdecl; external;
function IOHIDDeviceGetValueWithCallback(device: IOHIDDeviceRef; element: IOHIDElementRef; pValue: IOHIDValueRef; timeout: CFTimeInterval; callback: IOHIDValueCallback; context: pointer): IOReturn; cdecl; external;
function IOHIDDeviceCopyValueMultipleWithCallback(device: IOHIDDeviceRef; elements: CFArrayRef; pMultiple: CFDictionaryRefPtr; timeout: CFTimeInterval; callback: IOHIDValueMultipleCallback; context: pointer): IOReturn; cdecl; external;
function IOHIDDeviceSetReport(device: IOHIDDeviceRef; reportType: IOHIDReportType; reportID: CFIndex; report: pbyte; reportLength: CFIndex): IOReturn; cdecl; external;
function IOHIDDeviceSetReportWithCallback(device: IOHIDDeviceRef; reportType: IOHIDReportType; reportID: CFIndex; report: pbyte; reportLength: CFIndex; timeout: CFTimeInterval; callback: IOHIDReportCallback; context: pointer): IOReturn; cdecl; external;
function IOHIDDeviceGetReport(device: IOHIDDeviceRef; reportType: IOHIDReportType; reportID: CFIndex; report: pbyte; pReportLength: CFIndexPtr): IOReturn; cdecl; external;
function IOHIDDeviceGetReportWithCallback(device: IOHIDDeviceRef; reportType: IOHIDReportType; reportID: CFIndex; report: pbyte; pReportLength: CFIndexPtr; timeout: CFTimeInterval; callback: IOHIDReportCallback; context: pointer): IOReturn; cdecl; external;
{$endif}

{$endif}
