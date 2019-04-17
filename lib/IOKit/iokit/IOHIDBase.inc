{ Parsed from IOKit.framework IOHIDBase.h }

{$ifndef _IOKIT_HID_IOHIDBASE_H_}
{$ifdef TYPES}
type
  IOHIDDeviceRef = OpaqueType;
  IOHIDElementRef = OpaqueType;
  IOHIDValueRef = OpaqueType;
  IOHIDTransactionDirectionType = cuint32;
  IOHIDTransactionDirectionTypePtr = ^IOHIDTransactionDirectionType;

const
  kIOHIDTransactionDirectionTypeInput = 0;
  kIOHIDTransactionDirectionTypeOutput = 1;

type
  IOHIDCallback = procedure (context: pointer; result_: IOReturn; sender: pointer); cdecl;
  IOHIDReportCallback = procedure (context: pointer; result_: IOReturn; sender: pointer; type_: IOHIDReportType; reportID: cuint32; report: pbyte; reportLength: CFIndex); cdecl;
  IOHIDReportWithTimeStampCallback = procedure (context: pointer; result_: IOReturn; sender: pointer; type_: IOHIDReportType; reportID: cuint32; report: pbyte; reportLength: CFIndex; timeStamp: cuint64); cdecl;
  IOHIDValueCallback = procedure (context: pointer; result_: IOReturn; sender: pointer; value: IOHIDValueRef); cdecl;
  IOHIDValueMultipleCallback = procedure (context: pointer; result_: IOReturn; sender: pointer; multiple: CFDictionaryRef); cdecl;
  IOHIDDeviceCallback = procedure (context: pointer; result_: IOReturn; sender: pointer; device: IOHIDDeviceRef); cdecl;
{$endif}

{$endif}
