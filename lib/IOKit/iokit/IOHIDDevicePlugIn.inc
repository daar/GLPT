{ Parsed from IOKit.framework IOHIDDevicePlugIn.h }

{$ifndef _IOKIT_HID_IOHIDDEVICEPLUGIN_H}

{$ifdef TYPES}
type
  IOHIDDeviceDeviceInterface = record
    {$ifdef IOHID_DEVICE_DEVICE_FUNCS_V1 }
    {$else}
    open: function (self_: pointer; options: IOOptionBits): IOReturn; cdecl;
    close: function (self_: pointer; options: IOOptionBits): IOReturn; cdecl;
    getProperty: function (self_: pointer; key: CFStringRef; pProperty: CFTypeRefPtr): IOReturn; cdecl;
    setProperty: function (self_: pointer; key: CFStringRef; property_: CFTypeRef): IOReturn; cdecl;
    getAsyncEventSource: function (self_: pointer; pSource: CFTypeRefPtr): IOReturn; cdecl;
    copyMatchingElements: function (self_: pointer; matchingDict: CFDictionaryRef; pElements: CFArrayRefPtr; options: IOOptionBits): IOReturn; cdecl;
    setValue: function (self_: pointer; element: IOHIDElementRef; value: IOHIDValueRef; timeout: cuint32; callback: IOHIDValueCallback; context: pointer; options: IOOptionBits): IOReturn; cdecl;
    getValue: function (self_: pointer; element: IOHIDElementRef; pValue: IOHIDValueRef; timeout: cuint32; callback: IOHIDValueCallback; context: pointer; options: IOOptionBits): IOReturn; cdecl;
    setInputReportCallback: function (self_: pointer; report: pbyte; reportLength: CFIndex; callback: IOHIDReportCallback; context: pointer; options: IOOptionBits): IOReturn; cdecl;
    setReport: function (self_: pointer; reportType: IOHIDReportType; reportID: cuint32; report: pbyte; reportLength: CFIndex; timeout: cuint32; callback: IOHIDReportCallback; context: pointer; options: IOOptionBits): IOReturn; cdecl;
    getReport: function (self_: pointer; reportType: IOHIDReportType; reportID: cuint32; report: pbyte; pReportLength: CFIndexPtr; timeout: cuint32; callback: IOHIDReportCallback; context: pointer; options: IOOptionBits): IOReturn; cdecl;
    {$endif}
  end;
type
  IOHIDDeviceDeviceInterfacePtr = ^IOHIDDeviceDeviceInterface;
type
  IOHIDDeviceTimeStampedDeviceInterface = record
    setInputReportWithTimeStampCallback: function (self_: pointer; report: pbyte; reportLength: CFIndex; callback: IOHIDReportWithTimeStampCallback; context: pointer; options: IOOptionBits): IOReturn; cdecl;
  end;
type
  IOHIDDeviceTimeStampedDeviceInterfacePtr = ^IOHIDDeviceTimeStampedDeviceInterface;
type
  IOHIDDeviceQueueInterface = record
    getAsyncEventSource: function (self_: pointer; pSource: CFTypeRefPtr): IOReturn; cdecl;
    setDepth: function (self_: pointer; depth: cuint32; options: IOOptionBits): IOReturn; cdecl;
    getDepth: function (self_: pointer; pDepth: pcuint32): IOReturn; cdecl;
    addElement: function (self_: pointer; element: IOHIDElementRef; options: IOOptionBits): IOReturn; cdecl;
    removeElement: function (self_: pointer; element: IOHIDElementRef; options: IOOptionBits): IOReturn; cdecl;
    containsElement: function (self_: pointer; element: IOHIDElementRef; pValue: pboolean; options: IOOptionBits): IOReturn; cdecl;
    start: function (self_: pointer; options: IOOptionBits): IOReturn; cdecl;
    stop: function (self_: pointer; options: IOOptionBits): IOReturn; cdecl;
    setValueAvailableCallback: function (self_: pointer; callback: IOHIDCallback; context: pointer): IOReturn; cdecl;
    copyNextValue: function (self_: pointer; pValue: IOHIDValueRef; timeout: cuint32; options: IOOptionBits): IOReturn; cdecl;
  end;
type
  IOHIDDeviceQueueInterfacePtr = ^IOHIDDeviceQueueInterface;
type
  IOHIDDeviceTransactionInterface = record
    getAsyncEventSource: function (self_: pointer; pSource: CFTypeRefPtr): IOReturn; cdecl;
    setDirection: function (self_: pointer; direction: IOHIDTransactionDirectionType; options: IOOptionBits): IOReturn; cdecl;
    getDirection: function (self_: pointer; pDirection: IOHIDTransactionDirectionTypePtr): IOReturn; cdecl;
    addElement: function (self_: pointer; element: IOHIDElementRef; options: IOOptionBits): IOReturn; cdecl;
    removeElement: function (self_: pointer; element: IOHIDElementRef; options: IOOptionBits): IOReturn; cdecl;
    containsElement: function (self_: pointer; element: IOHIDElementRef; pValue: pboolean; options: IOOptionBits): IOReturn; cdecl;
    setValue: function (self_: pointer; element: IOHIDElementRef; value: IOHIDValueRef; options: IOOptionBits): IOReturn; cdecl;
    getValue: function (self_: pointer; element: IOHIDElementRef; pValue: IOHIDValueRef; options: IOOptionBits): IOReturn; cdecl;
    commit: function (self_: pointer; timeout: cuint32; callback: IOHIDCallback; context: pointer; options: IOOptionBits): IOReturn; cdecl;
    clear: function (self_: pointer; options: IOOptionBits): IOReturn; cdecl;
  end;
type
  IOHIDDeviceTransactionInterfacePtr = ^IOHIDDeviceTransactionInterface;
{$endif}

{$endif}
