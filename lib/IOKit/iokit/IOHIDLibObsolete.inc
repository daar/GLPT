{ Parsed from IOKit.framework IOHIDLibObsolete.h }

{$ifndef _IOKIT_HID_IOHIDLIBOBSOLETE_H_}

{$ifdef TYPES}
type
  IOHIDEventStruct = record
    type_: IOHIDElementType;
    elementCookie: IOHIDElementCookie;
    value: cint32;
    timestamp: AbsoluteTime;
    longValueSize: cuint32;
    longValue: pointer;
  end;
type
  IOHIDEventStructPtr = ^IOHIDEventStruct;

type
  IOHIDCallbackFunction = procedure (target: pointer; result_: IOReturn; refcon: pointer; sender: pointer); cdecl;
  IOHIDElementCallbackFunction = procedure (target: pointer; result_: IOReturn; refcon: pointer; sender: pointer; elementCookie: IOHIDElementCookie); cdecl;
  IOHIDReportCallbackFunction = procedure (target: pointer; result_: IOReturn; refcon: pointer; sender: pointer; bufferSize: cuint32); cdecl;

type
  IOHIDDeviceInterface = record
  end;
type
  IOHIDDeviceInterfacePtr = ^IOHIDDeviceInterface;
type
  IOHIDDeviceInterface121 = record
  end;
type
  IOHIDDeviceInterface121Ptr = ^IOHIDDeviceInterface121;
type
  IOHIDDeviceInterface122 = record
  end;
type
  IOHIDDeviceInterface122Ptr = ^IOHIDDeviceInterface122;
type
  IOHIDQueueInterface = record
  end;
type
  IOHIDQueueInterfacePtr = ^IOHIDQueueInterface;
type
  IOHIDOutputTransactionInterface = record
  end;
type
  IOHIDOutputTransactionInterfacePtr = ^IOHIDOutputTransactionInterface;
{$endif}

{$if defined(__NULL_MACRO__)}
{$ifdef TYPES}
type
  IOHIDDeviceInterface = record
    createAsyncEventSource: function (self_: pointer; source: CFRunLoopSourceRefPtr): IOReturn; cdecl;
    getAsyncEventSource: function (self_: pointer): CFRunLoopSourceRef; cdecl;
    createAsyncPort: function (self_: pointer; port: mach_port_tPtr): IOReturn; cdecl;
    getAsyncPort: function (self_: pointer): mach_port_t; cdecl;
    open: function (self_: pointer; flags: cuint32): IOReturn; cdecl;
    close: function (self_: pointer): IOReturn; cdecl;
    setRemovalCallback: function (self_: pointer; removalCallback: IOHIDCallbackFunction; removalTarget: pointer; removalRefcon: pointer): IOReturn; cdecl;
    getElementValue: function (self_: pointer; elementCookie: IOHIDElementCookie; valueEvent: IOHIDEventStructPtr): IOReturn; cdecl;
    setElementValue: function (self_: pointer; elementCookie: IOHIDElementCookie; valueEvent: IOHIDEventStructPtr; timeoutMS: cuint32; callback: IOHIDElementCallbackFunction; callbackTarget: pointer; callbackRefcon: pointer): IOReturn; cdecl;
    queryElementValue: function (self_: pointer; elementCookie: IOHIDElementCookie; valueEvent: IOHIDEventStructPtr; timeoutMS: cuint32; callback: IOHIDElementCallbackFunction; callbackTarget: pointer; callbackRefcon: pointer): IOReturn; cdecl;
    startAllQueues: function (self_: pointer): IOReturn; cdecl;
    stopAllQueues: function (self_: pointer): IOReturn; cdecl;
    allocQueue: function (self_: pointer): IOHIDQueueInterfacePtr; cdecl;
    allocOutputTransaction: function (self_: pointer): IOHIDOutputTransactionInterfacePtr; cdecl;
  end;
type
  IOHIDDeviceInterfacePtr = ^IOHIDDeviceInterface;
type
  IOHIDDeviceInterface121 = record
    IOHIDDEVICEINTERFACE_FUNCS_100: IUNKNOWN_C_GUTS;;
    setReport: function (self_: pointer; reportType: IOHIDReportType; reportID: cuint32; reportBuffer: pointer; reportBufferSize: cuint32; timeoutMS: cuint32; callback: IOHIDReportCallbackFunction; callbackTarget: pointer; callbackRefcon: pointer): IOReturn; cdecl;
    getReport: function (self_: pointer; reportType: IOHIDReportType; reportID: cuint32; reportBuffer: pointer; reportBufferSize: pcuint32; timeoutMS: cuint32; callback: IOHIDReportCallbackFunction; callbackTarget: pointer; callbackRefcon: pointer): IOReturn; cdecl;
  end;
type
  IOHIDDeviceInterface121Ptr = ^IOHIDDeviceInterface121;
type
  IOHIDDeviceInterface122 = record
    IOHIDDEVICEINTERFACE_FUNCS_100: IUNKNOWN_C_GUTS;;
    copyMatchingElements: function (self_: pointer; matchingDict: CFDictionaryRef; elements: CFArrayRefPtr): IOReturn; cdecl;
    setInterruptReportHandlerCallback: function (self_: pointer; reportBuffer: pointer; reportBufferSize: cuint32; callback: IOHIDReportCallbackFunction; callbackTarget: pointer; callbackRefcon: pointer): IOReturn; cdecl;
  end;
type
  IOHIDDeviceInterface122Ptr = ^IOHIDDeviceInterface122;
type
  IOHIDQueueInterface = record
    createAsyncEventSource: function (self_: pointer; source: CFRunLoopSourceRefPtr): IOReturn; cdecl;
    getAsyncEventSource: function (self_: pointer): CFRunLoopSourceRef; cdecl;
    createAsyncPort: function (self_: pointer; port: mach_port_tPtr): IOReturn; cdecl;
    getAsyncPort: function (self_: pointer): mach_port_t; cdecl;
    create_: function (self_: pointer; flags: cuint32; depth: cuint32): IOReturn; cdecl;
    dispose_: function (self_: pointer): IOReturn; cdecl;
    addElement: function (self_: pointer; elementCookie: IOHIDElementCookie; flags: cuint32): IOReturn; cdecl;
    removeElement: function (self_: pointer; elementCookie: IOHIDElementCookie): IOReturn; cdecl;
    hasElement: function (self_: pointer; elementCookie: IOHIDElementCookie): Boolean; cdecl;
    start: function (self_: pointer): IOReturn; cdecl;
    stop: function (self_: pointer): IOReturn; cdecl;
    getNextEvent: function (self_: pointer; event: IOHIDEventStructPtr; maxTime: AbsoluteTime; timeoutMS: cuint32): IOReturn; cdecl;
    setEventCallout: function (self_: pointer; callback: IOHIDCallbackFunction; callbackTarget: pointer; callbackRefcon: pointer): IOReturn; cdecl;
    getEventCallout: function (self_: pointer; outCallback: IOHIDCallbackFunction; outCallbackTarget: pointer; outCallbackRefcon: pointer): IOReturn; cdecl;
  end;
type
  IOHIDQueueInterfacePtr = ^IOHIDQueueInterface;
type
  IOHIDOutputTransactionInterface = record
    createAsyncEventSource: function (self_: pointer; source: CFRunLoopSourceRefPtr): IOReturn; cdecl;
    getAsyncEventSource: function (self_: pointer): CFRunLoopSourceRef; cdecl;
    createAsyncPort: function (self_: pointer; port: mach_port_tPtr): IOReturn; cdecl;
    getAsyncPort: function (self_: pointer): mach_port_t; cdecl;
    create_: function (self_: pointer): IOReturn; cdecl;
    dispose_: function (self_: pointer): IOReturn; cdecl;
    addElement: function (self_: pointer; elementCookie: IOHIDElementCookie): IOReturn; cdecl;
    removeElement: function (self_: pointer; elementCookie: IOHIDElementCookie): IOReturn; cdecl;
    hasElement: function (self_: pointer; elementCookie: IOHIDElementCookie): Boolean; cdecl;
    setElementDefault: function (self_: pointer; elementCookie: IOHIDElementCookie; valueEvent: IOHIDEventStructPtr): IOReturn; cdecl;
    getElementDefault: function (self_: pointer; elementCookie: IOHIDElementCookie; outValueEvent: IOHIDEventStructPtr): IOReturn; cdecl;
    setElementValue: function (self_: pointer; elementCookie: IOHIDElementCookie; valueEvent: IOHIDEventStructPtr): IOReturn; cdecl;
    getElementValue: function (self_: pointer; elementCookie: IOHIDElementCookie; outValueEvent: IOHIDEventStructPtr): IOReturn; cdecl;
    commit: function (self_: pointer; timeoutMS: cuint32; callback: IOHIDCallbackFunction; callbackTarget: pointer; callbackRefcon: pointer): IOReturn; cdecl;
    clear: function (self_: pointer): IOReturn; cdecl;
  end;
type
  IOHIDOutputTransactionInterfacePtr = ^IOHIDOutputTransactionInterface;
{$endif}

{$endif}
{$endif}
