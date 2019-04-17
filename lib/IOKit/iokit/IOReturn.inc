{ Parsed from IOKit.framework IOReturn.h }

{$ifndef __IOKIT_IORETURN_H}

{$ifdef TYPES}
type
  IOReturn = kern_return_t;
  IOReturnPtr = ^IOReturn;
{$endif}

{$ifdef TYPES}
const
  kIOReturnSuccess = KERN_SUCCESS;
{$endif}

{$endif}
