{ Parsed from IOKit.framework IOReturn.h }

{$ifndef __IOKIT_IORETURN_H}

{$ifdef TYPES}
type
  IOReturn = LongInt;
  IOReturnPtr = ^IOReturn;
{$endif}

{$ifdef TYPES}
const
  kIOReturnSuccess = KERN_SUCCESS;
{$endif}

{$endif}
