{ Parsed from IOKit.framework IOTypes.h }

{$ifndef __IOKIT_IOTYPES_H}

{$ifndef IOKIT}
{$ifdef TYPES}
const
  IOKIT = 1;
{$endif}

{$endif}

{$ifdef TYPES}
type
  IOOptionBits = UInt32;
  IOOptionBitsPtr = ^IOOptionBits;
  IOFixed = SInt32;
  IOFixedPtr = ^IOFixed;
  IOVersion = UInt32;
  IOVersionPtr = ^IOVersion;
  IOItemCount = UInt32;
  IOItemCountPtr = ^IOItemCount;
  IOCacheMode = UInt32;
  IOCacheModePtr = ^IOCacheMode;
  IOByteCount32 = UInt32;
  IOByteCount32Ptr = ^IOByteCount32;
  IOByteCount64 = UInt64;
  IOByteCount64Ptr = ^IOByteCount64;
  IOPhysicalAddress32 = UInt32;
  IOPhysicalAddress32Ptr = ^IOPhysicalAddress32;
  IOPhysicalAddress64 = UInt64;
  IOPhysicalAddress64Ptr = ^IOPhysicalAddress64;
  IOPhysicalLength32 = UInt32;
  IOPhysicalLength32Ptr = ^IOPhysicalLength32;
  IOPhysicalLength64 = UInt64;
  IOPhysicalLength64Ptr = ^IOPhysicalLength64;
{$endif}

{$if  not (defined(__arm__)) and not (defined(__i386__))}
{$ifdef TYPES}
type
  IOVirtualAddress = mach_vm_address_t;
  IOVirtualAddressPtr = ^IOVirtualAddress;
{$endif}

{$else}
{$ifdef TYPES}
type
  IOVirtualAddress = vm_address_t;
  IOVirtualAddressPtr = ^IOVirtualAddress;
{$endif}

{$endif}

{$if  not (defined(__arm__)) and not (defined(__i386__)) and not ((defined(__x86_64__)) and not (defined(KERNEL)))}
{$ifdef TYPES}
type
  IOByteCount = IOByteCount64;
  IOByteCountPtr = ^IOByteCount;
{$endif}

{$else}
{$ifdef TYPES}
type
  IOByteCount = IOByteCount32;
  IOByteCountPtr = ^IOByteCount;
{$endif}

{$endif}
{$ifdef TYPES}
type
  IOLogicalAddress = IOVirtualAddress;
  IOLogicalAddressPtr = ^IOLogicalAddress;
{$endif}

{$if  not (defined(__arm__)) and not (defined(__i386__)) and not ((defined(__x86_64__)) and not (defined(KERNEL)))}
{$ifdef TYPES}
type
  IOPhysicalAddress = IOPhysicalAddress64;
  IOPhysicalAddressPtr = ^IOPhysicalAddress;
  IOPhysicalLength = IOPhysicalLength64;
  IOPhysicalLengthPtr = ^IOPhysicalLength;

const
  IOPhysSize = 64;
{$endif}

{$else}
{$ifdef TYPES}
type
  IOPhysicalAddress = IOPhysicalAddress32;
  IOPhysicalAddressPtr = ^IOPhysicalAddress;
  IOPhysicalLength = IOPhysicalLength32;
  IOPhysicalLengthPtr = ^IOPhysicalLength;

const
  IOPhysSize = 32;
{$endif}

{$endif}
{$ifdef TYPES}
type
  IOPhysicalRange = record
    address: IOPhysicalAddress;
    length: IOByteCount;
  end;
type
  IOPhysicalRangePtr = ^IOPhysicalRange;
type
  IOVirtualRange = record
    address: IOVirtualAddress;
    length: IOByteCount;
  end;
type
  IOVirtualRangePtr = ^IOVirtualRange;
{$endif}

{$if  not (defined(__arm__)) and not (defined(__i386__))}
{$ifdef TYPES}
type
  IOAddressRange = IOVirtualRange;
  IOAddressRangePtr = ^IOAddressRange;
{$endif}

{$else}
{$ifdef TYPES}
type
  IOAddressRange = record
    address: mach_vm_address_t;
    length: mach_vm_size_t;
  end;
type
  IOAddressRangePtr = ^IOAddressRange;
{$endif}

{$endif}
{$ifdef TYPES}
type
  IONamedValue = record
    value: cint;
    name: PChar;
  end;
type
  IONamedValuePtr = ^IONamedValue;

type
  IOAlignment = cuint;
  IOAlignmentPtr = ^IOAlignment;
{$endif}

{$ifndef MACH_KERNEL}

{$ifndef __IOKIT_PORTS_DEFINED__}
{$ifdef TYPES}
type
  io_object_t = mach_port_t;
  io_object_tPtr = ^io_object_t;
{$endif}

{$endif}
{$ifdef TYPES}
type
  io_connect_t = io_object_t;
  io_connect_tPtr = ^io_connect_t;
  io_enumerator_t = io_object_t;
  io_enumerator_tPtr = ^io_enumerator_t;
  io_iterator_t = io_object_t;
  io_iterator_tPtr = ^io_iterator_t;
  io_registry_entry_t = io_object_t;
  io_registry_entry_tPtr = ^io_registry_entry_t;
  io_service_t = io_object_t;
  io_service_tPtr = ^io_service_t;
{$endif}

{$endif}
{$ifdef TYPES}
const
  kIODefaultMemoryType = 0;
const
  kIODefaultCache = 0;
  kIOInhibitCache = 1;
  kIOWriteThruCache = 2;
  kIOCopybackCache = 3;
  kIOWriteCombineCache = 4;
  kIOCopybackInnerCache = 5;
const
  kIOMapAnywhere = $00000001;
  kIOMapCacheMask = $00000700;
  kIOMapCacheShift = 8;
  kIOMapDefaultCache = kIODefaultCache shl kIOMapCacheShift;
  kIOMapInhibitCache = kIOInhibitCache shl kIOMapCacheShift;
  kIOMapWriteThruCache = kIOWriteThruCache shl kIOMapCacheShift;
  kIOMapCopybackCache = kIOCopybackCache shl kIOMapCacheShift;
  kIOMapWriteCombineCache = kIOWriteCombineCache shl kIOMapCacheShift;
  kIOMapCopybackInnerCache = kIOCopybackInnerCache shl kIOMapCacheShift;
  kIOMapUserOptionsMask = $00000fff;
  kIOMapReadOnly = $00001000;
  kIOMapStatic = $01000000;
  kIOMapReference = $02000000;
  kIOMapUnique = $04000000;
  kIOMapPrefault = $10000000;
  kIOMapOverwrite = $20000000;
const
  kNanosecondScale = 1;
  kMicrosecondScale = 1000;
  kMillisecondScale = 1000 * 1000;
  kSecondScale = 1000 * 1000 * 1000;
  kTickScale = kSecondScale / 100;
const
  kIOConnectMethodVarOutputSize = -3;

type
  IODeviceNumber = cuint;
  IODeviceNumberPtr = ^IODeviceNumber;
{$endif}

{$endif}
