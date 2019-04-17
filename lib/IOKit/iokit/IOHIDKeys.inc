{ Parsed from IOKit.framework IOHIDKeys.h }

{$ifndef _IOKIT_HID_IOHIDKEYS_H_}
{$ifdef TYPES}
const
  kIOHIDDeviceKey = 'IOHIDDevice';
  kIOHIDTransportKey = 'Transport';
  kIOHIDVendorIDKey = 'VendorID';
  kIOHIDVendorIDSourceKey = 'VendorIDSource';
  kIOHIDProductIDKey = 'ProductID';
  kIOHIDVersionNumberKey = 'VersionNumber';
  kIOHIDManufacturerKey = 'Manufacturer';
  kIOHIDProductKey = 'Product';
  kIOHIDSerialNumberKey = 'SerialNumber';
  kIOHIDCountryCodeKey = 'CountryCode';
  kIOHIDStandardTypeKey = 'StandardType';
  kIOHIDLocationIDKey = 'LocationID';
  kIOHIDDeviceUsageKey = 'DeviceUsage';
  kIOHIDDeviceUsagePageKey = 'DeviceUsagePage';
  kIOHIDDeviceUsagePairsKey = 'DeviceUsagePairs';
  kIOHIDPrimaryUsageKey = 'PrimaryUsage';
  kIOHIDPrimaryUsagePageKey = 'PrimaryUsagePage';
  kIOHIDMaxInputReportSizeKey = 'MaxInputReportSize';
  kIOHIDMaxOutputReportSizeKey = 'MaxOutputReportSize';
  kIOHIDMaxFeatureReportSizeKey = 'MaxFeatureReportSize';
  kIOHIDReportIntervalKey = 'ReportInterval';
  kIOHIDSampleIntervalKey = 'SampleInterval';
  kIOHIDBatchIntervalKey = 'BatchInterval';
  kIOHIDRequestTimeoutKey = 'RequestTimeout';
  kIOHIDReportDescriptorKey = 'ReportDescriptor';
  kIOHIDResetKey = 'Reset';
  kIOHIDKeyboardLanguageKey = 'KeyboardLanguage';
  kIOHIDAltHandlerIdKey = 'alt_handler_id';
  kIOHIDBuiltInKey = 'Built-In';
  kIOHIDDisplayIntegratedKey = 'DisplayIntegrated';
  kIOHIDProductIDMaskKey = 'ProductIDMask';
  kIOHIDProductIDArrayKey = 'ProductIDArray';
  kIOHIDPowerOnDelayNSKey = 'HIDPowerOnDelayNS';
  kIOHIDCategoryKey = 'Category';
  kIOHIDMaxResponseLatencyKey = 'MaxResponseLatency';
  kIOHIDUniqueIDKey = 'UniqueID';
  kIOHIDPhysicalDeviceUniqueIDKey = 'PhysicalDeviceUniqueID';
  kIOHIDTransportUSBValue = 'USB';
  kIOHIDTransportBluetoothValue = 'Bluetooth';
  kIOHIDTransportBluetoothLowEnergyValue = 'BluetoothLowEnergy';
  kIOHIDTransportAIDBValue = 'AIDB';
  kIOHIDTransportI2CValue = 'I2C';
  kIOHIDTransportSPIValue = 'SPI';
  kIOHIDTransportSerialValue = 'Serial';
  kIOHIDTransportIAPValue = 'iAP';
  kIOHIDTransportAirPlayValue = 'AirPlay';
  kIOHIDTransportSPUValue = 'SPU';
  kIOHIDCategoryAutomotiveValue = 'Automotive';
  kIOHIDElementKey = 'Elements';
  kIOHIDElementCookieKey = 'ElementCookie';
  kIOHIDElementCollectionTypeKey = 'CollectionType';
  kIOHIDElementUsageKey = 'Usage';
  kIOHIDElementUsagePageKey = 'UsagePage';
  kIOHIDElementMinKey = 'Min';
  kIOHIDElementMaxKey = 'Max';
  kIOHIDElementScaledMinKey = 'ScaledMin';
  kIOHIDElementScaledMaxKey = 'ScaledMax';
  kIOHIDElementSizeKey = 'Size';
  kIOHIDElementReportSizeKey = 'ReportSize';
  kIOHIDElementReportCountKey = 'ReportCount';
  kIOHIDElementReportIDKey = 'ReportID';
  kIOHIDElementIsArrayKey = 'IsArray';
  kIOHIDElementIsRelativeKey = 'IsRelative';
  kIOHIDElementIsWrappingKey = 'IsWrapping';
  kIOHIDElementIsNonLinearKey = 'IsNonLinear';
  kIOHIDElementHasPreferredStateKey = 'HasPreferredState';
  kIOHIDElementHasNullStateKey = 'HasNullState';
  kIOHIDElementFlagsKey = 'Flags';
  kIOHIDElementUnitKey = 'Unit';
  kIOHIDElementUnitExponentKey = 'UnitExponent';
  kIOHIDElementNameKey = 'Name';
  kIOHIDElementValueLocationKey = 'ValueLocation';
  kIOHIDElementDuplicateIndexKey = 'DuplicateIndex';
  kIOHIDElementParentCollectionKey = 'ParentCollection';
{$endif}

{$ifndef __ppc__}
{$ifdef TYPES}
const
  kIOHIDElementVendorSpecificKey = 'VendorSpecific';
{$endif}

{$else}
{$ifdef TYPES}
const
  kIOHIDElementVendorSpecificKey = 'VendorSpecifc';
{$endif}

{$endif}
{$ifdef TYPES}
const
  kIOHIDElementCookieMinKey = 'ElementCookieMin';
  kIOHIDElementCookieMaxKey = 'ElementCookieMax';
  kIOHIDElementUsageMinKey = 'UsageMin';
  kIOHIDElementUsageMaxKey = 'UsageMax';
  kIOHIDElementCalibrationMinKey = 'CalibrationMin';
  kIOHIDElementCalibrationMaxKey = 'CalibrationMax';
  kIOHIDElementCalibrationSaturationMinKey = 'CalibrationSaturationMin';
  kIOHIDElementCalibrationSaturationMaxKey = 'CalibrationSaturationMax';
  kIOHIDElementCalibrationDeadZoneMinKey = 'CalibrationDeadZoneMin';
  kIOHIDElementCalibrationDeadZoneMaxKey = 'CalibrationDeadZoneMax';
  kIOHIDElementCalibrationGranularityKey = 'CalibrationGranularity';
{$endif}

{$ifdef __LP64__}
{$ifdef TYPES}
type
  IOHIDElementCookie = cuint32;
  IOHIDElementCookiePtr = ^IOHIDElementCookie;
{$endif}

{$else}
{$ifdef TYPES}
type
  IOHIDElementCookie = pointer;
{$endif}

{$endif}
{$ifdef TYPES}
const
  kIOHIDElementTypeInput_Misc = 1;
  kIOHIDElementTypeInput_Button = 2;
  kIOHIDElementTypeInput_Axis = 3;
  kIOHIDElementTypeInput_ScanCodes = 4;
  kIOHIDElementTypeOutput = 129;
  kIOHIDElementTypeFeature = 257;
  kIOHIDElementTypeCollection = 513;

type
  IOHIDElementType = clong;
  IOHIDElementTypePtr = ^IOHIDElementType;

const
  kIOHIDElementCollectionTypePhysical = $00;
  kIOHIDElementCollectionTypeApplication = 0;
  kIOHIDElementCollectionTypeLogical = 1;
  kIOHIDElementCollectionTypeReport = 2;
  kIOHIDElementCollectionTypeNamedArray = 3;
  kIOHIDElementCollectionTypeUsageSwitch = 4;
  kIOHIDElementCollectionTypeUsageModifier = 5;

type
  IOHIDElementCollectionType = clong;
  IOHIDElementCollectionTypePtr = ^IOHIDElementCollectionType;

const
  kIOHIDReportTypeInput = 0;
  kIOHIDReportTypeOutput = 1;
  kIOHIDReportTypeFeature = 2;
  kIOHIDReportTypeCount = 3;

type
  IOHIDReportType = clong;
  IOHIDReportTypePtr = ^IOHIDReportType;

const
  kIOHIDOptionsTypeNone = $00;
  kIOHIDOptionsTypeSeizeDevice = $01;

type
  IOHIDOptionsType = cuint32;
  IOHIDOptionsTypePtr = ^IOHIDOptionsType;

const
  kIOHIDQueueOptionsTypeNone = $00;
  kIOHIDQueueOptionsTypeEnqueueAll = $01;

type
  IOHIDQueueOptionsType = cuint32;
  IOHIDQueueOptionsTypePtr = ^IOHIDQueueOptionsType;

const
  kIOHIDElementFlagsConstantMask = $0001;
  kIOHIDElementFlagsVariableMask = $0002;
  kIOHIDElementFlagsRelativeMask = $0004;
  kIOHIDElementFlagsWrapMask = $0008;
  kIOHIDElementFlagsNonLinearMask = $0010;
  kIOHIDElementFlagsNoPreferredMask = $0020;
  kIOHIDElementFlagsNullStateMask = $0040;
  kIOHIDElementFlagsVolativeMask = $0080;
  kIOHIDElementFlagsBufferedByteMask = $0100;

type
  IOHIDElementFlags = cuint32;
  IOHIDElementFlagsPtr = ^IOHIDElementFlags;

const
  kIOHIDStandardTypeANSI = 0;
  kIOHIDStandardTypeISO = 1;
  kIOHIDStandardTypeJIS = 2;

type
  IOHIDStandardType = cuint32;
  IOHIDStandardTypePtr = ^IOHIDStandardType;

const
  kIOHIDValueScaleTypeCalibrated = 0;
  kIOHIDValueScaleTypePhysical = 1;

type
  IOHIDValueScaleType = cuint32;
  IOHIDValueScaleTypePtr = ^IOHIDValueScaleType;

const
  kIOHIDValueOptionsFlagRelativeSimple = 1 shl 0;
  kIOHIDValueOptionsFlagPrevious = 1 shl 1;

type
  IOHIDValueOptions = cuint32;
  IOHIDValueOptionsPtr = ^IOHIDValueOptions;

const
  kIOHIDDigitizerGestureCharacterStateKey = 'DigitizerCharacterGestureState';
  kIOHIDKeyboardCapsLockDelay = 'CapsLockDelay';
  kIOHIDKeyboardEjectDelay = 'EjectDelay';
  kIOHIDKeyboardPressCountTrackingEnabledKey = 'PressCountTrackingEnabled';
  kIOHIDKeyboardPressCountUsagePairsKey = 'PressCountUsagePairs';
  kIOHIDKeyboardPressCountTriplePressTimeoutKey = 'PressCountTriplePressTimeout';
  kIOHIDKeyboardPressCountDoublePressTimeoutKey = 'PressCountDoublePressTimeout';
  kIOHIDKeyboardLongPressTimeoutKey = 'LongPressTimeout';
  kIOHIDBiometricTapTrackingEnabledKey = 'TapTrackingEnabled';
  kIOHIDBiometricDoubleTapTimeoutKey = 'DoubleTapTimeout';
  kIOHIDBiometricTripleTapTimeoutKey = 'TripleTapTimeout';
  kFnFunctionUsageMapKey = 'FnFunctionUsageMap';
  kFnKeyboardUsageMapKey = 'FnKeyboardUsageMap';
  kNumLockKeyboardUsageMapKey = 'NumLockKeyboardUsageMap';
  kKeyboardUsageMapKey = 'KeyboardUsageMap';
{$endif}

{$endif}
