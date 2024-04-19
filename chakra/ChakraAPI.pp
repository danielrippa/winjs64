unit ChakraAPI;

{$mode delphi}
{$calling stdcall}


interface

  uses
    ChakraTypes;

  const
    dll = 'ChakraCore_x64.dll';

  function JsCreateRuntime(Attributes: Cardinal; ThreadService: TJsThreadServiceFunc; out Runtime: TJsValue): TJsErrorCode; external dll;
  function JsCreateContext(Runtime: TJsValue; out NewContext: TJsValue): TJsErrorCode; external dll;
  function JsGetCurrentContext(out CurrentContext: TJsValue): TJsErrorCode; external dll;
  function JsSetCurrentContext(Context: TJsValue): TJsErrorCode; external dll;

  function JsGetGlobalObject(out GlobalObject: TJsValue): TJsErrorCode; external dll;

  function JsCreateExternalArrayBuffer(Data: Pointer; ByteLength: Cardinal; FinalizeCallback: TJsFinalizeProc; CallbackState: Pointer; out Result: TJsValue): TJsErrorCode; external dll;

  function JsRun(Script: TJsValue; SourceContext: UIntPtr; SourceUrl: TJsValue; ParseAttributes: TJsParseScriptAttributeSet; out Result: TJsValue): TJsErrorCode; external dll;

  function JsCreateError(Message: TJsValue; out Error: TJsValue): TJsErrorCode; external dll;
  function JsSetException(Error: TJsValue): TJsErrorCode; external dll;
  function JsGetAndClearExceptionWithMetadata(out Metadata: TJsValue): TJsErrorCode; external dll;

  function JsGetUndefinedValue(out UndefinedValue: TJsValue): TJsErrorCode; external dll;

  function JsCreateNamedFunction(FunctionName: TJsValue; NativeFunction: TJsNativeFunc; CallbackState: Pointer; out FunctionValue: TJSvalue): TJsErrorCode; external dll;

  function JsCreateObject(out Value: TJsValue): TJsErrorCode; external dll;

  function JsObjectGetProperty(Instance, Key: TJsValue; out Value: TJsValue): TJsErrorCode; external dll;
  function JsObjectSetProperty(Instance, Key, Value: TJsValue; UseStrictRules: ByteBool): TJsErrorCode; external dll;

  function JsCreateArray(ItemCount: Cardinal; out ArrayValue: TJsValue): TJsErrorCode; external dll;

  function JsGetIndexedProperty(ArrayValue, Index: TJsValue; out Value: TJsValue): TjsErrorCode; external dll;
  function JsSetIndexedProperty(ArrayValue, ItemIndex, Value: TJsValue): TJsErrorCode; external dll;

  function JsGetValueType(Value: TJsValue; out ValueType: TJsValueType): TJsErrorCode; external dll;

  function JsNumberToInt(Value: TJsValue; out IntValue: Integer): TJsErrorCode; external dll;
  function JsIntToNumber(IntValue: Integer; out Value: TJsValue): TJsErrorCode; external dll;

  function JsBoolToBoolean(BooleanValue: ByteBool; out Value: TJsValue): TJsErrorCode; external dll;
  function JsBooleanToBool(BooleanValue: TJsValue; out Value: ByteBool): TJsErrorCode; external dll;

  function JsCreateStringUtf16(Content: PUnicodeChar; Length: NativeUInt; out Value: TJsValue): TJsErrorCode; external dll;
  function JsCopyStringUtf16(Value: TJsValue; Start: Integer; Length: Integer; Buffer: PUnicodeChar; Written: PNativeUInt): TJsErrorCode; external dll;

  function JsConvertValueToString(Value: TJsValue; out StringValue: TJsValue): TJsErrorCode; external dll;

implementation

end.