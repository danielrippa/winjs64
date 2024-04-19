unit ChakraError;

{$mode delphi}

interface

  uses
    ChakraTypes, SysUtils;

  type

    TScriptError = record
      Line: Integer;
      Column: Integer;
      Source: WideString;
      ScriptName: WideString;

      Message: WideString;
    end;

    EChakraError = class(Exception);

    EChakraAPIError = class(EChakraError);

    EChakraScriptError = class(EChakraError)
      ScriptError: TScriptError;

      constructor Create(aMessage: WideString; aScriptError: TScriptError);
    end;

  procedure TryChakraAPI(aFunctionName: WideString; aErrorCode: TJsErrorCode);

  procedure CheckParams(FunctionName: UnicodeString; Args: PJsValue; ArgCount: Word; ArgTypes: array of TJsValueType; MandatoryCount: Integer);

  procedure ThrowError(aFmt: WideString; aParams: array of const);

implementation

  uses
    Chakra, ChakraAPI, TypInfo;

  function GetScriptError: TScriptError;
  var
    Metadata: TJsValue;
    Exception: TJsValue;
  begin
    TryChakraAPI('JsGetAndClearExceptionWithMetadata', JsGetAndClearExceptionWithMetadata(Metadata));

    with Result do begin
      Line := GetIntProperty(Metadata, 'line');
      Column := GetIntProperty(Metadata, 'column');
      Source := GetStrProperty(Metadata, 'source');
      ScriptName := GetStrProperty(Metadata, 'url');

      Exception := GetProperty(Metadata, 'exception');

      if GetValueType(Exception) = jsObject then begin
        Exception := JsValueAsJsString(Exception);
      end;

      Message := JsValueAsString(Exception);
    end;
  end;

  type

    TErrorType = (etNoError, etUsage, etEngine, etScript, etUnknown);

  function GetErrorType(aErrorCode: TJsErrorCode): TErrorType;
  begin
    if (aErrorCode = jecNoError) then
      Result := etNoError
    else if (aErrorCode >= jecUsageError) and (aErrorCode < jecEngineError) then
      Result := etUsage
    else if (aErrorCode >= jecEngineError) and (aErrorCode < jecScriptError) then
      Result := etEngine
    else if (aErrorCode >= jecScriptError) and (aErrorCode < TJsErrorCode($40000)) then
      Result := etScript
    else
      Result := etUnknown
  end;

  resourcestring

    NotEnoughParameters = 'Not enough parameters when calling ChakraCore API function ''%s''. %d parameters expected but %d parameters given';
    WrongArgumentType = 'Wrong argument type when calling ChakraCore API function ''%s''. Argument[%d] (%s)%s must be a %s';
    InvalidArgument = 'Invalid argument value when calling ChakraCore API function ''%s''';
    NullArgument = 'Argument was null when calling ChakraCore API function ''%s''';
    OutOfMemory = 'The ChakraCore engine has run out of memory when calling ChakraCore API function ''%s''';
    ScriptError = 'A ChakraCore Script error has occurred when calling ChakraCore API function ''%s''';
    UnknownError = 'An error of type ''%s'' with code ''%d'' has occurred when calling ChakraCore API function ''%s''';

  function FormatFromErrorCode(aErrorCode: TJsErrorCode): WideString;
  begin
    case aErrorCode of

      jecInvalidArgument: Result := InvalidArgument;
      jecNullArgument: Result := NullArgument;
      jecOutOfMemory: Result := OutOfMemory;

      jecScriptError: Result := ScriptError;

      else Result := UnknownError;
    end;
  end;

  procedure TryChakraAPI;
  var
    MessageFormat: WideString;
    ErrorCode: Integer;
    EnumName: WideString;
  begin

    if aErrorCode = jecNoError then Exit;

    aErrorCode := TJsErrorCode(Ord(aErrorCode) and $F0000);

    MessageFormat := FormatFromErrorCode(aErrorCode);

    case aErrorCode of

      jecInvalidArgument, jecNullArgument:
        raise EChakraAPIError.Create(WideFormat(MessageFormat, [aFunctionName]));

      jecOutOfMemory:
        raise EChakraError.Create(WideFormat(MessageFormat, [aFunctionName]));

      jecScriptError:
        raise EChakraScriptError.Create(WideFormat(MessageFormat, [aFunctionName]), GetScriptError);

      else

        EnumName := GetEnumName(TypeInfo(TErrorType), Ord(GetErrorType(aErrorCode)));
        raise EChakraError.Create(WideFormat(MessageFormat, [EnumName, ErrorCode, aFunctionName]));

    end;

  end;

  constructor EChakraScriptError.Create;
  begin
    Message := aMessage;
    ScriptError := aScriptError;
  end;

  procedure CheckParams;
  var
    I: Integer;
    Value: TJsValue;
    ValueType: TJsValueType;
    RequiredTypeName, ValueTypeName: UnicodeString;
    ValueString: UnicodeString;
  begin
    if MandatoryCount > ArgCount then begin
      ThrowError('Not enough parameters when calling ''%s''. %d parameters expected but %d parameters given', [FunctionName, MandatoryCount, ArgCount]);
    end;

    for I := 0 to Length(ArgTypes) - 1 do begin

      Value := Args^; Inc(Args);

      ValueType := GetValueType(Value);

      if ValueType <> ArgTypes[I] then begin

        ValueTypeName := JsTypeName(ValueType);
        ValueString := JsValueAsString(Value);

        RequiredTypeName := JsTypeName(ArgTypes[I]);

        ThrowError('Error calling ''%s''. Argument[%d] (%s)%s must be %s', [ FunctionName, I, ValueTypeName, ValueString, RequiredTypeName ]);

      end;
    end;
  end;

  function CreateError(Message: TJsValue): TJsValue;
  begin
    TryChakraAPI('JsCreateError', JsCreateError(Message, Result));
  end;

  procedure SetException(Message: TJsValue);
  begin
    TryChakraAPI('JsSetException', JsSetException(CreateError(Message)));
  end;

  procedure ThrowError;
  var
    Message: TJsValue;
  begin
    Message := StringAsJsString(WideFormat(aFmt, aParams));
    SetException(Message);
  end;

end.