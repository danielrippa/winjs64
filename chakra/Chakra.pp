unit Chakra;

{$mode delphi}

interface

  uses
    ChakraTypes;

  function GetGlobalObject: TJsValue;

  function Undefined: TJsValue;

  function CreateObject: TJsValue;

  procedure SetFunction(Instance: TJsValue; FunctionName: WideString; Callback: TJsFunctionFunc);

  function GetProperty(Instance: TJsValue; PropertyName: WideString): TJsValue;
  procedure SetProperty(Instance: TJsValue; PropertyName: WideString; Value: TJsValue);

  function GetIntProperty(Instance: TJsValue; PropertyName: WideString): Integer;
  function GetStrProperty(Instance: TJsValue; PropertyName: WideString): WideString;

  function CreateArray(ItemCount: Integer): TJsValue;

  function GetArrayItem(ArrayValue: TJsValue; ItemIndex: Integer): TJsValue;
  procedure SetArrayItem(ArrayValue: TJsValue; ItemIndex: Integer; Value: TJsValue);

  function GetValueType(Value: TJsValue): TJsValueType;

  function JsValueAsJsString(Value: TJsValue): TJsValue;
  function JsValueAsString(Value: TJsValue): WideString;

  function JsStringAsString(Value: TJsValue): WideString;
  function StringAsJsString(Value: WideString): TJsValue;

  function IntAsJsNumber(Value: Integer): TJsValue;
  function JsNumberAsInt(Value: TJsValue): Integer;

  function BooleanAsJsBoolean(Value: Boolean): TJsValue;
  function JsBooleanAsBoolean(Value: TJsValue): Boolean;

  function Run(SourceContext: UIntPtr; ScriptSource, ScriptName: WideString): TJsValue;

  function JsTypeName(Value: TJsValueType): WideString;

  function CallFunction(Func: TJsValue; Args: PJsValue; ArgCount: Word): TJsValue;

implementation

  uses
    ChakraAPI, ChakraError;

  function GetGlobalObject;
  begin
    TryChakraAPI('JsGetGlobalObject', JsGetGlobalObject(Result));
  end;

  function GetValueType;
  begin
    TryChakraAPI('JsGetValueType', JsGetValueType(Value, Result));
  end;

  function JsValueAsJsString;
  begin
    TryChakraAPI('JsConvertValueToString', JsConvertValueToString(Value, Result));
  end;

  function JsStringLength(Value: TJsValue): Integer;
  begin
    TryChakraAPI('JsCopyStringUtf16', JsCopyStringUtf16(Value, 0, -1, Nil, @Result));
  end;

  function JsStringAsString;
  var
    StringValue: TJsValue;
    L: Integer;
  begin
    Result := '';

    StringValue := JsValueAsJsString(Value);
    L := JsStringLength(StringValue);

    if L > 0 then begin
      SetLength(Result, L);
      TryChakraAPI('JsCopyStringUtf16', JsCopyStringUtf16(StringValue, 0, L, PWideChar(Result), Nil));
    end;
  end;

  function JsValueAsString;
  begin
    Result := JsStringAsString(JsValueAsJsString(Value));
  end;

  function StringAsJsString;
  const
    Null: WideChar = #0;
    PNull: PWideChar = @Null;
  var
    P: PWideChar;
  begin
    P := PWideChar(Value);
    if not Assigned(P) then P := PNull;

    TryChakraAPI('JsCreateStringUtf16', JsCreateStringUtf16(P, Length(Value), Result));
  end;

  function GetObjectProperty(Instance, PropertyName: TJsValue): TJsValue;
  begin
    TryChakraAPI('JsObjectGetProperty', JsObjectGetProperty(Instance, PropertyName, Result));
  end;

  function GetProperty;
  begin
    Result := GetObjectProperty(Instance, StringAsJsString(PropertyName));
  end;

  function GetStrProperty;
  begin
    Result := JsStringAsString(GetProperty(Instance, PropertyName));
  end;

  function GetIntProperty;
  begin
    Result := JsNumberAsInt(GetProperty(Instance, PropertyName));
  end;

  function IntAsJsNumber;
  begin
    TryChakraAPI('JsIntToNumber', JsIntToNumber(Value, Result));
  end;

  function JsNumberAsInt;
  begin
    TryChakraAPI('JsNumberToInt', JsNumberToInt(Value, Result));
  end;

  procedure SetObjectProperty(Instance, PropertyName, Value: TJsValue);
  begin
    TryChakraAPI('JsObjectSetProperty', JsObjectSetProperty(Instance, PropertyName, Value, False));
  end;

  procedure SetProperty;
  begin
    SetObjectProperty(Instance, StringAsJsString(PropertyName), Value);
  end;

  function CreateExternalArrayBuffer(Value: WideString): TJsValue;
  begin

    TryChakraAPI(
      'JsCreateExternalArrayBuffer',

      JsCreateExternalArrayBuffer(
        PWideChar(Value),
        Length(Value) * SizeOf(WideChar),
        Nil, Nil,
        Result
      )
    );

  end;

  function Run;
  var
    Source, URL: TJsValue;
  begin
    Source := CreateExternalArrayBuffer(ScriptSource);
    URL := StringAsJsString(ScriptName);

    TryChakraAPI('JsRun', JsRun(Source, SourceContext, URL, [psaUtf16Encoded], Result));
  end;

  function JsTypeName;
  begin
    case Value of
      JsUndefined: Result := 'Undefined';
      JsNull: Result := 'Null';
      JsNumber: Result := 'Number';
      JsString: Result := 'String';
      JsBoolean: Result := 'Boolean';
      JsObject: Result := 'Object';
      JsFunction: Result := 'Function';
      JsError: Result := 'Error';
      JsArray: Result := 'Array';
      JsSymbol: Result := 'Symbol';
      JsArrayBuffer: Result := 'ArrayBuffer';
      JsTypedArray: Result := 'TypedArray';
      JsDataView: Result := 'DataView';
    end;
  end;

  function CreateObject;
  begin
    TryChakraAPI('JsCreateObject', JsCreateObject(Result));
  end;

  function FunctionCallback(Callee: TJsValue; IsConstructCall: ByteBool; Args: PJsValue; ArgCount: Word; Callback: Pointer): TJsValue; stdcall;
  var
    Fn: TJsFunctionFunc;
  begin
    Fn := Callback;

    Inc(Args); Dec(ArgCount);

    Result := Fn(Args, ArgCount);
  end;

  function CreateNamedFunction(FunctionName: TJsValue; Callback: TJsFunctionFunc): TJsValue;
  begin
    TryChakraAPI('JsCreateNamedFunction', JsCreateNamedFunction(FunctionName, FunctionCallback, @Callback, Result));
  end;

  procedure SetFunction;
  var
    Fn, Name: TJsValue;
  begin
    Name := StringAsJsString(FunctionName);
    Fn := CreateNamedFunction(Name, Callback);
    SetObjectProperty(Instance, Name, Fn);
  end;

  function Undefined;
  begin
    TryChakraAPI('JsGetUndefinedValue', JsGetUndefinedValue(Result));
  end;

  function CreateArray;
  begin
    TryChakraAPI('JsCreateArray', JsCreateArray(ItemCount, Result));
  end;

  function GetArrayItem;
  begin
    TryChakraAPI('JsGetIndexedProperty', JsGetIndexedProperty(ArrayValue, IntAsJsNumber(ItemIndex), Result));
  end;

  procedure SetArrayItem;
  begin
    TryChakraAPI('JsSetIndexedProperty', JsSetIndexedProperty(ArrayValue, IntAsJsNumber(ItemIndex), Value));
  end;

  function BooleanAsJsBoolean;
  begin
    TryChakraAPI('JsBoolToBoolean', JsBoolToBoolean(Value, Result));
  end;

  function JsBooleanAsBoolean;
  var
    B: ByteBool;
  begin
    TryChakraAPI('JsBooleanToBool', JsBooleanToBool(Value, B));
    Result := B;
  end;

  function CallFunction;
  begin
    TryChakraAPI('JsCallFunction', JsCallFunction(Func, Args, ArgCount, Result));
  end;

end.