unit WinJsErr;

{$mode delphi}

interface

  uses
    ChakraTypes, SysUtils;

  type

    EWinjsException = class(Exception)
      private
        FErrorCode: Integer;
      public
        constructor Create(aMessage: WideString; aErrorCode: Integer);
        property ErrorCode: Integer read FErrorCode;
    end;

  procedure CheckParams(aFunctionName: WideString; Args: PJsValue; ArgCount: Word; ArgTypes: array of TJsValueType; aMandatoryCount: Integer);

implementation

  uses
    ChakraErr, Chakra, ChakraUtils;

  constructor EWinjsException.Create;
  begin
    inherited Create(aMessage);
    FErrorCode := aErrorCode;
  end;

  procedure CheckParams;
  var
    I: Integer;
    Value: TJsValue;
    ValueType: TJsValueType;
    RequiredTypeName, ValueTypeName: WideString;
    ValueString: WideString;
  begin
    if aMandatoryCount > ArgCount then begin
      ThrowError('Not enough parameters when calling ''%s''. %d parameters expected but %d parameters given', [aFunctionName, aMandatoryCount, ArgCount]);
    end;

    for I := 0 to Length(ArgTypes) - 1 do begin

      Value := Args^; Inc(Args);

      ValueType := GetValueType(Value);

      if ValueType <> ArgTypes[I] then begin

        ValueTypeName := JsTypeName(ValueType);
        ValueString := JsValueAsString(Value);

        RequiredTypeName := JsTypeName(ArgTypes[I]);

        ThrowError('Error calling ''%s''. Argument[%d] (%s)%s must be %s', [ aFunctionName, I, ValueTypeName, ValueString, RequiredTypeName ]);

      end;
    end;
  end;

end.