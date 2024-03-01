unit WinJsProcess;

{$mode delphi}

interface

  uses
    ChakraTypes;

  function GetWinJsProcess: TJsValue;

implementation

  uses
    Chakra, WinJsProcessIO, SysUtils, ChakraErr;

  function GetArgs: TJsValue;
  var
    I: Integer;
  begin
    Result := CreateArray(ParamCount + 1);

    for I := 0 to ParamCount do begin
      SetArrayItem(Result, I, StringAsJsString(ParamStr(I)));
    end;
  end;

  function GetEnvVars: TJsValue;
  var
    I: Integer;
    S: array of String;
  begin
    Result := CreateObject;

    for I := 0 to GetEnvironmentVariableCount - 1 do begin
      S := GetEnvironmentString(I).Split(['=']);

      if ((Length(S) > 1) and (not S[0].IsEmpty)) then begin
        SetProperty(Result, S[0], StringAsJsString(S[1]));
      end;
    end;
  end;

  function ProcessSleep(Args: PJsValue; ArgCount: Word): TJsValue;
  begin
    Result := Undefined;
    CheckParams('sleep', Args, ArgCount, [jsNumber], 1);
    Sleep(JsNumberAsInt(Args^));
  end;

  function GetWinJsProcess;
  begin
    Result := CreateObject;

    SetProperty(Result, 'args', GetArgs);
    SetProperty(Result, 'envVars', GetEnvVars);

    SetFunction(Result, 'sleep', ProcessSleep);

    SetProperty(Result, 'io', GetWinJsProcessIO);
  end;

end.