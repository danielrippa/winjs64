unit WinJsProcess;

{$mode delphi}

interface

  uses
    ChakraTypes;

  function GetWinJsProcess: TJsValue;

implementation

  uses
    Chakra, SysUtils, ChakraError, WinJsUtils, Windows;

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

  function ProcessQuit(Args: PJsValue; ArgCount: Word): TJsValue;
  begin
    Result := Undefined;
    CheckParams('quit', Args, ArgCount, [jsNumber], 1);
    Quit(JsNumberAsInt(Args^));
  end;

  function ProcessIOStdOut(Args: PJsValue; ArgCount: Word): TJsValue;
  begin
    Result := Undefined;
    CheckParams('process.io.stdout', Args, ArgCount, [], 1);

    Write(JsValueAsString(Args^));
  end;

  function ProcessIOStdErr(Args: PJsValue; ArgCount: Word): TJsValue;
  begin
    Result := Undefined;
    CheckParams('process.io.stderr', Args, ArgCount, [], 1);

    Write(StdErr, JsValueAsString(Args^));
  end;

  function ProcessIODebug(Args: PJsValue; ArgCount: Word): TJsValue;
  begin
    Result := Undefined;
    CheckParams('process.io.debug', Args, ArgCount, [], 1);

    Debug(JsValueAsString(Args^));
  end;

  function ProcessIOReadLn(Args: PJsValue; ArgCount: Word): TJsValue;
  var
    Value: String;
  begin
    ReadLn(Value);
    Result := StringAsJsString(Value);
  end;

  function ProcessIOStdIn(Args: PJsValue; ArgCount: Word): TJsValue;
  var
    StandardInput: WideString;
    Character: Char;
  begin
    StandardInput := '';

    while not EOF(Input) do begin
      Read(Character);
      StandardInput := StandardInput + Character;
    end;

    Result := StringAsJsString(StandardInput);
  end;

  function ProcessIOSetTerminalMode(Args: PJsValue; ArgCount: Word): TJsValue;
  var
    h: THandle;
    lwMode: LongWord;
    Enable: Boolean;
  begin
    CheckParams('process.io.setTerminalMode', Args, ArgCount, [jsBoolean], 1);

    Enable := JsBooleanAsBoolean(Args^);

    h := GetStdHandle(STD_OUTPUT_HANDLE);
    GetConsoleMode(h, @lwMode);

    if Enable then begin
      lwMode := lwMode or ENABLE_VIRTUAL_TERMINAL_PROCESSING;
    end else begin
      lwMode := lwMode and not ENABLE_VIRTUAL_TERMINAL_PROCESSING;
    end;

    Result := BooleanAsJsBoolean(SetConsoleMode(h, lwMode));
  end;

  function GetWinJsProcessIO: TJsValue;
  begin
    Result := CreateObject;

    SetFunction(Result, 'stdout', ProcessIOStdOut);
    SetFunction(Result, 'stderr', ProcessIOStdErr);

    SetFunction(Result, 'setTerminalMode', ProcessIOSetTerminalMode);

    SetFunction(Result, 'getStdin', ProcessIOStdIn);

    SetFunction(Result, 'readLn', ProcessIOReadln);

    SetFunction(Result, 'debug', ProcessIODebug);
  end;

  function GetWinJsProcess;
  begin
    Result := CreateObject;

    SetProperty(Result, 'args', GetArgs);
    SetProperty(Result, 'envVars', GetEnvVars);

    SetFunction(Result, 'sleep', ProcessSleep);
    SetFunction(Result, 'quit', ProcessQuit);

    SetProperty(Result, 'io', GetWinJsProcessIO);
  end;

end.