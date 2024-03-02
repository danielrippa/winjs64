unit WinJsRuntime;

{$mode delphi}

interface

  uses
    ChakraTypes;

  type

    TWinJsRuntime = record
      private
        FRuntime: TJsValue;
        FContext: TJsValue;
        FGlobal: TJsValue;

        FSourceContext: UIntPtr;

        function GetGlobal: TJsValue;
      public
        procedure Init;
        function EvalScriptSource(aScriptName, aScriptSource: WideString): TJsValue;
        property Global: TJsValue read GetGlobal;
    end;

  var Runtime: TWinJsRuntime;

implementation

  uses
    ChakraAPI, Chakra, WinJsProcess, WinjsUtils, WinJsErr, SysUtils, WinJsOS;

  var WinJsLibraryHandles: array of TLibHandle;

  function WinJsLoadScript(Args: PJsValue; ArgCount: Word): TJsValue;
  var
    aFilePath, aScriptName: WideString;
  begin
    CheckParams('loadScript', Args, ArgCount, [jsString, jsString], 1);

    aFilePath := JsStringAsString(Args^); Inc(Args);

    aScriptName := '';

    if ArgCount > 1 then begin
      aScriptName := JsStringAsString(Args^);
    end;

    Result := LoadScript(aFilePath, aScriptName);
  end;

  function LoadWinJsLibrary(FilePath: WideString): TJsValue;
  type
    TGetJsValue = function: TJsValue;
  var
    L: Integer;
    Handle: TLibHandle;
    GetJsValue: TGetJsValue;
  begin
    L := Length(WinJsLibraryHandles);
    SetLength(WinJsLibraryHandles, L + 1);

    Handle := WinjsUtils.LoadLibrary(FilePath);

    WinJsLibraryHandles[L] := Handle;

    GetJsValue := GetProcAddress(Handle, 'GetJsValue');

    if Assigned(GetJsValue) then begin
      Result := GetJsValue;
    end else begin
      raise EWinJsException.Create(Format('Missing GetJsValue export in ''%s'' library', [FilePath]), 0);
    end;

  end;

  function WinjsLoadLibrary(Args: PJsValue; ArgCount: Word): TJsValue;
  var
    aFilePath: WideString;
  begin
    CheckParams('loadLibrary', Args, ArgCount, [jsString], 1);
    aFilePath := JsStringAsString(Args^);
    Result := LoadWinJsLibrary(aFilePath);
  end;

  function WinjsLoadWasm(Args: PJsValue; ArgCount: Word): TJsValue;
  begin
    CheckParams('loadWasm', Args, ArgCount, [jsString], 1);
    Result := LoadWasm(JsStringAsString(Args^));
  end;

  function GetWinJs: TJsValue;
  begin
    Result := CreateObject;

    SetFunction(Result, 'loadScript', WinJsLoadScript);
    SetFunction(Result, 'loadLibrary', WinJsLoadLibrary);
    SetFunction(Result, 'loadWasm', WinJsLoadWasm);

    SetProperty(Result, 'process', GetWinJsProcess);
  end;

  procedure TWinJsRuntime.Init;
  const
    EnableExperimentalFeatures = $00000020;
  begin
    TryChakraAPI('JsCreateRuntime', JsCreateRuntime(EnableExperimentalFeatures, Nil, FRuntime));
    TryChakraAPI('JsCreateContext', JsCreateContext(FRuntime, FContext));
    TryChakraAPI('JsSetCurrentContext', JsSetCurrentContext(FContext));

    SetProperty(Global, 'winjs', GetWinJs);
    SetProperty(Global, 'os', GetWinJsOS);
  end;

  function TWinJsRuntime.GetGlobal;
  begin
    Result := GetGlobalObject;
  end;

  function TWinJsRuntime.EvalScriptSource;
  begin
    Inc(FSourceContext);
    Result := Chakra.EvalScriptSource(FSourceContext, aScriptSource, aScriptName);
  end;

initialization

  Runtime.Init;

end.