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
      public
        procedure Init;
        function RunScriptSource(aScriptSource, aScriptName: WideString): TJsValue;
        property Global: TJsValue read FGlobal;
    end;

  var
    Runtime: TWinJsRuntime;

implementation

  uses
    Chakra, ChakraError, ChakraAPI, WinJsUtils, WinJsOS, WinJsProcess;

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

    Result := Runtime.RunScriptSource(ReadTextFile(aFilePath), aScriptName);
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
  begin
    TryChakraAPI('JsCreateRuntime', JsCreateRuntime(EnableExperimentalFeatures, Nil, FRuntime));
    TryChakraAPI('JsCreateContext', JsCreateContext(FRuntime, FContext));
    TryChakraAPI('JsSetCurrentContext', JsSetCurrentContext(FContext));

    FGlobal := GetGlobalObject;

    SetProperty(FGlobal, 'winjs', GetWinJs);
    SetProperty(FGlobal, 'os', GetWinJsOs);
  end;

  function TWinJsRuntime.RunScriptSource;
  begin
    Inc(FSourceContext);
    Result := Run(FSourceContext, aScriptSource, aScriptName);
  end;

initialization

  Runtime.Init;

end.