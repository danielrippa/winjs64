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
    ChakraAPI, Chakra, WinJsProcess;

  function GetWinJs: TJsValue;
  begin
    Result := CreateObject;
    // TODO:
  end;

  procedure TWinJsRuntime.Init;
  const
    EnableExperimentalFeatures = $00000020;
  begin
    TryChakraAPI('JsCreateRuntime', JsCreateRuntime(EnableExperimentalFeatures, Nil, FRuntime));
    TryChakraAPI('JsCreateContext', JsCreateContext(FRuntime, FContext));
    TryChakraAPI('JsSetCurrentContext', JsSetCurrentContext(FContext));

    SetProperty(Global, 'process', GetWinJsProcess);
    SetProperty(Global, 'winjs', GetWinJs);
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
