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
    Chakra, ChakraError, ChakraAPI, WinJsInstance, WinJsOS;

  procedure TWinJsRuntime.Init;
  const
    EnableExperimentalFeatures = $00000020;
  begin
    TryChakraAPI('JsCreateRuntime', JsCreateRuntime(EnableExperimentalFeatures, Nil, FRuntime));
    TryChakraAPI('JsCreateContext', JsCreateContext(FRuntime, FContext));
    TryChakraAPI('JsSetCurrentContext', JsSetCurrentContext(FContext));

    FGlobal := GetGlobalObject;

    SetProperty(FGlobal, 'winjs', GetWinJsInstance);
    SetProperty(FGlobal, 'os', GetWinJsOsInstance);
  end;

  function TWinJsRuntime.RunScriptSource;
  begin
    Inc(FSourceContext);
    Result := Chakra.Run(FSourceContext, aScriptSource, aScriptName);
  end;

initialization

  Runtime.Init;

end.