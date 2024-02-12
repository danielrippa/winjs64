unit WinJsProcessIO;

{$mode delphi}

interface

  uses
    ChakraTypes;

  function GetWinJsProcessIO: TJsValue;

implementation

  uses
    Chakra, WinJsErr;

  function ProcessIOStdOut(Args: PJsValue; ArgCount: Word): TJsValue;
  begin
    Result := Undefined;
    CheckParams('process.io.stdout', Args, ArgCount, [], 1);

    Write(JsValueAsString(Args^));
  end;

  function GetWinJsProcessIO;
  begin
    Result := CreateObject;

    SetFunction(Result, 'stdout', ProcessIOStdOut);
  end;

end.