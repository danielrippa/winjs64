unit WinJsProcess;

{$mode delphi}

interface

  uses
    ChakraTypes;

  function GetWinJsProcess: TJsValue;

implementation

  uses
    Chakra, WinJsProcessIO;

  function GetWinJsProcess;
  begin
    Result := CreateObject;

    SetProperty(Result, 'io', GetWinJsProcessIO);
    // TODO:
  end;

end.