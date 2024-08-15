unit WinJsOS;

{$mode delphi}

interface

  uses
    ChakraTypes;

  function GetWinJsOS: TJsValue;

implementation

  uses
    Chakra, WinJsFileSystem, SysUtils, DateUtils, WinJsUtils;

  function OSNow(Args: PJsValue; ArgCount: Word): TJsValue;
  begin
    Result := StringAsJsString(FormatDateTime('yyyy-mm-dd hh:nn:ss:zzz', Now));
  end;

  function OSNowAsMilliseconds(Args: PJsValue; ArgCount: Word): TJsValue;
  begin
    Result := IntAsJsNumber(NowAsMilliseconds);
  end;

  function GetWinJsOS;
  begin
    Result := CreateObject;

    SetFunction(Result, 'now', OSNow);
    SetFunction(Result, 'nowAsMilliseconds', OSNowAsMilliseconds);

    SetProperty(Result, 'fileSystem', GetWinJsFileSystem);
  end;

end.