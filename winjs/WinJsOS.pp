unit WinJsOS;

{$mode delphi}

interface

  uses
    ChakraTypes;

  function GetWinJsOSInstance: TJsValue;

implementation

  uses
    Chakra, WinJsFileSystem, SysUtils, DateUtils;

  function OSNow(Args: PJsValue; ArgCount: Word): TJsValue;
  begin
    Result := StringAsJsString(FormatDateTime('yyyy-mm-dd hh:nn:ss:zzz', Now));
  end;

  function OSNowAsMilliseconds(Args: PJsValue; ArgCount: Word): TJsValue;
  var
    UnixEpoch: TDateTime;
    Milliseconds: Integer;
  begin
    UnixEpoch := EncodeDate(1970, 1, 1);
    Milliseconds := MillisecondsBetween(Now, UnixEpoch);
    Result := IntAsJsNumber(Milliseconds);
  end;

  function GetWinJsOSInstance;
  begin
    Result := CreateObject;

    SetFunction(Result, 'now', OSNow);
    SetFunction(Result, 'nowAsMilliseconds', OSNowAsMilliseconds);

    SetProperty(Result, 'fileSystem', GetWinJsFileSystem);
  end;

end.