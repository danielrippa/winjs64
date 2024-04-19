unit WinJsInstance;

{$mode delphi}

interface

  uses
    ChakraTypes;

  function GetWinJsInstance: TJsValue;

implementation

  uses
    Chakra, WinJsUtils, WinJsRuntime, DynLibs, Windows, SysUtils, ChakraError, WinJsError, WinJsProcess;

  function LoadLibrary(aFilePath: WideString): THandle;
  var
    Handle: THandle;
    LastError: DWORD;
    ErrorMessage: String;
    Path: UnicodeString;
  begin

    Path := aFilePath;

    Handle := DynLibs.LoadLibrary(Path);

    if Handle = 0 then begin
      LastError := GetLastError();
      ErrorMessage := SysErrorMessage(LastError);

      raise Exception.CreateFmt('%s ''%s''', [ErrorMessage, aFilePath]);
    end;

    Result := Handle;
  end;

  function LoadWasm(aFilePath: WideString): TJsValue;
  begin
    Result := StringAsJsString(ReadTextFile(aFilePath));
  end;

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

  var WinJsLibraryHandles: array of TLibHandle;

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

    Handle := LoadLibrary(FilePath);

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

  function GetWinJsInstance;
  begin
    Result := CreateObject;

    SetFunction(Result, 'loadScript', WinJsLoadScript);
    SetFunction(Result, 'loadLibrary', WinJsLoadLibrary);
    SetFunction(Result, 'loadWasm', WinJsLoadWasm);

    SetProperty(Result, 'process', GetWinJsProcess);
  end;

end.