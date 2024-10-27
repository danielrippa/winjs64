unit WinJsUtils;

{$mode delphi}

interface

  uses
    ChakraTypes, WinJsError;

  function RunJsFile(aScriptFilePath: WideString): Integer;

  procedure WinJsUsage;

  function ReadTextFile(aFilePath: WideString): WideString;

  procedure Debug(aMessage: WideString);

  function LoadWasm(aFilePath: WideString): TJsValue;

  function LoadWinJsLibrary(aFilePath: WideString): TJsValue;

  function NowAsMilliseconds: Integer;

  procedure Quit(AErrorLevel: Integer);

  function SetTerminalMode(AEnable: Boolean): Boolean;

  function FilenameWithoutExtension(aFilePath: WideString): WideString;

  implementation

  uses
    DynLibs, DateUtils, SysUtils, StrUtils, Classes, Windows, Chakra, WinJsRuntime, ChakraError;

  function NowAsMilliseconds;
  var
    UnixEpoch: TDateTime;
  begin
    UnixEpoch := EncodeDate(1970, 1, 1);
    Result := MillisecondsBetween(Now, UnixEpoch);
  end;

  procedure WriteErrLn(aFormat: WideString; aArgs: array of const);
  begin
    WriteLn(StdErr, WideFormat(aFormat, aArgs));
  end;

  function FilenameWithoutExtension;
  var
    Extension: WideString;
  begin
    Extension := ExtractFileExt(aFilePath);

    if Extension <> '' then begin
      Result := ExtractFileName(Copy(aFilePath, 1, RPos(Extension, aFilePath) - 1));
    end else begin
      Result := ExtractFileName(aFilePath);
    end;
  end;

  procedure WinJsUsage;
  begin
    WriteErrLn('Usage:', []);
    WriteErrln('%s jsFilePath', [ FilenameWithoutExtension(ParamStr(0)) ]);
  end;

  function ReadTextFile;
  var
    FileStream: TFileStream;
    S: UTF8String;
  begin
    Result := '';

    FileStream := TFileStream.Create(aFilePath, fmOpenRead or fmShareDenyNone);

    if FileStream.Size = 0 then Exit;

    with FileStream do begin
      SetLength(S, Size);
      Read(S[1], Size);

      Result := UTF8String(S);

    end;

    FileStream.Free;

  end;

  procedure Debug;
  begin
    OutputDebugStringW(PWideChar(aMessage));
  end;

  function LoadWasm(aFilePath: WideString): TJsValue;
  begin
    Result := StringAsJsString(ReadTextFile(aFilePath));
  end;

  function TryLoadLibrary(aFilePath: WideString): THandle;
  var
    Handle: THandle;
    LastError: DWORD;
    ErrorMessage: String;
    FilePath: String;
  begin

    FilePath := aFilePath;

    Handle := LoadLibrary(PChar(FilePath));

    if Handle = 0 then begin
      LastError := GetLastError();
      ErrorMessage := SysErrorMessage(LastError);

      raise Exception.CreateFmt('%s ''%s''', [ErrorMessage, aFilePath]);
    end;

    Result := Handle;
  end;

  var WinJsLibraryHandles: array of TLibHandle;

  function LoadWinJsLibrary;
  type
    TGetJsValue = function: TJsValue;
  var
    L: Integer;
    Handle: TLibHandle;
    GetJsValue: TGetJsValue;
  begin

    L := Length(WinJsLibraryHandles);
    SetLength(WinJsLibraryHandles, L + 1);

    Handle := TryLoadLibrary(aFilePath);

    WinJsLibraryHandles[L] := Handle;

    GetJsValue := GetProcAddress(Handle, 'GetJsValue');

    if Assigned(GetJsValue) then begin
      Result := GetJsValue;
    end else begin
      raise EWinJsException.Create(Format('Missing GetJsValue export in ''%s'' library', [aFilePath]), 0);
    end;

  end;

  function RunJsFile;
  var
    ErrorLevel: TJsValue;
    ScriptSource, ScriptName: WideString;
  begin

    Result := 0;

    try

      with Runtime do begin
        ScriptSource := ReadTextFile(aScriptFilePath);
        ScriptName := aScriptFilePath;

        RunScriptSource(ScriptSource, ScriptName);

        ErrorLevel := GetProperty(Global, 'errorLevel');
      end;

      if GetValueType(ErrorLevel) = jsNumber then begin
        Result := JsNumberAsInt(ErrorLevel);
      end;

    except

      on E: EChakraScriptError do begin

        with E.ScriptError do begin
          WriteErrLn('[%s] %s.', [ E.ClassName, E.Message ]);
          WriteErrLn('Script: "%s"', [ ScriptName ]);
          WriteErrLn('Message: %s', [ Message ]);
          WriteErrLn('Line: %d, Column: %d', [ Line + 1, Column ]);
          WriteErrLn('Source: %s', [ Source ]);

          Result := ErrorCode;
        end;

      end;

       on E: EWInJsException do begin
        with E do begin
          WriteErrLn('Message: %s', [Message]);
          WriteErrLn('Error: %d', [ErrorCode]);

          Result := ErrorCode;
        end;
      end;

      on E: Exception do begin
        with E do begin
          WriteErrLn('Exception: %s', [Message]);

          Result := -1;
        end;
      end;

    end;

  end;

  procedure Quit;
  begin
    Halt(AErrorLevel);
  end;

  function SetTerminalMode;
  var
    h: THandle;
    lwMode: LongWord;
    Enable: Boolean;
  begin

    h := GetStdHandle(STD_OUTPUT_HANDLE);
    GetConsoleMode(h, @lwMode);

    if AEnable then begin
      lwMode := lwMode or ENABLE_VIRTUAL_TERMINAL_PROCESSING;
    end else begin
      lwMode := lwMode and not ENABLE_VIRTUAL_TERMINAL_PROCESSING;
    end;

    Result := SetConsoleMode(h, lwMode);
  end;

end.