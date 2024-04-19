unit WinJsUtils;

{$mode delphi}

interface

  uses
    ChakraTypes;

  function RunJsFile(aScriptFilePath: WideString): Integer;

  function FileNameWithoutExtension(aFilePath: WideString): WideString;

  function ReadTextFile(aFilePath: WideString): WideString;

  procedure Debug(aMessage: WideString);

implementation

  uses
    WinJsRuntime, SysUtils, StrUtils, Classes, Windows, Chakra, WinJsError, ChakraError;

  procedure WriteErrLn(aFormat: WideString; aArgs: array of const);
  begin
    WriteLn(StdErr, WideFormat(aFormat, aArgs));
  end;

  function RunJsFile;
  var
    ErrorLevel: TJsValue;
  begin

    Result := 0;

    try

      with Runtime do begin
        RunScriptSource(ReadTextFile(aScriptFilePath), WideFormat('%s %s', [ ParamStr(0), aScriptFilePath ]));
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

  function FileNameWithoutExtension;
  var
    Extension: WideString;
  begin
    Extension := ExtractFileExt(aFilePath);

    if Extension <> '' then begin
      Result := ExtractFileName(
        Copy(
          aFilePath, 1, RPos(Extension, aFilePath) - 1
        )
      );
    end else begin
      Result := ExtractFileName(aFilePath);
    end;

  end;

  function ReadTextFile;
  var
    FileStream: TFileStream;
    S: UTF8String;
  begin

    Result := '';

    FileStream := TFileStream.Create(aFilePath, fmOpenRead);
    try

      if FileStream.Size = 0 then Exit;

      with FileStream do begin
        SetLength(S, Size);
        Read(S[1], Size);

        Result := UTF8Decode(S);
      end;

    finally
      FileStream.Free;
    end;

  end;

  procedure Debug;
  begin
    OutputDebugStringW(PWideChar(aMessage));
  end;

end.