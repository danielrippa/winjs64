unit WinJsUtils;

{$mode delphi}

interface

  uses ChakraTypes;

  function RunJsFile(aScriptFilePath: WideString): Integer;

  function ExtractFileNameWithoutExt(aFilePath: WideString): WideString;

implementation

  uses
    WinJsRuntime, Chakra, SysUtils, ChakraErr, StrUtils, Classes, WinJsErr;

  function ExtractFileNameWithoutExt;
  var
    Ext: WideString;
  begin
    Ext := ExtractFileExt(aFilePath);
    if Ext <> '' then begin
      Result := ExtractFileName(Copy(aFilePath, 1, RPos(Ext, aFilePath) - 1));
    end else begin
      Result := ExtractFileName(aFilePath);
    end;
  end;

  procedure WriteErrLn(aFormat: WideString; aArgs: array of const);
  begin
    WriteLn(StdErr, WideFormat(aFormat, aArgs));
  end;

  function ReadUnicodeTextFileContent(aFilePath: WideString): WideString;
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

        Result := UTF8String(S);

      end;

    finally
      FileStream.Free;
    end;

  end;

  function RunJsFile;
  var
    ScriptName: WideString;
    ScriptSource: WideString;

    ErrorLevel: TJsValue;
  begin

    Result := 0;

    ScriptName := ExtractFileNameWithoutExt(aScriptFilePath);
    ScriptSource := ReadUnicodeTextFileContent(aScriptFilePath);

    try

      Runtime.EvalScriptSource(ScriptName, ScriptSource);

      ErrorLevel := GetProperty(Runtime.Global, 'errorLevel');

      if GetValueType(ErrorLevel) = jsNumber then begin
        Result := JsNumberAsInt(ErrorLevel);
      end;

    except

      on E: EWInJsException do begin
        with E do begin
          WriteErrLn('Message: %s', [Message]);
          WriteErrLn('Error: %d', [ErrorCode]);

          Result := ErrorCode;
        end;
      end;

      on E: EChakraScriptError do begin
        with E.ScriptError do begin
          WriteErrLn('[%s] %s.', [E.ClassName, E.Message]);
          WriteErrLn('Script: "%s" (Line: %d, Column: %d)', [ ScriptName, Line + 1, Column ]);
          WriteErrLn('Source: %s', [Source]);

          Result := ErrorCode;
        end;

      end;

      on E: EChakraAPIError do begin
        // TODO:
      end;

      on E: Exception do begin
        WriteErrLn('[%s] %s.', [ E.ClassName, E.Message ]);
        Result := 1;
      end;

    end;

  end;

end.