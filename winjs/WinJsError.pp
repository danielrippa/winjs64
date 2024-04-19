unit WinJsError;

{$mode delphi}

interface

  uses
    ChakraTypes, SysUtils;

  type

    EWinjsException = class(Exception)
      private
        FErrorCode: Integer;
      public
        constructor Create(aMessage: WideString; aErrorCode: Integer);
        property ErrorCode: Integer read FErrorCode;
    end;

implementation

  constructor EWinjsException.Create;
  begin
    inherited Create(aMessage);
    FErrorCode := aErrorCode;
  end;

end.