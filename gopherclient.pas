unit gopherclient;

{$mode objfpc}{$H+}{$R+}{$Q+}

interface

uses
  Classes, ssockets, SysUtils, strutils;

Type

  GopherType = (Text, Menu, CSO, Error, MacBinHex, PCBinHex, UUEncoded, Index,
                Telnet, Binary, Duplicate, Sound, Event, Calendar, HTML, TN3270,
                MIME, IMAGE, WHOIS, QUERY, GIF, WWW);

  TGopherMenu = record
    gtype: char;
    name: String;
    selector: String;
    host: String;
    port: Integer;
  end;

  AGopherMenu = Array of TGopherMenu;
  PGopherMenu = ^TGopherMenu;

  { TGopherClient }

  TGopherClient = class(TComponent)
  Private
    FHost: String;
    FPort: Integer;
    Procedure GetEntry(data: TStream; entry: TStringList);
  Public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
    Procedure SetHost(Const host: String; Const port: Integer);
    Function SendSelector(Const selector: String; Const query: String): TMemoryStream;
    Function SendSelector(Const selector: String; Const text: Boolean): TMemoryStream;
    Function GetMenu(Const selector: String): AGopherMenu;
    function GetMenu(data: TStream): AGopherMenu;
  end;

implementation

{ TGopherClient }

procedure TGopherClient.GetEntry(data: TStream; entry: TStringList);
var
  line: String;
  b: Byte;
begin
  line:='';
  Repeat
    b:=data.ReadByte;
    if (b<>10) and (b<>13) then
      line:=line+chr(b);
  until (b=10);
  entry.Clear;
  entry.Delimiter:=#9;
  entry.StrictDelimiter:=True;
  entry.DelimitedText:=line;
end;

constructor TGopherClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TGopherClient.Destroy;
begin
  inherited Destroy;
end;

procedure TGopherClient.SetHost(const host: String; const port: Integer);
begin
  FHost:=host;
  FPort:=port;
end;

function TGopherClient.SendSelector(const selector: String; const query: String
  ): TMemoryStream;
begin
  Result:=SendSelector(selector+#9+query, False);
end;

function TGopherClient.SendSelector(const selector: String; const text: Boolean
  ): TMemoryStream;
var
  sock: TInetSocket;
  buf: String;
  size: Integer;
begin
  Result:=TMemoryStream.Create;
  if text then
    Result.Write('<pre>', 5);
  buf := selector+#13#10;
  Try
    sock:=TInetSocket.Create(FHost, FPort);
    sock.Write(buf[1], Length(buf));
    buf:='';
    SetLength(buf, 4096);
    Repeat
      size:=sock.Read(buf[1], 4096);
      if (size>0) then
        Result.Write(buf[1], size);
    until (size=0);
    sock.Free;
  Except
    Result.Clear;
  end;
  if text then
    Result.Write('</pre>', 6);
  Result.Seek(0, soBeginning);
end;

function TGopherClient.GetMenu(const selector: String): AGopherMenu;
begin
  Result:=GetMenu(SendSelector(selector, False));
end;

function TGopherClient.GetMenu(data: TStream): AGopherMenu;
var
  entry: TStringList;
  i: Integer;
begin
  SetLength(Result, 0);
  if data.Size = 0 then
    Exit;
  entry:=TStringList.Create;
  Repeat
    GetEntry(data, entry);
    if (entry.Count = 4) and (Length(entry.Strings[0]) > 1) then
    begin
      i:=Length(Result);
      SetLength(Result, i+1);
      Result[i].gtype:=entry.Strings[0][1];
      Result[i].name:=RightStr(entry.Strings[0], Length(entry.Strings[0])-1);
      Result[i].selector:=entry.Strings[1];
      Result[i].host:=entry.Strings[2];
      Result[i].port:=StrToInt(entry.Strings[3]);
    end;
  until (data.Position>=data.Size);
  entry.Free;
end;

end.

