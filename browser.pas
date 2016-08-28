unit browser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Ipfilebroker, Iphtml, Graphics, base64, IpMsg,
  gopherclient, URIParser, strutils;

type

  { TGopherDataProvider }

  TGopherDataProvider = class(TIpCustomHtmlDataProvider)
  public
    constructor Create(AOwner : TComponent); override;
    function CheckURL(const URL : string; var ContentType : string) : Boolean; override;
    function DoGetStream(const URL: string): TStream; override;
    function GetHtmlStream(const URL : string; PostData : TIpFormDataEntity) : TStream; override;
    procedure GetImage(Sender: TIpHtmlNode; const URL: String; var Picture: TPicture); override;
  end;

implementation

{ TGopherDataProvider }

constructor TGopherDataProvider.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  HandledProtocols.Add('GOPHER');
end;

function TGopherDataProvider.CheckURL(const URL: string; var ContentType: string
  ): Boolean;
var
  uri: TURI;
begin
  uri:=ParseURI(URL, 'gopher', 70, True);
  Case uri.Path[2] of
    'h': ContentType:='text/html';
    '0': ContentType:='text/plain';
    'I': ContentType:='image/png';
    'g': ContentType:='image/gif';
  end;
  if ContentType <> '' then
    Result:=True
  else
    Result:=False;
end;

function TGopherDataProvider.DoGetStream(const URL: string): TStream;
begin
  Result:=TMemoryStream.Create;
  WriteLn('DoGetStream: ',URL);
end;

function TGopherDataProvider.GetHtmlStream(const URL: string;
  PostData: TIpFormDataEntity): TStream;
var
  uri: TURI;
  selector: String;
  gopher: TGopherClient;
begin
  uri:=ParseURI(URL, 'gopher', 70, True);
  selector:=RightStr(uri.Path, Length(uri.Path)-2)+uri.Document;
  if uri.Params <> '' then
    selector:=selector+'?'+uri.Params;
  gopher:=TGopherClient.Create(Self);
  gopher.SetHost(uri.Host, uri.Port);
  Result:=gopher.SendSelector(selector, False);
  gopher.Free;
  selector:='';
end;

procedure TGopherDataProvider.GetImage(Sender: TIpHtmlNode; const URL: String;
  var Picture: TPicture);
var
  data: TStringList;
  strm: TMemoryStream;
  buf: String;
  gopher: TGopherClient;
  uri: TURI;
begin
  Picture:=TPicture.Create;
  if LeftStr(URL, 5) = 'data:' then
  begin
    strm:=TMemoryStream.Create;
    data:=TStringList.Create;
    data.Delimiter:=',';
    data.StrictDelimiter:=True;
    data.DelimitedText:=URL;
    buf:=DecodeStringBase64(data.Strings[1]);
    strm.Write(buf[1], Length(buf));
    data.Free;
    strm.Seek(0, soBeginning);
    Picture.LoadFromStream(strm);
    strm.Free;
  end
  else
  begin
    WriteLn(ReplaceStr(URL, ' ','%20'));
    uri:=ParseURI(ReplaceStr(URL, ' ','%20'), 'gopher', 70, True);
    buf:=RightStr(uri.Path, Length(uri.Path)-2)+uri.Document;
    WriteLn(buf);
    gopher:=TGopherClient.Create(Self);
    gopher.SetHost(uri.Host, uri.Port);
    Picture.LoadFromStream(gopher.SendSelector(buf, False));
    gopher.Free;
  end;
end;

end.

