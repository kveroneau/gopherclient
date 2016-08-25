unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, IpHtml, Ipfilebroker, Forms, Controls, Graphics,
  Dialogs, ComCtrls, ExtCtrls, StdCtrls, gopherclient, URIParser, base64;

type

  { TForm1 }

  TForm1 = class(TForm)
    EngageBtn: TButton;
    GopherAddr: TEdit;
    ImageViewer: TImage;
    ImageList1: TImageList;
    FileDataProvider: TIpFileDataProvider;
    IpHtmlPanel1: TIpHtmlPanel;
    Label1: TLabel;
    TreeView1: TTreeView;
    procedure EngageBtnClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TreeView1Click(Sender: TObject);
    procedure TreeView1Expanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure GetImage(Sender: TIpHtmlNode; const URL: String; var Picture: TPicture);
  private
    GopherItems: Array of PGopherMenu;
    function GetPointer(MenuItem: TGopherMenu): PGopherMenu;
    procedure PopulateNode(Node: TTreeNode; host: String; port: Integer; selector: String);
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormShow(Sender: TObject);
var
  root: TTreeNode;
begin
  FileDataProvider.OnGetImage:=@GetImage;
  root:=TreeView1.Items.Add(nil, 'gopher.veroneau.net:70');
  root.ImageIndex:=0;
  root.SelectedIndex:=0;
  PopulateNode(root, 'gopher.veroneau.net', 70, '');
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  TreeView1.Height:=Height-22;
  IpHtmlPanel1.Width:=Width-TreeView1.Width;
  IpHtmlPanel1.Height:=Height-22;
  ImageViewer.Width:=IpHtmlPanel1.Width;
  ImageViewer.Height:=IpHtmlPanel1.Height;
  GopherAddr.Width:=Width-Label1.Width-EngageBtn.Width-5;
end;

procedure TForm1.EngageBtnClick(Sender: TObject);
var
  uri: TURI;
  gopher: TGopherClient;
  root: TTreeNode;
  selector: String;
begin
  uri:=ParseURI(GopherAddr.Text, 'gopher', 70, True);
  if uri.Protocol <> 'gopher' then
  begin
    ShowMessage('This client can only view gopherspace!');
    Exit;
  end;
  ImageViewer.Visible:=False;
  selector:=RightStr(uri.Path, Length(uri.Path)-2)+uri.Document;
  Case uri.Path[2] of
    '0':
      begin
        gopher:=TGopherClient.Create(Self);
        gopher.SetHost(uri.Host, uri.Port);
        IpHtmlPanel1.Visible:=True;
        IpHtmlPanel1.SetHtmlFromStream(gopher.SendSelector(selector, True));
        gopher.Free;
      end;
    '1':
      begin
        root:=TreeView1.Items.Add(nil, uri.Host);
        root.ImageIndex:=0;
        root.SelectedIndex:=0;
        PopulateNode(root, uri.Host, uri.Port, selector);
      end;
  end;
end;

procedure TForm1.TreeView1Click(Sender: TObject);
var
  item: PGopherMenu;
  gopher: TGopherClient;
  MenuItems: AGopherMenu;
  data: TMemoryStream;
  entry: TGopherMenu;
  buf: String;
  root: TTreeNode;
begin
  if (TreeView1.Selected <> nil) and (TreeView1.Selected.Data <> nil) then
  begin
    ImageViewer.Visible:=False;
    item:=TreeView1.Selected.Data;
    Caption:='Gopher - '+item^.name;
    GopherAddr.Text:='gopher://'+item^.host+':'+IntToStr(item^.port)+'/'+item^.gtype+item^.selector;
    Case item^.gtype of
      '0':
        begin
          gopher:=TGopherClient.Create(Self);
          gopher.SetHost(item^.host, item^.port);
          IpHtmlPanel1.Visible:=True;
          IpHtmlPanel1.SetHtmlFromStream(gopher.SendSelector(item^.selector, True));
          gopher.Free;
        end;
      '1':
        begin
          gopher:=TGopherClient.Create(Self);
          gopher.SetHost(item^.host, item^.port);
          MenuItems:=gopher.GetMenu(item^.selector);
          gopher.Free;
          data:=TMemoryStream.Create;
          data.Write('<pre>', 5);
          for entry in MenuItems do
          begin
            buf := entry.name+#13#10;
            data.Write(buf[1], Length(buf));
          end;
          data.Write('</pre>', 6);
          data.Seek(0, soBeginning);
          IpHtmlPanel1.Visible:=True;
          IpHtmlPanel1.SetHtmlFromStream(data);
          data.Free;
        end;
      '7':
        begin
          TreeView1.Selected:=nil;
          buf:=InputBox(item^.host, item^.selector, '');
          root:=TreeView1.Items.Add(nil, buf);
          root.ImageIndex:=0;
          root.SelectedIndex:=0;
          PopulateNode(root, item^.host, item^.port, item^.selector+#9+buf);
        end;
      'h':
        begin
          gopher:=TGopherClient.Create(Self);
          gopher.SetHost(item^.host, item^.port);
          IpHtmlPanel1.Visible:=True;
          IpHtmlPanel1.SetHtmlFromStream(gopher.SendSelector(item^.selector, False));
          gopher.Free;
        end;
      'I':
        begin
          gopher:=TGopherClient.Create(Self);
          gopher.SetHost(item^.host, item^.port);
          IpHtmlPanel1.Visible:=False;
          ImageViewer.Visible:=True;
          ImageViewer.Picture.LoadFromStream(gopher.SendSelector(item^.selector, False));
          gopher.Free;
        end;
    end;
  end;
end;

procedure TForm1.TreeView1Expanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
var
  item: PGopherMenu;
begin
  AllowExpansion:=True;
  if Node.Data <> nil then
  begin
    item:=Node.Data;
    PopulateNode(Node, item^.host, item^.port, item^.selector);
  end;
end;

procedure TForm1.GetImage(Sender: TIpHtmlNode; const URL: String;
  var Picture: TPicture);
var
  data: TStringList;
  strm: TMemoryStream;
  buf: String;
begin
  strm:=TMemoryStream.Create;
  data:=TStringList.Create;
  data.Delimiter:=',';
  data.StrictDelimiter:=True;
  data.DelimitedText:=URL;
  buf:=DecodeStringBase64(data[1]);
  strm.Write(buf[1], Length(buf));
  data.Free;
  strm.Seek(0, soBeginning);
  {Picture.LoadFromStream(strm);}
  strm.Free;
end;

function TForm1.GetPointer(MenuItem: TGopherMenu): PGopherMenu;
var
  i: Integer;
begin
  New(Result);
  with MenuItem do
  begin
    Result^.gtype:=gtype;
    Result^.host:=host;
    Result^.name:=name;
    Result^.port:=port;
    Result^.selector:=selector;
  end;
  i:=Length(GopherItems);
  SetLength(GopherItems, i+1);
  GopherItems[i] := Result;
end;

procedure TForm1.PopulateNode(Node: TTreeNode; host: String; port: Integer;
  selector: String);
var
  gopher: TGopherClient;
  item: TTreeNode;
  MenuItems: AGopherMenu;
  MenuItem: TGopherMenu;
begin
  SetLength(GopherItems, 0);
  gopher:=TGopherClient.Create(Self);
  gopher.SetHost(host, port);
  MenuItems:=gopher.GetMenu(selector);
  gopher.Free;
  for MenuItem in MenuItems do
    if MenuItem.gtype <> 'i' then
    begin
      item := TreeView1.Items.AddChild(Node, MenuItem.name);
      if MenuItem.gtype <> '1' then
      begin
        item.ImageIndex:=1;
        item.SelectedIndex:=1;
        item.Data:=GetPointer(MenuItem);
      end
      else
      begin
        item.ImageIndex:=0;
        item.SelectedIndex:=0;
        item.Data:=GetPointer(MenuItem);
        item.HasChildren:=True;
      end;
    end;
end;

end.

