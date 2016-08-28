unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, IpHtml, Ipfilebroker, Forms, Controls, Graphics,
  Dialogs, ComCtrls, ExtCtrls, StdCtrls, Menus, ExtDlgs, PairSplitter,
  gopherclient, URIParser, strutils, IpMsg, browser;

type

  { TForm1 }

  TForm1 = class(TForm)
    EngageBtn: TButton;
    GopherAddr: TEdit;
    ImageViewer: TImage;
    ImageMenu: TPopupMenu;
    MenuBar: TMainMenu;
    GopherMenu: TMenuItem;
    ExitApp: TMenuItem;
    HelpMenu: TMenuItem;
    AboutApp: TMenuItem;
    BookmarkItem: TMenuItem;
    BMItem: TMenuItem;
    PaneSplitter: TPairSplitter;
    TreePane: TPairSplitterSide;
    ViewPane: TPairSplitterSide;
    RemoveNodeItem: TMenuItem;
    SaveItemDialog: TSaveDialog;
    SaveItem: TMenuItem;
    StatusBar: TStatusBar;
    TreeMenu: TPopupMenu;
    SaveImage: TMenuItem;
    SavePictureDialog: TSavePictureDialog;
    TreeImages: TImageList;
    GopherDataProvider: TGopherDataProvider;
    IpHtmlPanel1: TIpHtmlPanel;
    Label1: TLabel;
    TreeView1: TTreeView;
    procedure AboutAppClick(Sender: TObject);
    procedure BookmarkItemClick(Sender: TObject);
    procedure EngageBtnClick(Sender: TObject);
    procedure ExitAppClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure IpHtmlPanel1DocumentOpen(Sender: TObject);
    procedure RemoveNodeItemClick(Sender: TObject);
    procedure SaveImageClick(Sender: TObject);
    procedure SaveItemClick(Sender: TObject);
    procedure TreePaneResize(Sender: TObject);
    procedure TreeView1Click(Sender: TObject);
    procedure TreeView1Expanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
  private
    GopherItems: Array of PGopherMenu;
    BookmarkNode: TTreeNode;
    function GetPointer(MenuItem: TGopherMenu): PGopherMenu;
    procedure PopulateNode(Node: TTreeNode; host: String; port: Integer; selector: String);
    procedure PopulateNode(Node: TTreeNode; MenuItems: AGopherMenu);
    procedure SaveBookmarks;
    procedure LoadBookmarks;
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
  StatusBar.SimpleText:='Initializing...';
  GopherDataProvider:=TGopherDataProvider.Create(Self);
  IpHtmlPanel1.DataProvider:=GopherDataProvider;
  SetLength(GopherItems, 0);
  BookmarkNode:=TreeView1.Items.Add(nil, 'Bookmarks');
  BookmarkNode.ImageIndex:=0;
  BookmarkNode.SelectedIndex:=0;
  LoadBookmarks;
  root:=TreeView1.Items.Add(nil, 'gopher.veroneau.net:70');
  root.ImageIndex:=0;
  root.SelectedIndex:=0;
  PopulateNode(root, 'gopher.veroneau.net', 70, '');
  root.Expand(False);
  StatusBar.SimpleText:='Ready.';
end;

procedure TForm1.IpHtmlPanel1DocumentOpen(Sender: TObject);
begin
  GopherAddr.Text:=IpHtmlPanel1.CurURL;
  Caption:='Gopher - '+IpHtmlPanel1.Title;
end;

procedure TForm1.RemoveNodeItemClick(Sender: TObject);
begin
  if (TreeView1.Selected <> nil) and (TreeView1.Selected.Data = nil) then
    if TreeView1.Selected.Text <> 'Bookmarks' then
      TreeView1.Items.Delete(TreeView1.Selected);
end;

procedure TForm1.SaveImageClick(Sender: TObject);
begin
  if SavePictureDialog.Execute then
  begin
    ImageViewer.Picture.SaveToFile(SavePictureDialog.FileName, ExtractFileExt(SavePictureDialog.FileName));
  end;
end;

procedure TForm1.SaveItemClick(Sender: TObject);
var
  item: PGopherMenu;
  gopher: TGopherClient;
  data: TMemoryStream;
  data_ok: Boolean;
  ext: String;
  Picture: TPicture;
begin
  data_ok:=False;
  if (TreeView1.Selected <> nil) and (TreeView1.Selected.Data <> nil) then
  begin
    item := TreeView1.Selected.Data;
    Case item^.gtype of
      '0','h','9':
        begin
          gopher:=TGopherClient.Create(Self);
          gopher.SetHost(item^.host, item^.port);
          data:=gopher.SendSelector(item^.selector, False);
          gopher.Free;
          data_ok:=True;
          SaveItemDialog.FileName:=ExtractFileNameOnly(item^.selector);
          ext:=ExtractFileExt(item^.selector);
          if ext = '' then
            if item^.gtype = 'h' then
              SaveItemDialog.DefaultExt:='html'
            else
              SaveItemDialog.DefaultExt:='txt'
          else
            SaveItemDialog.DefaultExt:=ext;
        end;
      'I', 'g':
        begin
          if SavePictureDialog.Execute then
          begin
            Picture:=TPicture.Create;
            gopher:=TGopherClient.Create(Self);
            gopher.SetHost(item^.host, item^.port);
            Picture.LoadFromStream(gopher.SendSelector(item^.selector, False));
            gopher.Free;
            Picture.SaveToFile(SavePictureDialog.FileName, ExtractFileExt(SavePictureDialog.FileName));
            Picture.Free;
          end;
        end
      else
        ShowMessage('This Gopher type is currently unsupported.');
    end;
  end;
  if data_ok then
  begin
    if SaveItemDialog.Execute then
      try
        data.SaveToFile(SaveItemDialog.FileName);
      except
        StatusBar.SimpleText:='Error saving file.';
        ShowMessage('There was an error while trying to save the file.');
      end;
    data.Free;
  end;
end;

procedure TForm1.TreePaneResize(Sender: TObject);
begin
  TreeView1.Width:=TreePane.Width;
  TreeView1.Height:=TreePane.Height;
  IpHtmlPanel1.Width:=ViewPane.Width;
  IpHtmlPanel1.Height:=ViewPane.Height;
  ImageViewer.Width:=ViewPane.Width;
  ImageViewer.Height:=ViewPane.Height;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  PaneSplitter.Width:=Width;
  PaneSplitter.Height:=Height-22-19-25;
  GopherAddr.Width:=Width-Label1.Width-EngageBtn.Width-5;
  TreePaneResize(Self);
end;

procedure TForm1.EngageBtnClick(Sender: TObject);
var
  uri: TURI;
  gopher: TGopherClient;
  root: TTreeNode;
  selector: String;
begin
  if LeftStr(GopherAddr.Text, 9) <> 'gopher://' then
    GopherAddr.Text:='gopher://'+GopherAddr.Text;
  uri:=ParseURI(GopherAddr.Text, 'gopher', 70, True);
  if uri.Protocol <> 'gopher' then
  begin
    ShowMessage('This client can only view gopherspace!');
    Exit;
  end;
  ImageViewer.Visible:=False;
  if uri.Path = '' then
    uri.Path:='/1';
  selector:=RightStr(uri.Path, Length(uri.Path)-2)+uri.Document;
  StatusBar.SimpleText:='Loading '+uri.Host+':'+IntToStr(uri.Port)+'/'+selector+'...';
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
    'h': IpHtmlPanel1.OpenURL(GopherAddr.Text);
  end;
  StatusBar.SimpleText:='Ready.';
end;

procedure TForm1.AboutAppClick(Sender: TObject);
begin
  ShowMessage('Cross-platform Gopher client!');
end;

procedure TForm1.BookmarkItemClick(Sender: TObject);
var
  node: TTreeNode;
begin
  if (TreeView1.Selected <> nil) and (TreeView1.Selected.Data <> nil) then
  begin
    node:=TreeView1.Items.AddChild(BookmarkNode, TreeView1.Selected.Text);
    node.ImageIndex:=TreeView1.Selected.ImageIndex;
    node.SelectedIndex:=node.ImageIndex;
    node.Data:=TreeView1.Selected.Data;
    node.HasChildren:=TreeView1.Selected.HasChildren;
    SaveBookmarks;
  end;
end;

procedure TForm1.ExitAppClick(Sender: TObject);
begin
  Close;
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
    StatusBar.SimpleText:='Loading '+item^.name+'...';
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
          root.ImageIndex:=3;
          root.SelectedIndex:=3;
          PopulateNode(root, item^.host, item^.port, item^.selector+#9+buf);
        end;
      '9': SaveItemClick(TreeView1);
      'h':
        begin
          {gopher:=TGopherClient.Create(Self);
          gopher.SetHost(item^.host, item^.port);
          IpHtmlPanel1.Visible:=True;
          IpHtmlPanel1.SetHtmlFromStream(gopher.SendSelector(item^.selector, False));
          gopher.Free;}
          IpHtmlPanel1.OpenURL('gopher://'+item^.host+':'+IntToStr(item^.port)+'/'+item^.gtype+item^.selector);
        end;
      'I', 'g': IpHtmlPanel1.OpenURL('gopher://'+item^.host+':'+IntToStr(item^.port)+'/'+item^.gtype+ReplaceStr(item^.selector, ' ','%20'));
      'x':
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
  StatusBar.SimpleText:='Ready.';
end;

procedure TForm1.TreeView1Expanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
var
  item: PGopherMenu;
begin
  AllowExpansion:=True;
  if (Node.Data <> nil) and (Node.Count = 0) then
  begin
    item:=Node.Data;
    StatusBar.SimpleText:='Loading '+item^.name+'...';
    PopulateNode(Node, item^.host, item^.port, item^.selector);
    StatusBar.SimpleText:='Ready.';
  end;
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
  MenuItems: AGopherMenu;
begin
  gopher:=TGopherClient.Create(Self);
  gopher.SetHost(host, port);
  MenuItems:=gopher.GetMenu(selector);
  gopher.Free;
  PopulateNode(Node, MenuItems);
end;

procedure TForm1.PopulateNode(Node: TTreeNode; MenuItems: AGopherMenu);
var
  item: TTreeNode;
  MenuItem: TGopherMenu;
begin
  if Length(MenuItems) = 0 then
    Exit;
  for MenuItem in MenuItems do
    if MenuItem.gtype <> 'i' then
    begin
      item := TreeView1.Items.AddChild(Node, MenuItem.name);
      Case MenuItem.gtype of
        '0': item.ImageIndex:=1;
        '1': item.ImageIndex:=0;
        '7': item.ImageIndex:=3;
        '9': item.ImageIndex:=6;
        'h': item.ImageIndex:=2;
        'I', 'g': item.ImageIndex:=5;
      else
        item.ImageIndex:=4;
      end;
      item.SelectedIndex:=item.ImageIndex;
      item.Data:=GetPointer(MenuItem);
      if MenuItem.gtype = '1' then
        item.HasChildren:=True;
    end;
end;

procedure TForm1.SaveBookmarks;
var
  f: TMemoryStream;
  item: PGopherMenu;
  buf: String;
  i: Integer;
begin
  f:=TMemoryStream.Create;
  for i := 0 to BookmarkNode.Count-1 do
  begin
    item:=BookmarkNode.Items[i].Data;
    buf:=item^.gtype+item^.name+#9+item^.selector+#9+item^.host+#9+IntToStr(item^.port)+#10;
    f.Write(buf[1], Length(buf));
  end;
  buf:='';
  {$IFDEF DARWIN}
  f.SaveToFile(GetEnvironmentVariable('HOME')+'/.bookmarks.dat');
  {$ELSE}
  f.SaveToFile('bookmarks.dat');
  {$ENDIF}
  f.Free;
end;

procedure TForm1.LoadBookmarks;
var
  f: TMemoryStream;
  gopher: TGopherClient;
  MenuItems: AGopherMenu;
  fname: String;
begin
  {$IFDEF DARWIN}
  fname:=GetEnvironmentVariable('HOME')+'/.bookmarks.dat';
  {$ELSE}
  fname:='bookmarks.dat';
  {$ENDIF}
  if not FileExists(fname) then
    Exit;
  f:=TMemoryStream.Create;
  f.LoadFromFile(fname);
  gopher:=TGopherClient.Create(Self);
  MenuItems:=gopher.GetMenu(f);
  gopher.Free;
  f.Free;
  PopulateNode(BookmarkNode, MenuItems);
end;

end.

