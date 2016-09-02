unit downloadmgr;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls, gopherclient;

type

  { TDownloadThread }

  TDownloadThread = class(TThread)
  private
    fStatusText: String;
    fIdx: Integer;
    fFname: String;
    gitem: PGopherMenu;
    procedure UpdateStatus;
  protected
    procedure Execute; override;
  public
    Constructor Create(CreateSuspended: boolean; const idx: Integer; const fname: String; item: PGopherMenu);
  end;

  { TDownloadManager }

  TDownloadManager = class(TForm)
    DownloadList: TListView;
    procedure FormCreate(Sender: TObject);
  private
    threads: array of TDownloadThread;
  public
    procedure AddDownload(item: PGopherMenu; const fname: String);
  end;

var
  DownloadManager: TDownloadManager;

implementation

{$R *.lfm}

{ TDownloadThread }

procedure TDownloadThread.UpdateStatus;
begin
  DownloadManager.DownloadList.Items.Item[fIdx].SubItems[0]:=fStatusText;
end;

procedure TDownloadThread.Execute;
var
  gopher: TGopherClient;
  data: TMemoryStream;
  Picture: TPicture;
begin
  fStatusText:='Downloading...';
  Synchronize(@UpdateStatus);
  gopher:=TGopherClient.Create(nil);
  gopher.SetHost(gitem^.host, gitem^.port);
  data:=gopher.SendSelector(gitem^.selector, False);
  gopher.Free;
  if (gitem^.gtype = 'I') or (gitem^.gtype = 'g') then
  begin
    Picture:=TPicture.Create;
    Picture.LoadFromStream(data);
    Picture.SaveToFile(fFname, ExtractFileExt(fFname));
    Picture.Free;
  end
  else
    data.SaveToFile(fFname);
  data.Free;
  fStatusText:='Download completed!';
  Synchronize(@UpdateStatus);
end;

constructor TDownloadThread.Create(CreateSuspended: boolean; const idx: Integer; const fname: String; item: PGopherMenu);
begin
  FreeOnTerminate:=True;
  fIdx:=idx;
  fFname:=fname;
  gitem:=item;
  inherited Create(CreateSuspended);
end;

{ TDownloadManager }

procedure TDownloadManager.FormCreate(Sender: TObject);
begin
  DownloadList.Width:=Width;
  DownloadList.Height:=Height;
end;

procedure TDownloadManager.AddDownload(item: PGopherMenu; const fname: String);
var
  listItem: TListItem;
  i: Integer;
begin
  listItem:=TListItem.Create(DownloadList.Items);
  listItem.Caption:=item^.name;
  listItem.SubItems.Add('Downloading...');
  Case item^.gtype of
    '0': listItem.ImageIndex:=1;
    '1': listItem.ImageIndex:=0;
    '7': listItem.ImageIndex:=3;
    '9': listItem.ImageIndex:=6;
    'h': listItem.ImageIndex:=2;
    'I', 'g': listItem.ImageIndex:=5;
  else
    listItem.ImageIndex:=4;
  end;
  DownloadList.Items.AddItem(listItem);
  i:=Length(threads);
  SetLength(threads, i+1);
  threads[i]:=TDownloadThread.Create(False, DownloadList.Items.Count-1, fname, item);
end;

end.

