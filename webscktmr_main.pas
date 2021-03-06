unit webscktmr_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  EditBtn, MaskEdit, Spin, UniqueInstance;

type

  { TFormWebSocketTmr }

  TFormWebSocketTmr = class(TForm)
    ButtonTXT: TButton;
    ButtonFnt: TButton;
    ButtonS: TButton;
    ButtonR: TButton;
    CheckBoxMilli: TCheckBox;
    ColorButtonB: TColorButton;
    EditTXT: TEdit;
    FontDialog1: TFontDialog;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Panel1: TPanel;
    StaticTextTmr: TStaticText;
    Timer1: TTimer;
    UniqueInstance1: TUniqueInstance;
    procedure ButtonFntClick(Sender: TObject);
    procedure ButtonSClick(Sender: TObject);
    procedure ButtonRClick(Sender: TObject);
    procedure ButtonTXTClick(Sender: TObject);
    procedure ColorButtonBColorChanged(Sender: TObject);
    procedure EditTXTKeyPress(Sender: TObject; var Key: char);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure WebSckResIncoming(const msg:RawByteString);
    procedure WebSckTXTIncoming(const msg:RawByteString);
  private

  public
    procedure LoadAppConfig;
    procedure SaveAppConfig;
  end;

var
  FormWebSocketTmr: TFormWebSocketTmr;
  StartTime: single;


implementation

uses
  DefaultTranslator, DateUtils, uwebsockClock, IniFiles;

resourcestring
  rsStop = 'Stop';
  rsStart = 'Start';
  rsResetTimer = 'Reset Timer?';
  rsCloseTimer = 'Close Timer?';
  rsYes = 'Yes';
  rsNo = 'No';


var
  stime: TDateTime;
  DoReset: Boolean = False;
  lastTick, curtick: Int64;
  ye, mo, dd, hh, mm, ss, sm: Word;
  clockwebsck, clockreswebsck, webscktxt: TWebsocketClockServer;
  appconfig: string;
  WSPort: string = '57900';
  WSPortR: string = '57901';
  WSPortT: string = '57902';

{$R *.lfm}

function InitAppConfig:UnicodeString;
begin
  CreateDir(GetAppConfigDir(False));
  Result:=GetAppConfigFile(False);
end;


{ TFormWebSocketTmr }

procedure TFormWebSocketTmr.FormShow(Sender: TObject);
begin
  lastTick:=0;
  curtick:=0;
  appconfig:=InitAppConfig;
  LoadAppConfig;
  try
    clockwebsck:=TWebsocketClockServer.Create(WSPort,'WebSckClock');
    clockreswebsck:=TWebsocketClockServer.Create(WSPortR,'WebSckClockRes');
    clockreswebsck.Protocol.Incoming:=@WebSckResIncoming;
    webscktxt:=TWebsocketClockServer.Create(WSPortT,'WebSckTXT');
    webscktxt.Protocol.Incoming:=@WebSckTXTIncoming;
  except
    on e:exception do
      ShowMessage(e.Message);
  end;
  stime:=Now;
  Timer1.Enabled:=True;
  //Timer1Timer(nil);
end;

procedure TFormWebSocketTmr.Timer1Timer(Sender: TObject);
var
  CurTime: TDateTime;
  s: string;
begin
  if ButtonS.Tag<>0 then
    curtick:=MilliSecondsBetween(Now,stime);
  CurTime:=(lastTick+curtick)/3600/24/1000;
  DecodeDateTime(Curtime,ye,mo,dd,hh,mm,ss,sm);
  dd:=Trunc(CurTime);
  hh:=hh+dd*24;
  if CheckBoxMilli.Checked then
    s:=Format('%d:%.2d:%.2d.%.3d',[hh,mm,ss,sm])
    else
      s:=Format('%d:%.2d:%.2d',[hh,mm,ss]);
  StaticTextTmr.Caption:=s;
  clockwebsck.BroadcastMsg(s);
end;

procedure TFormWebSocketTmr.WebSckResIncoming(const msg: RawByteString);
var
  ResTime: TDateTime;
  s: string;
  ResTick: Int64;
  rye, rmo, rdd, rhh, rmm, rss, rsm: Word;
begin
  if ButtonS.Tag<>0 then begin
    ResTick:=MilliSecondsBetween(Now,stime);
    ResTime:=(lastTick+ResTick)/3600/24/1000;
    DecodeDateTime(ResTime,rye,rmo,rdd,rhh,rmm,rss,rsm);
    rdd:=Trunc(ResTime);
    rhh:=rhh+rdd*24;
    if CheckBoxMilli.Checked then
      s:=Format('%d:%.2d:%.2d.%.3d',[rhh,rmm,rss,rsm])
      else
        s:=Format('%d:%.2d:%.2d',[rhh,rmm,rss]);
    clockreswebsck.BroadcastMsg(msg+' '+s);
  end;
end;

procedure TFormWebSocketTmr.WebSckTXTIncoming(const msg: RawByteString);
begin
  webscktxt.BroadcastMsg(msg);
end;

procedure TFormWebSocketTmr.LoadAppConfig;
var
  inifile:TIniFile;
begin
  try
    inifile:=TIniFile.Create(appconfig);
    try
      lastTick:=inifile.ReadInt64('TIME','LASTTICK',0);
      WSPort:=inifile.ReadString('NET','PORT','57900');
      WSPortR:=inifile.ReadString('NET','PORTR','57901');
      WSPortT:=inifile.ReadString('NET','PORTT','57902');
      CheckBoxMilli.Checked:=inifile.ReadBool('TIME','SHOWMILLI',False);
      StaticTextTmr.Font.Name:=inifile.ReadString('FONT','NAME','');
      StaticTextTmr.Font.Size:=inifile.ReadInteger('FONT','SIZE',36);
      StaticTextTmr.Font.Color:=inifile.ReadInteger('FONT','COLOR',clHighlight);
      ColorButtonB.ButtonColor:=inifile.ReadInteger('FONT','BGCOLOR',clBlack);
    finally
      inifile.Free;
    end;
  except
  end;
end;

procedure TFormWebSocketTmr.SaveAppConfig;
var
  inifile:TIniFile;
begin
  try
    lastTick:=lastTick+curtick;
    inifile:=TIniFile.Create(appconfig);
    try
      inifile.WriteInt64('TIME','LASTTICK',lastTick);
      inifile.WriteString('NET','PORT',WSPort);
      inifile.WriteString('NET','PORTR',WSPortR);
      inifile.WriteString('NET','PORTT',WSPortT);
      inifile.WriteBool('TIME','SHOWMILLI',CheckBoxMilli.Checked);
      inifile.WriteString('FONT','NAME',StaticTextTmr.Font.Name);
      inifile.WriteInteger('FONT','SIZE',StaticTextTmr.Font.Size);
      inifile.WriteInteger('FONT','COLOR',StaticTextTmr.Font.Color);
      inifile.WriteInteger('FONT','BGCOLOR',ColorButtonB.ButtonColor);
    finally
      inifile.Free;
    end;
  except
  end;
end;

procedure TFormWebSocketTmr.ButtonSClick(Sender: TObject);
begin
  if ButtonS.Tag=0 then begin
    stime:=Now;
    ButtonS.Tag:=1;
    if DoReset then begin
      lastTick:=0;
      DoReset:=False;
    end;
    //Timer1.Enabled:=True;
    ButtonS.Caption:=rsStop;
  end else begin
    ButtonS.Tag:=0;
    //Timer1.Enabled:=False;
    lastTick:=lastTick+curtick;
    curtick:=0;
    ButtonS.Caption:=rsStart;
  end;
end;

procedure TFormWebSocketTmr.ButtonFntClick(Sender: TObject);
begin
  FontDialog1.Font.Name:=StaticTextTmr.Font.Name;
  FontDialog1.Font.Size:=StaticTextTmr.Font.Size;
  FontDialog1.Font.Color:=StaticTextTmr.Font.Color;
  if FontDialog1.Execute then begin
    StaticTextTmr.Font.Name:=FontDialog1.Font.Name;
    StaticTextTmr.Font.Size:=FontDialog1.Font.Size;
    StaticTextTmr.Font.Color:=FontDialog1.Font.Color;
  end;
end;

procedure TFormWebSocketTmr.ButtonRClick(Sender: TObject);
begin
  if QuestionDlg(Caption, rsResetTimer, mtConfirmation, [mrYes,rsYes, mrNo, rsNo,'IsDefault'], '')=mrYes
    then begin
    stime:=Now;
    if ButtonS.Tag=0 then
      DoReset:=True;
    lastTick:=0;
    Timer1Timer(nil);
  end;
end;

procedure TFormWebSocketTmr.ButtonTXTClick(Sender: TObject);
begin
  webscktxt.BroadcastMsg(EditTXT.Text);
  EditTXT.SelectAll;
end;

procedure TFormWebSocketTmr.ColorButtonBColorChanged(Sender: TObject);
begin
  Color:=ColorButtonB.ButtonColor;
end;

procedure TFormWebSocketTmr.EditTXTKeyPress(Sender: TObject; var Key: char);
begin
  if Key=#13 then begin
    ButtonTXT.Click;
    Key:=#0;
  end;
end;


procedure TFormWebSocketTmr.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if (ButtonS.Tag<>0) and (QuestionDlg(Caption, rsCloseTimer, mtConfirmation, [
    mrYes, rsYes, mrNo, rsNo, 'IsDefault'], '')=mrNo) then
    CanClose:=False;
end;

procedure TFormWebSocketTmr.FormDestroy(Sender: TObject);
begin
  clockwebsck.Free;
  clockreswebsck.Free;
  webscktxt.free;
  SaveAppConfig;
end;

end.

