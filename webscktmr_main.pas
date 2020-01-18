unit webscktmr_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  EditBtn, MaskEdit, Spin, UniqueInstance;

type

  { TFormWebSocketTmr }

  TFormWebSocketTmr = class(TForm)
    ButtonS: TButton;
    ButtonR: TButton;
    Panel1: TPanel;
    StaticTextTmr: TStaticText;
    Timer1: TTimer;
    UniqueInstance1: TUniqueInstance;
    procedure ButtonSClick(Sender: TObject);
    procedure ButtonRClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private

  public

  end;

var
  FormWebSocketTmr: TFormWebSocketTmr;
  StartTime: single;


implementation

uses
  DefaultTranslator, DateUtils, uwebsockClock;

resourcestring
  rsStop = 'Stop';
  rsStart = 'Start';
  rsResetTimer = 'Reset Timer?';
  rsCloseTimer = 'Close Timer?';
  rsYes = 'Yes';
  rsNo = 'No';


var
  stime: TDateTime;
  DoReset: Boolean = True;
  lastTick, curtick: Int64;
  ye, mo, dd, hh, mm, ss, sm: Word;
  clockwebsck: TWebsocketClockServer;

{$R *.lfm}

{ TFormWebSocketTmr }

procedure TFormWebSocketTmr.FormShow(Sender: TObject);
begin
  stime:=Now;
  lastTick:=0;
  clockwebsck:=TWebsocketClockServer.Create('57900');
end;

procedure TFormWebSocketTmr.Timer1Timer(Sender: TObject);
var
  CurTime: TDateTime;
  s: string;
begin
  curtick:=MilliSecondsBetween(Now,stime);
  CurTime:=(lastTick+curtick)/3600/24/1000;
  DecodeDateTime(Curtime,ye,mo,dd,hh,mm,ss,sm);
  dd:=DaysBetween(Now,stime);
  hh:=hh+dd*24;
  s:=Format('%d:%.2d:%.2d',[hh,mm,ss]);
  StaticTextTmr.Caption:=s;
  clockwebsck.BroadcastMsg(s);
end;

procedure TFormWebSocketTmr.ButtonSClick(Sender: TObject);
begin
  if ButtonS.Tag=0 then begin
    stime:=Now;
    if DoReset then begin
      lastTick:=0;
      DoReset:=False;
    end;
    Timer1.Enabled:=True;
    ButtonS.Tag:=1;
    ButtonS.Caption:=rsStop;
  end else begin
    Timer1.Enabled:=False;
    lastTick:=lastTick+curtick;
    ButtonS.Tag:=0;
    ButtonS.Caption:=rsStart;
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
end;

end.

