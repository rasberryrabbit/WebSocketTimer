unit uwebsockClock;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SynCommons, SynCrtSock, SynBidirSock;

type

  TWebsocketClockincoming = procedure(const msg:RawByteString) of object;

  { TWebSocketProtocolClock }

  TWebSocketProtocolClock = class(TWebSocketProtocolChat)
    private
      fIncoming: TWebsocketClockincoming;
    public
      procedure InComingMsg(Sender: THttpServerResp; const Frame: TWebSocketFrame);

      property Incoming:TWebsocketClockincoming read fIncoming write fIncoming;
  end;

  { TWebsocketClockServer }

  TWebsocketClockServer=class
    private
      fServer:TWebSocketServer;
      fProtocol:TWebSocketProtocolClock;
    protected
    public
      constructor Create(const Port, Name: string);
      destructor Destroy; override;

      procedure BroadcastMsg(const msg: RawByteString);

      property Protocol:TWebSocketProtocolClock read fProtocol;
  end;


implementation

{ TWebSocketProtocolClock }

procedure TWebSocketProtocolClock.InComingMsg(Sender: THttpServerResp;
  const Frame: TWebSocketFrame);
var
  sout:RawByteString;
begin
  case Frame.opcode of
  focText, focBinary :
    if Assigned(fIncoming) then begin
      fIncoming(Frame.payload);
    end;
  end;
end;

{ TWebsocketClockServer }

constructor TWebsocketClockServer.Create(const Port,Name: string);
begin
  fServer:=TWebSocketServer.Create(Port,nil,nil,Name);
  fProtocol:=TWebSocketProtocolClock.Create('chatclock','');
  fProtocol.OnIncomingFrame:=@fProtocol.InComingMsg;
  fServer.WebSocketProtocols.Add(fProtocol);
end;

destructor TWebsocketClockServer.Destroy;
begin
  fServer.Free;
  inherited Destroy;
end;

procedure TWebsocketClockServer.BroadcastMsg(const msg:RawByteString);
var
  outmsg:TWebSocketFrame;
begin
  outmsg.opcode:=focText;
  outmsg.payload:=msg;
  fServer.WebSocketBroadcast(outmsg);
end;


end.
