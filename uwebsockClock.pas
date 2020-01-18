unit uwebsockClock;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SynCommons, SynCrtSock, SynBidirSock;

type

  { TWebSocketProtocolClock }

  TWebSocketProtocolClock = class(TWebSocketProtocolChat)
    public
      procedure InComingMsg(Sender: THttpServerResp; const Frame: TWebSocketFrame);
  end;

  { TWebsocketClockServer }

  TWebsocketClockServer=class
    private
      fServer:TWebSocketServer;
    protected
    public
      constructor Create(const Port:string);
      destructor Destroy; override;

      procedure BroadcastMsg(const msg: RawByteString);
  end;

  TWebsocketClockincoming = procedure(const msg:RawByteString);

var
  WebsocketClockincoming : TWebsocketClockincoming = nil;

implementation

{ TWebSocketProtocolClock }

procedure TWebSocketProtocolClock.InComingMsg(Sender: THttpServerResp;
  const Frame: TWebSocketFrame);
var
  sout:RawByteString;
begin
  case Frame.opcode of
  focText, focBinary :
    if Assigned(WebsocketClockincoming) then begin
      WebsocketClockincoming(Frame.payload);
    end;
  end;
end;

{ TWebsocketClockServer }

constructor TWebsocketClockServer.Create(const Port: string);
var
  protocol:TWebSocketProtocolClock;
begin
  fServer:=TWebSocketServer.Create(Port,nil,nil,'webchatclock');
  protocol:=TWebSocketProtocolClock.Create('chatclock','');
  protocol.OnIncomingFrame:=@protocol.InComingMsg;
  fServer.WebSocketProtocols.Add(protocol);
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
