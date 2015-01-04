unit Unit3;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Menus;

type
  TForm3 = class(TForm)
    Memo: TMemo;
    PopupMenu: TPopupMenu;
    clear1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure clear1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;
  log:TMemo;

  procedure debug(msg:string);

implementation
uses Model;

{$R *.dfm}

procedure debug(msg:string);
begin
  log.Lines.Add(msg);
end;

procedure TForm3.FormCreate(Sender: TObject);
begin
log:=Form3.Memo;

end;

procedure TForm3.clear1Click(Sender: TObject);
begin
Memo.lines.Clear;
end;

end.
