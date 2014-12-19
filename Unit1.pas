unit Unit1;

interface

uses
  Windows, Messages,Math, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ComCtrls, Textures,dglopengl,AppEvnts,morphmath, ExtCtrls, Buttons, StdCtrls, model,Spin,
  XPMan, JvExControls, JvLinkLabel;

type
  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    Model1: TMenuItem;
    Load1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    about1: TMenuItem;
    StatusBar1: TStatusBar;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    N2: TMenuItem;
    OpenDialog2: TOpenDialog;
    Timer1: TTimer;
    Panel1: TPanel;
    TrackBar1: TTrackBar;
    View1: TMenuItem;
    view_1: TMenuItem;
    view_2: TMenuItem;
    view_3: TMenuItem;
    view_4: TMenuItem;
    N3: TMenuItem;
    Center1: TMenuItem;
    view_5: TMenuItem;
    view_6: TMenuItem;
    Panel2: TPanel;
    Panel3: TPanel;
    GroupBox1: TGroupBox;
    TrackBar2: TTrackBar;
    TrackBar3: TTrackBar;
    TrackBar4: TTrackBar;
    XPManifest1: TXPManifest;
    Button1: TButton;
    OpenDialog3: TOpenDialog;
    N5: TMenuItem;
    GroupBox2: TGroupBox;
    TrackBar5: TTrackBar;
    TrackBar6: TTrackBar;
    TrackBar7: TTrackBar;
    Label1: TLabel;
    N6: TMenuItem;
    Vetices1: TMenuItem;
    Depth1: TMenuItem;
    SpeedButton: TSpeedButton;
    Timer: TTimer;
    ScrollBar: TScrollBar;
    Label2: TLabel;
    JvLinkLabel: TJvLinkLabel;
    procedure about1Click(Sender: TObject);
    procedure Load1Click(Sender: TObject);
    procedure import1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ApplicationIdle(Sender: TObject; var Done: Boolean);
    procedure FormResize(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Center1Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure Panel2MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Panel2MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Panel2MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Button1Click(Sender: TObject);
    procedure N2Click(Sender: TObject);
    procedure Vetices1Click(Sender: TObject);
    procedure Lof1Click(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure ScrollBarChange(Sender: TObject);
  private
    procedure RenderScene;
  end;

var
  Form1  : TForm1;




implementation

uses Unit2, Unit3;

{$R *.dfm}



var
 OldTime : integer;
 MDrag   : Byte = 0;
 MPos    : TPoint;
 AngleX  : single = 0;
 AngleY  : single = 0;




procedure TForm1.about1Click(Sender: TObject);
begin
MessageBox(Handle, PChar('Haxe Model Viewer v0.1' + #13 + 'Created by DJOKER' +#13+'https://djokergames.wordpress.com'), PChar('About'), MB_ICONINFORMATION);
end;

procedure TForm1.Load1Click(Sender: TObject);
var
 i : integer;
 bone:TBone;
begin
// Load Model //



if OpenDialog1.Execute then
 begin

 if (mesh<>nil) then
begin
    mesh.Destroy;
    mesh:=nil;
end;

 mesh:=Tmodel.Create;
 mesh.loadh3d(OpenDialog1.FileName,ExtractFileDir(OpenDialog1.FileName)+'\Textures\');

  bone:=TBone.Create;
  bone.position:=VectorCreate(1,5,1);
  mesh.addChild(bone);

  bone:=TBone.Create;
  bone.position:=VectorCreate(1,-1,1);
  mesh.addChild(bone);


 TrackBar1.Max      := Round(          mesh.duration);
 TrackBar1.Position := 0;
 TrackBar1.OnChange(self);

end;
end;

procedure TForm1.import1Click(Sender: TObject);
var
 F    : TextFile;
 i, j : integer;
 str  : string;
begin

end;

procedure TForm1.Exit1Click(Sender: TObject);
begin
Close;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
// INIT //


isQuit := true;
glHandle := panel2.Handle;
if not glInit then
 begin
 MessageBox(Handle, 'Can''t initialize OpenGL context', 'Fatal Error', MB_ICONHAND);
 Halt;
 end;
 
OldTime := GetTimer;
isQuit  := false;
Center1.Click;
ScrollBarChange(self);
Application.OnIdle := ApplicationIdle;
end;



procedure TForm1.RenderScene;
 procedure DrawGrid;
 var
  i : integer;
 begin
 //glTex_Disable;
 glDisable(GL_LIGHTING);
 glColor3f(0.5, 0.5, 0.5);
 glBegin(GL_LINES);
 for i := -10 to 10 do
  begin
  glVertex3f(-100, 0, i * 10);
  glVertex3f( 100, 0, i * 10);
  end;
 for i := -10 to 10 do
  begin
  glVertex3f(i * 10, 0, -100);
  glVertex3f(i * 10, 0,  100);
  end;
 glEnd;
 end;

var
 Time_Delta : integer;

   ambient:array[0..3]of single;
    no_mat:array[0..3]of single;
 		mat_ambient:array[0..3]of single;
		 mat_diffuse:array[0..3]of single;
		 mat_specular:array[0..3]of single;
shine,	 mat_shininess:single;

begin
if isQuit then
 Exit;
Time_Delta := GetTimer - OldTime;
OldTime    := Time_Delta + OldTime;

glEnable(GL_DEPTH_TEST);
glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
glWidth  := Panel2.Width ;
glHeight := panel2.Height + 90;
glSetProj(45, 0.4, 1000);
glViewPort(0, 0, glWidth, glHeight);

with Offset do
 glTranslatef(-X, -Y, -Z);
glRotatef(AngleX, 1, 0, 0);
glRotatef(AngleY, 0, 1, 0);

glDisable(GL_LIGHTING);
glTex_Disable;


if view_5.Checked then
 DrawGrid;


if view_6.Checked then
 begin
 glDepthFunc(GL_ALWAYS);
 glBegin(GL_LINES);
  glColor3f(1, 0, 0); glVertex3f(0, 0, 0); glVertex3f(10,  0,  0); // OX
  glColor3f(0, 1, 0); glVertex3f(0, 0, 0); glVertex3f( 0, 10,  0); // OY
  glColor3f(0, 0, 1); glVertex3f(0, 0, 0); glVertex3f( 0,  0, 10); // OZ
 glEnd;

 glDepthFunc(GL_LESS);
 end;





   mat_shininess:=100.0;
	 ambient[0]:=0.5;
   ambient[1]:=0.5;
   ambient[2]:=0.5;
 	 glLightModelfv(GL_LIGHT_MODEL_AMBIENT,@ambient);


   shine:=0.0;
   red:=0.5;
   green:=0.5;
   blue:=0.5;

	 mat_ambient[0]:=red;
   mat_ambient[1]:=green;
   mat_ambient[2]:=blue;
   mat_ambient[3]:=1;

   mat_diffuse[0]:=red;
   mat_diffuse[1]:=green;
   mat_diffuse[2]:=blue;
   mat_diffuse[3]:=1;


   mat_specular[0]:=shine;
   mat_specular[1]:=shine;
   mat_specular[2]:=shine;
   mat_specular[3]:=shine;



		glMaterialfv(GL_FRONT_AND_BACK,GL_AMBIENT,@mat_ambient);
		glMaterialfv(GL_FRONT_AND_BACK,GL_DIFFUSE,@mat_diffuse);
		glMaterialfv(GL_FRONT_AND_BACK,GL_SPECULAR,@mat_specular);
		glMaterialfv(GL_FRONT_AND_BACK,GL_SHININESS,@mat_shininess);



if view_2.Checked then
 glPolygonMode(GL_FRONT_AND_BACK, GL_LINE)
else
 glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);

if view_3.Checked then

 glEnable(GL_LIGHTING)
else
 glDisable(GL_LIGHTING);

if view_4.Checked then
 glEnable(GL_CULL_FACE)
else
 glDisable(GL_CULL_FACE);


   RED:=TrackBar5.Position/255;
   GREEN:=TrackBar6.Position/255;
   BLUE:=TrackBar7.Position/255;


 if view_3.Checked then
begin
  
  glColor3f(RED,GREEN,BLUE);
 RenderCube (1, 50, 100,1,1,1);
end;

glCullFace(GL_FRONT);
glEnable(GL_LIGHT0);
glLight_Pos(GL_LIGHT0, VectorCreate(1, 50, 100));


glLight_Color(GL_LIGHT0,RED,green,BLUE);




if Depth1.Checked then
begin
glDepthMask(true);
end else
begin
    glDepthMask(false);

end;

if SpeedButton.Down then
 begin
   Timer.Enabled:=True;
   TrackBar1.Enabled:=False;
 end else
 begin
  Timer.Enabled:=false;
     TrackBar1.Enabled:=true;
 end;



   if Assigned(mesh) then
   begin
     mesh.Update(Time_Delta);

     mesh.render;
     mesh.rotate(DegToRad(TrackBar2.Position),DegToRad(TrackBar3.Position),DegToRad(TrackBar4.Position));
   end;


glSwap;
end;

procedure TForm1.ApplicationIdle(Sender: TObject;
  var Done: Boolean);
begin
invalidaterect(Handle, nil, false);
Done := WindowState <> wsMinimized;
end;

procedure TForm1.FormPaint(Sender: TObject);
begin
RenderScene;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
RenderScene;
end;
//=============================================

procedure TForm1.FormDestroy(Sender: TObject);
begin

glClose;
isQuit := true;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
StatusBar1.Panels[0].Text := 'FPS: ' + IntToStr(glFPS);
end;

procedure TForm1.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
 pwi : TWindowInfo;
begin
if Button = mbLeft then
 begin
 if ssShift in Shift then
  MDrag := 2
 else
  if ssCtrl in Shift then
   MDrag := 3
  else
   MDrag := 1;
 GetCursorPos(MPos);
 SetCursor(0);
 GetWindowInfo(Handle, pwi);
 with pwi.rcWindow do
  SetCursorPos(Left + (Right - Left) div 2, Top + (Bottom - Top) div 2);
 end; 
end;

procedure TForm1.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
if MDrag in [1..3] then
 begin
 MDrag := 0;
 SetCursorPos(MPos.X, MPos.Y);
 SetCursor(crDefault);
 end;
end;

var
 SubDrag : boolean = false;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
 pwi  : TWindowInfo;
 p, c : TPoint;
begin
if MDrag in [1..3] then
 begin
 GetWindowInfo(Handle, pwi);
 with pwi.rcWindow do
  p := Point(Left + (Right - Left) div 2, Top + (Bottom - Top) div 2);
 if SubDrag then
  begin
  GetCursorPos(c);
  SetCursorPos(p.X, p.Y);
  if MDrag = 1 then
   begin
   AngleX := AngleX + (c.Y - p.Y);
   AngleY := AngleY + (c.X - p.X);
   AngleX := max(min(AngleX, 90), -90);
   end
  else
   if MDrag = 2 then
    Offset.Z := min(1000, max(10, Offset.Z + (c.Y - p.Y)))
   else
    begin
    Offset.X := Offset.X - c.X + p.X;
    Offset.Y := Offset.Y + c.Y - p.Y;
    end;
  SubDrag := false;
  end
 else
  SubDrag := true;
 end;
end;

procedure TForm1.Center1Click(Sender: TObject);
begin
Offset := VectorCreate(0, 0, 100);
AngleX := 0;
AngleY := 0;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
///
if Assigned(mesh) then
begin
mesh.frame := TrackBar1.Position ;

Label1.Caption := IntToStr(TrackBar1.Position) + '/' + IntToStr(Round(mesh.duration));
end;
end;

procedure TForm1.SpeedButton3Click(Sender: TObject);
var
 i : integer;
begin

end;

procedure TForm1.Panel2MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
 pwi : TWindowInfo;
begin
if Button = mbLeft then
 begin
 if ssShift in Shift then
  MDrag := 2
 else
  if ssCtrl in Shift then
   MDrag := 3
  else
   MDrag := 1;
 GetCursorPos(MPos);
 SetCursor(0);
 GetWindowInfo(Handle, pwi);
 with pwi.rcWindow do
  SetCursorPos(Left + (Right - Left) div 2, Top + (Bottom - Top) div 2);
 end; 
end;

procedure TForm1.Panel2MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
 pwi  : TWindowInfo;
 p, c : TPoint;
begin
if MDrag in [1..3] then
 begin
 GetWindowInfo(Handle, pwi);
 with pwi.rcWindow do
  p := Point(Left + (Right - Left) div 2, Top + (Bottom - Top) div 2);
 if SubDrag then
  begin
  GetCursorPos(c);
  SetCursorPos(p.X, p.Y);
  if MDrag = 1 then
   begin
   AngleX := AngleX + (c.Y - p.Y);
   AngleY := AngleY + (c.X - p.X);
   AngleX := max(min(AngleX, 90), -90);
   end
  else
   if MDrag = 2 then
    Offset.Z := min(1000, max(10, Offset.Z + (c.Y - p.Y)))
   else
    begin
    Offset.X := Offset.X - c.X + p.X;
    Offset.Y := Offset.Y + c.Y - p.Y;
    end;
  SubDrag := false;
  end
 else
  SubDrag := true;
 end;
end;

procedure TForm1.Panel2MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin

if MDrag in [1..3] then
 begin
 MDrag := 0;
 SetCursorPos(MPos.X, MPos.Y);
 SetCursor(crDefault);
 end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
TrackBar2.Position:=0;
TrackBar3.Position:=0;
TrackBar4.Position:=0;
end;

procedure TForm1.N2Click(Sender: TObject);
var
  i:Integer;
begin
if (OpenDialog3.Execute) then
begin
  if Assigned(mesh) then
  begin

  mesh.setTexture(OpenDialog3.FileName);


  end;
end;

end;

procedure TForm1.Vetices1Click(Sender: TObject);
begin
form2.show;
end;

procedure TForm1.Lof1Click(Sender: TObject);
begin
form3.show;
end;

procedure TForm1.TimerTimer(Sender: TObject);
begin
 if Assigned(mesh) then
   begin

     TrackBar1.Position := TrackBar1.Position+1;
     if ( TrackBar1.Position>=mesh.duration-1) then
     begin
        TrackBar1.Position:=0;
     end;
       end;

    

end;

procedure TForm1.ScrollBarChange(Sender: TObject);
begin
Label2.Caption :='Animation Delay:'+ IntToStr(ScrollBar.Position) ;

timer.Interval:=ScrollBar.Position;
end;

end.
