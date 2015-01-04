unit Model;

interface
uses Windows,SysUtils,classes,Controls,ComCtrls,ExtCtrls, Buttons, StdCtrls, Forms,math,
morphio,dialogs,morphutils,Graphics,gltexture,morphmath,ms3dtypes,
assimp,dglopengl;



type

TWeight=record
    bone :Integer;
    weight:single;
end;

    TSkin=record
    Pos:Vector3D;
    Nor:Vector3D;
    bones:array[0..3]of Integer;
    weights:array[0..3]of single;
    numBones:Integer;
    end;

T3DVertex=record
    Pos:Vector3D;
    Nor:Vector3D;
    coord0:Vector2D;
    coord1:Vector2D;
    bones:array[0..3]of TWeight;

    numBones:Integer;

    end;
P3DVertex=^T3DVertex;


PosKeyFrame=class
Pos:Vector3D;
frame:Single;

end;
RotKeyFrame=class
Rot:Quaternion;
frame:Single;

end;


Type TArrayVertex = class(TPersistent)
  private
    { Private declarations }
    FCount: Integer;
    FItems : Array of T3DVertex;
    function GetData: P3DVertex;
    function GetItem(Index: Integer): T3DVertex;
    procedure SetCount(const Value: Integer);
    procedure SetItem(Index: Integer; const Value: T3DVertex);
  public
    constructor Create;
    Destructor Destroy;   override;
    Procedure Clear;
    Procedure Add(Value: T3DVertex);
    Property Count: Integer read FCount write SetCount;
    Property Data : P3DVertex read GetData;
    Property Items[Index: Integer]: T3DVertex read GetItem write SetItem; default;
  end;

TMaterial=class
public
name:string;
texture:string;
textureDetail:string;
tex0:GLuint;
tex1:GLuint;
flags,matId:Integer;
r,g,b,a:Single;
ContainsLightmap:Boolean;
constructor Create;

procedure setTexture0(filanem:string);
procedure setTexture1(filanem:string);

end;


TEntity=class
public

position:Vector3D;
rotation:Vector3D;
scale:Vector3D;
orientarion:Quaternion;
AbsoluteTransformation:Matrix;
LocalWorld:Matrix;
WorldMatrix:Matrix;

parent:TEntity;
childs:TList;
Name:string;
parentName:string;
index:Integer;


function getRelativeTransformation():Matrix;virtual;
procedure UpdateAbsolutePosition();

procedure addChild(child:TEntity);
procedure rotate(yaw:Single; pitch:Single; roll:Single);

procedure Update(TimeDelta: integer);virtual;
procedure Render;virtual;

constructor Create;
destructor Destroy;
end;

 TAnim = record
  Name : string[16];
  s, e : integer;     
 end;

TBone=class(TEntity)
public
  PosKeys:TList;
  RotKeys:TList;
  numPosKeys:Integer;
  numRotKeys:Integer;
  numKeys:Integer;
  offMatrix:Matrix;
  localMatrix:Matrix;
  vertexList:TIntegerList;
  WeightsList:TSingleList;


  procedure render(); overload;
  constructor Create;
  destructor Destroy;
  procedure animte(movetime:Single);
  procedure  InitPose();

  procedure addPoskey(Pos:PosKeyFrame);
  procedure addRotkey(Rot:RotKeyFrame);


  procedure addKeyFrame(Pos:PosKeyFrame;Rot:RotKeyFrame);

     function FindPosition(time:Single):Integer  ;
     function FindRotation(time:Single):Integer  ;
     procedure animteRotation(movetime:Single);
     procedure animtePosition(movetime:Single);

end;

TCube=class(TEntity)
public
   procedure render(); overload;
end;


TMesh=class

public
 tris:TIntegerList;
 vertex:TArrayVertex;
 no_tris:Integer;
 name:string;
 material:TMaterial;
 positions:array of Vector3D;
 numbones:Integer;
constructor Create;
destructor Destroy;

function addVertex(v:T3DVertex):Integer;
function addFace(a,b,c:Integer):Integer;

function getVertexPosition(index:Integer):Vector3d;

function  getVertexAt(index:Integer):T3DVertex;
procedure setVertexAt(index:Integer;v:T3DVertex);


function getIndice(numface:Integer;index:Integer):Integer;




procedure render();

end;

TModel=class(TEntity)
public
  meshes:TList;
  bones:TList;
  materials:TList;
  numBones:Integer;
  framesPerSecond:Single;
  duration:Single;
  frame:Single;
  Anim    : array of TAnim;
  AnimCur : integer;
   MaxTime : integer;
   CurTime : integer;
   Root:TBone;
   copyKeys:Boolean;
   Frames  : integer;
  maxkeyFrame, lastKeyFrme:Single;
   m_GlobalInverseTransform:Matrix;
   haveBones:Boolean;
  constructor Create;
  destructor Destroy;
  function createMesh():TMesh;
  procedure render(); overload;
procedure Update(TimeDelta: integer);overload;
  
  procedure loadh3d(f,path:string);
  procedure importb3d(f,path:string);
  procedure importmsd3d(f,path:string);
  procedure importassimp(f,path:string);
   procedure importassimpanimation(f,path:string;merge:Boolean);


  procedure setTexture(filename:string);

  function getBoneAt(index:Integer):TBone;
   function getBoneByName(name:string):TBone;
   function findBoneByName(name:string):Integer;

   function getMeshAt(index:Integer):Tmesh;

   



end;


procedure RenderCube(x, y, z, Width, Height, Deapth: Single);


 function GetTimer: integer;


procedure Font_Init;


procedure Font_Free; stdcall;




function glInit: boolean;


procedure glClose;


function glDisplay(Width, Height, BPP, Freq: integer): boolean;


procedure glSwap;


procedure glSetProj(angle, zNear, zFar: single);


procedure glSetOrtho(left, top, right, bottom: single);


procedure glTex_Free(ID: gluint);

procedure glTex_Enable(ID: gluint);


procedure glTex_Disable;

procedure glBlend(ID: Byte);


procedure glLight_cfg(ID: integer);

procedure glLight_Pos(ID: integer; Pos: Vector3d);


procedure glLight_Color(ID: integer; R, G, B: single);



function glFont_Create(const Name: string): gluint;


procedure glTextOut(font: gluint; const Text: string; X, Y : Integer; sx : single = 1; sy: single = 1);

procedure glBillBoard(Pos: Vector3d; Size: single);


var
  mesh:TModel=nil;
//  log:TMemo;


  isQuit : boolean;
  glWidth,glHeight:Integer;
  Offset:Vector3D;



 Initialized : boolean = false;

 glHandle : HWND;
 DC     : HDC;
 RC     : HGLRC;

   RED,GREEN,BLUE:SINGLE;
 glActive     : boolean = true;

 glFPS    : DWORD;


implementation
uses Unit3;



var
 cFPS   : DWORD = 0;
 tFPS   : integer;
 CurTex : gluint;


 FontBase : DWORD;




function LoadTexture(FileName: String;var Texture:glUint ):boolean ;
var
  image:TGLTexture;
begin
  Result:=False;
  image:=TGLTexture.Create;
  image.LoadTexture(filename);
  image.BuildTexture(Texture);
//  Texture:= image.Texture;
  image.Destroy;
  Result:=True;

end;


 function GetTimer: integer;
var
 T, F : LARGE_INTEGER;
begin
QueryPerformanceFrequency(int64(F));
QueryPerformanceCounter(int64(T));
Result := trunc(1000 * T.QuadPart/F.QuadPart);
end;

procedure Font_Init;
const
 cs = 1/16;
var
 i: DWORD;
 cx, cy: single;
begin
FontBase := glGenLists(256);
for i := 0 to 255 do
 begin
 glNewList(FontBase + i, GL_COMPILE);
 cx := (i mod 16)/16;
 cy := (i div 16)/16;

 glBegin(GL_QUADS);
  glTexCoord2f(cx,      -cy);      glVertex2f( 0,  0);
  glTexCoord2f(cx + cs, -cy);      glVertex2f(16,  0);
  glTexCoord2f(cx + cs, -cy - cs); glVertex2f(16, 16);
  glTexCoord2f(cx,      -cy - cs); glVertex2f( 0, 16);
 glEnd;

 glTranslatef(8, 0, 0);
 glEndList;
 end;
end;

procedure Font_Free; stdcall;
begin
glDeleteLists(FontBase, 256);
end;



function glInit: boolean;
var
 pfd     : PIXELFORMATDESCRIPTOR;
 iFormat : integer;
begin
  InitOpenGL();

Result := false;
DC := GetDC(glHandle);
if DC = 0 then Exit;
FillChar(pfd, SizeOf(pfd), 0);
with pfd do
 begin
 nSize      := SizeOf(TPIXELFORMATDESCRIPTOR);
 nVersion   := 1;
 dwFlags    := PFD_DRAW_TO_WINDOW or
               PFD_SUPPORT_OPENGL or
               PFD_DOUBLEBUFFER;
 iPixelType := PFD_TYPE_RGBA;
 cColorBits := 32;
 cDepthBits := 16;
 iLayerType := PFD_MAIN_PLANE;
 end;

iFormat := ChoosePixelFormat(DC, @pfd);
if iFormat = 0 then
 Exit;

if not SetPixelFormat(DC, iFormat, @pfd) then
 Exit;

RC := wglCreateContext(DC);
if RC = 0 then Exit;
if not wglMakeCurrent(DC, RC) then
 Exit;

wglSwapIntervalEXT    := wglGetProcAddress('wglSwapIntervalEXT');
wglGetSwapIntervalEXT := wglGetProcAddress('wglGetSwapIntervalEXT');


    ReadExtensions;
    ReadImplementationProperties;

glClearColor(0, 0, 0.4, 0);
glDepthFunc(GL_LEQUAL);
glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
glEnable(GL_COLOR_MATERIAL);

Result := true;
end;

procedure glClose;
begin
wglMakeCurrent(DC, 0);
wglDeleteContext(RC);
ReleaseDC(glHandle, DC);
end;

function glDisplay(Width, Height, BPP, Freq: integer): boolean;
var
 dev  : TDeviceMode;
 res  : DWORD;
 bool : boolean;
begin
Result := false;
FillChar(dev, SizeOf(dev), 0);
dev.dmSize := sizeof(dev);
EnumDisplaySettings(nil, 0, dev);
with dev do
 begin
 dmPelsWidth        := Width;
 dmPelsHeight       := Height;
 dmBitsPerPel       := BPP;
 dmDisplayFrequency := Freq;
 dmFields := DM_BITSPERPEL or
             DM_PELSWIDTH  or
             DM_PELSHEIGHT or
             DM_DISPLAYFREQUENCY;
 res := ChangeDisplaySettings(dev, CDS_TEST or CDS_FULLSCREEN);
 if res = DISP_CHANGE_SUCCESSFUL then
  ChangeDisplaySettings(dev, CDS_FULLSCREEN);
 end;

if res <> DISP_CHANGE_SUCCESSFUL then
 begin
 bool := false;
 if Freq > 0 then
  bool := glDisplay(Width, Height, BPP, 0);
 if not bool then
  begin
  ChangeDisplaySettings(_devicemodeA(nil^), CDS_FULLSCREEN);
  Exit;
  end;
 end;

glWidth  := Width;
glHeight := Height;


MoveWindow(glHandle, 0, 0, glWidth, glHeight, false);
Result := true;
end;

procedure glSwap;
var
 t : integer;
begin
if wglGetSwapIntervalEXT <> 0 then
 wglSwapIntervalEXT(0);
glFlush;
glFinish;
SwapBuffers(DC);
cFPS := cFPS + 1;
t := GetTimer;
if t - tFPS >= 1000 then
 begin
 tFPS  := t;
 glFPS := cFPS;
 cFPS  := 0;
 end;
end;

procedure glSetProj(angle, zNear, zFar: single);
begin
glMatrixMode(GL_PROJECTION);
glLoadIdentity;
gluPerspective(angle, glWidth/glHeight, zNear, zFar);
glMatrixMode(GL_MODELVIEW);
glLoadIdentity;
end;

procedure glSetOrtho(left, top, right, bottom: single);
begin
glMatrixMode(GL_PROJECTION);
glLoadIdentity;
gluOrtho2D(left, right, bottom, top);
glMatrixMode(GL_MODELVIEW);
glLoadIdentity;
end;




procedure glTex_Free(ID: gluint);
var
 i, j : integer;
begin
if ID > 0 then
 begin
  glDeleteTextures(1, @ID)
end;
end;
procedure glTex_Enable(ID: gluint);
begin
//if CurTex <> ID then
// begin
// if CurTex = 0 then
 // glEnable(GL_TEXTURE_2D);
 glBindTexture(GL_TEXTURE_2D, ID);
 CurTex := ID;
// end;
end;

procedure glTex_Disable;
begin
glDisable(GL_TEXTURE_2D);
glBindTexture(GL_TEXTURE_2D, 0);
CurTex := 0;
end;


procedure glBlend(ID: Byte);
begin
if ID = 0 then
 glDisable(GL_BLEND)
else
 begin
 glEnable(GL_BLEND);
  case ID of
   1 : glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
   2 : glBlendFunc(GL_SRC_ALPHA, GL_ONE);
   3 : glBlendFunc(GL_ZERO, GL_SRC_COLOR);
  end;
 end;
end;

procedure glLight_cfg(ID: integer);
const
 light_position : array [0..3] of single = (1, 1, 1, 0);
 white_light    : array [0..3] of single = (1, 1, 1, 1);
begin
glLightfv(ID, GL_POSITION, @light_position);
glLightfv(ID, GL_DIFFUSE,  @white_light);
glLightfv(ID, GL_SPECULAR, @white_light);
end;

procedure glLight_Pos(ID: integer; Pos: Vector3d);
var
 p : array [0..3] of single;
begin
p[0] := Pos.X;
p[1] := Pos.Y;
p[2] := Pos.Z;
p[3] := 1;
glLightfv(ID, GL_POSITION, @p);
end;

procedure glLight_Color(ID: integer; R, G, B: single);
var
 c : array [0..3] of single;



begin
c[0] := R;
c[1] := G;
c[2] := B;
c[3] := 1;
glLightfv(ID, GL_DIFFUSE,  @c);
glLightfv(ID, GL_SPECULAR, @c);

  
end;

// TEXT //
function glFont_Create(const Name: string): gluint;
begin
 LoadTexture('data\textures\font\' + Name + '.tga',result);
end;

procedure glTextOut(font: gluint; const Text: string; X, Y : Integer; sx : single = 1; sy: single = 1);
var
 s : string;
 i : integer;
 l : integer;
begin
glEnable(GL_ALPHA_TEST);
glAlphaFunc(GL_GEQUAL, 1/255);

glListBase(FontBase);
glTex_Enable(font);
glPushMatrix;
glTranslatef(X, Y, 0);
glScalef(sx, sy, 1);
s := Text;
l := Length(s);
for i := 1 to l do
 glCallLists(1, GL_UNSIGNED_BYTE, @s[i]);
glPopMatrix;
glDisable(GL_ALPHA_TEST);
end;

procedure glBillBoard(Pos: Vector3d; Size: single);
var
 mm     : array [0..15] of single;
 v      : array [0..3] of Vector3d;
 v1, v2 : Vector3d;
begin
glGetFloatv(GL_MODELVIEW_MATRIX, @mm);
v1 := VectorScale(VectorCreate(mm[0], mm[4], mm[8]), Size);
v2 := VectorScale(VectorCreate(mm[1], mm[5], mm[9]), Size);

v[0] := VectorAdd(Pos, VectorCreate(-v1.X - v2.X, -v1.Y - v2.Y, -v1.Z - v2.Z));
v[1] := VectorAdd(Pos, VectorCreate( v1.X - v2.X,  v1.Y - v2.Y,  v1.Z - v2.Z));
v[2] := VectorAdd(Pos, VectorCreate( v1.X + v2.X,  v1.Y + v2.Y,  v1.Z + v2.Z));
v[3] := VectorAdd(Pos, VectorCreate(-v1.X + v2.X, -v1.Y + v2.Y, -v1.Z + v2.Z));

glBegin(GL_QUADS);
 glTexCoord2f(0, 1); glVertex3fv(@v[0]);
 glTexCoord2f(1, 1); glVertex3fv(@v[1]);
 glTexCoord2f(1, 0); glVertex3fv(@v[2]);
 glTexCoord2f(0, 0); glVertex3fv(@v[3]);
glEnd;
end;


procedure RenderCube(x, y, z, Width, Height, Deapth: Single);
var w,h,d: glFloat;
begin
  // Calculate the half width, deapth and height
  w:=Width  / 2;
  h:=Height / 2;
  d:=Deapth / 2;


  // Render a simple cube with the side 1
  glBegin(GL_QUADS);
    // Top Face
    glVertex3f(x+w, y+h, z-d);
    glVertex3f(x-w, y+h, z-d);
    glVertex3f(x-w, y+h, z+d);
    glVertex3f(x+w, y+h, z+d);
    // Bottom Face
   glVertex3f(x-w, y-h, z-d);
    glVertex3f(x+w, y-h, z-d);
     glVertex3f(x+w, y-h, z+d);
     glVertex3f(x-w, y-h, z+d);

    // Front Face
  glVertex3f(x-w, y-h, z+d);
   glVertex3f(x+w, y-h, z+d);
 glVertex3f(x+w, y+h, z+d);
  glVertex3f(x-w, y+h, z+d);
    // Back Face
 glVertex3f(x+w, y-h, z-d);
  glVertex3f(x-w, y-h, z-d);
    glVertex3f(x-w, y+h, z-d);
 glVertex3f(x+w, y+h, z-d);

    // Right face
   glVertex3f(x+w, y-h, z-d);
 glVertex3f(x+w, y+h, z-d);
  glVertex3f(x+w, y+h, z+d);
   glVertex3f(x+w, y-h, z+d);
    // Left Face
    glVertex3f(x-w, y-h, z-d);
     glVertex3f(x-w, y-h, z+d);
    glVertex3f(x-w, y+h, z+d);
     glVertex3f(x-w, y+h, z-d);
  glEnd();
end;

procedure drawVertexPrimitiveList(const  vertices:array of T3DVertex;vertexCount:integer; const indexList:array of integer;primitiveCount:integer);
var
vert:integer;
begin


	glEnableClientState(GL_VERTEX_ARRAY);
	glEnableClientState(GL_TEXTURE_COORD_ARRAY);
	glEnableClientState(GL_NORMAL_ARRAY);
  glVertexPointer(3, GL_FLOAT, sizeof(T3DVertex), @vertices[0].pos);
  glNormalPointer(GL_FLOAT,  sizeof(T3DVertex), @vertices[0].nor);
   glClientActiveTextureARB(GL_TEXTURE0_ARB);
    glTexCoordPointer(2, GL_FLOAT, sizeof(T3DVertex), @vertices[0].coord0);
    glEnableClientState(GL_TEXTURE_COORD_ARRAY);
    glClientActiveTextureARB(GL_TEXTURE1_ARB);
    glTexCoordPointer(2, GL_FLOAT, sizeof(T3DVertex), @vertices[0].coord1);
    glEnableClientState(GL_TEXTURE_COORD_ARRAY);
		 	glDrawElements(GL_TRIANGLES, primitiveCount*3, GL_UNSIGNED_INT, @indexList[0]);
	glDisableClientState(GL_VERTEX_ARRAY);
	glDisableClientState(GL_NORMAL_ARRAY);
	glDisableClientState(GL_TEXTURE_COORD_ARRAY);
  glActiveTextureARB(GL_TEXTURE0_ARB);
  glEnable(GL_TEXTURE_2D);


end;

constructor TMaterial.Create;
begin
  r:=1;
  g:=1;
  b:=1;
  a:=1;
  matId:=0;
  flags:=0;
  texture:='none';
  textureDetail:='none';
  tex0:=0;
  tex1:=0;


end;
procedure TMaterial.setTexture0(filanem:string);
begin
LoadTexture(filanem,tex0);
end;
procedure TMaterial.setTexture1(filanem:string);
begin
LoadTexture(filanem,tex1);
end;

constructor TMesh.Create;
begin
 tris:=TIntegerList.Create;
 no_tris:=0;
 vertex:=TArrayVertex.Create;
  material:=TMaterial.Create;
//  Wights:=TList.Create;
end;
destructor TMesh.Destroy;
begin
  tris.Destroy;
  vertex.Destroy;
//  Wights.Destroy;

end;
function TMesh.addVertex(v:T3DVertex):Integer;
begin
  Result:=  vertex.Count;
  vertex.Add(v);

  SetLength(positions,length(positions)+1);
  positions[High(positions)]:=v.Pos;


end;

function TMesh.getVertexPosition(index:Integer):Vector3d;
begin
Result:=positions[index];
end;

function TMesh.getVertexAt(index:Integer):T3DVertex;
begin
Result:=vertex[index];
end;

function TMesh.getIndice(numface:Integer;index:Integer):Integer;
begin

	result:= tris[numface * 3 + index];
end;



procedure TMesh.setVertexAt(index:Integer;v:T3DVertex);
begin
vertex[index]:=v;
end;

function TMesh.addFace(a,b,c:Integer):Integer;
begin
    Inc(no_tris);
    tris.Add(a);
    tris.Add(b);
    tris.Add(c);
    result:=no_tris;
end;
procedure TMesh.render();
var
  vert:Integer;
  Vertice:T3DVertex;
begin

  glVertexPointer(3, GL_FLOAT, sizeof(T3DVertex), @vertex.Data.Pos);
  glNormalPointer(GL_FLOAT,  sizeof(T3DVertex) ,  @vertex.Data.Nor);

  glClientActiveTextureARB(GL_TEXTURE0_ARB);
  glTexCoordPointer(2, GL_FLOAT, sizeof(T3DVertex), @vertex.data.coord0);
  glEnableClientState(GL_TEXTURE_COORD_ARRAY);
  glClientActiveTextureARB(GL_TEXTURE1_ARB);
  glTexCoordPointer(2, GL_FLOAT, sizeof(T3DVertex), @vertex.Data.coord1);
  glEnableClientState(GL_TEXTURE_COORD_ARRAY);


  // if (material.tex0<>0) then
  // begin
     glTex_Enable(material.tex0);
  // end;




   glDrawElements(GL_TRIANGLES, no_tris * 3, GL_UNSIGNED_INT, tris.List);


end;

constructor TModel.Create;
begin
   inherited create;
     meshes:=TList.Create;
      materials:=TList.Create;
        bones:=TList.Create;
        AnimCur := -1;
              haveBones:=False;
Anim    := nil;
copyKeys:=False;
lastKeyFrme:=0;
end;
destructor TModel.Destroy;
var
  i:Integer;
  BOne:TBone;

begin
   inherited Destroy;

   for i:=0 to bones.Count-1 do
   begin
   BOne:=TBone(bones[i]);
   BOne.Destroy;
   end;
    bones.Clear;
   meshes.Clear;
   materials.Clear;
   meshes.Destroy;
   materials.Destroy;
   bones.Destroy;
end;
function TModel.CreateMesh():TMesh;
var
  surf:TMesh;
begin
  surf:=TMesh.Create;
  meshes.Add(surf);
  Result:=surf;
end;


procedure TModel.setTexture(filename:string);
var
  i:Integer;
  surface:TMesh;
  tex:GLuint;
begin

  LoadTexture(filename,tex);
  for i:=0 to meshes.Count-1 do
  begin
    surface:=TMesh(meshes[i]);
    surface.material.tex0:=tex;
  end;


end;


procedure TModel.Update(TimeDelta: integer);
var
 t,m,b, v,i:Integer;
  BOne:TBone;
  subMesh:Tmesh;
  vertex:T3DVertex;

  Pos:Vector3D;
  w,x,y,z,ovx,ovy,ovz:Single;
  tform_mat:Matrix;
begin
inherited Update(timedelta);



if( haveBones) then
begin
if ( bones.Count>=1) then
begin
for i:=0 to bones.Count-1 do
begin
   BOne:=TBone(bones[i]);
   BOne.animte(frame);
end;
end;



if (meshes.Count>0) then
begin
for m:=0 to meshes.Count-1 do
begin
      

    subMesh:= getMeshAt(m);

    for v:=0 to  subMesh.vertex.Count-1 do
    begin
        vertex:=subMesh.getVertexAt(v);
      

   x:=0;
   y:=0;
   z:=0;
   ovx:=0;
   ovy:=0;
   ovz:=0;



   if (vertex.numBones=1) then
   begin

          Bone:= getBoneAt(vertex.bones[0].bone);
          Pos:=subMesh.getVertexPosition(v);


          tform_mat:=MatrixMultiply4x4(BOne.AbsoluteTransformation,BOne.offMatrix);
          w:=1;



          ovx:=pos.x;
          ovy:=pos.y;
          ovz:=pos.z;
   		     x := ( tform_mat.v[0] * ovx + tform_mat.v[4] * ovy + tform_mat.v[8] * ovz  + tform_mat.v[12] ) * w;
			     y := ( tform_mat.v[1] * ovx + tform_mat.v[5] * ovy + tform_mat.v[9] * ovz  + tform_mat.v[13] ) * w;
		 	     z := ( tform_mat.v[2] * ovx + tform_mat.v[6] * ovy + tform_mat.v[10] * ovz + tform_mat.v[14] ) * w;


   end else
   begin

        if (vertex.bones[0].bone<>-1) then
        begin

          Bone:= getBoneAt(vertex.bones[0].bone);
          Pos:=subMesh.getVertexPosition(v);


          tform_mat:=MatrixMultiply4x4(BOne.AbsoluteTransformation,BOne.offMatrix);
          w:=vertex.bones[0].weight;



          ovx:=pos.x;
          ovy:=pos.y;
          ovz:=pos.z;
   		     x := ( tform_mat.v[0] * ovx + tform_mat.v[4] * ovy + tform_mat.v[8] * ovz  + tform_mat.v[12] ) * w;
			     y := ( tform_mat.v[1] * ovx + tform_mat.v[5] * ovy + tform_mat.v[9] * ovz  + tform_mat.v[13] ) * w;
		 	     z := ( tform_mat.v[2] * ovx + tform_mat.v[6] * ovy + tform_mat.v[10] * ovz + tform_mat.v[14] ) * w;


        if (vertex.bones[1].bone<>-1) then
        begin

          Bone:= getBoneAt(vertex.bones[1].bone);


          tform_mat:=MatrixMultiply4x4(BOne.AbsoluteTransformation,BOne.offMatrix);
          w:=vertex.bones[1].weight;
          ovx:=pos.x;
          ovy:=pos.y;
          ovz:=pos.z;
   		     x :=x+ ( tform_mat.v[0] * ovx + tform_mat.v[4] * ovy + tform_mat.v[8] * ovz  + tform_mat.v[12] ) * w;
			     y :=y+ ( tform_mat.v[1] * ovx + tform_mat.v[5] * ovy + tform_mat.v[9] * ovz  + tform_mat.v[13] ) * w;
		 	     z :=z+ ( tform_mat.v[2] * ovx + tform_mat.v[6] * ovy + tform_mat.v[10] * ovz + tform_mat.v[14] ) * w;


            if (vertex.bones[2].bone<>-1) then
        begin

          Bone:= getBoneAt(vertex.bones[2].bone);


          tform_mat:=MatrixMultiply4x4(BOne.AbsoluteTransformation,BOne.offMatrix);
          w:=vertex.bones[2].weight;
          ovx:=pos.x;
          ovy:=pos.y;
          ovz:=pos.z;
   		     x :=x+ ( tform_mat.v[0] * ovx + tform_mat.v[4] * ovy + tform_mat.v[8] * ovz  + tform_mat.v[12] ) * w;
			     y :=y+ ( tform_mat.v[1] * ovx + tform_mat.v[5] * ovy + tform_mat.v[9] * ovz  + tform_mat.v[13] ) * w;
		 	     z :=z+ ( tform_mat.v[2] * ovx + tform_mat.v[6] * ovy + tform_mat.v[10] * ovz + tform_mat.v[14] ) * w;


        if (vertex.bones[3].bone<>-1) then
        begin

          Bone:= getBoneAt(vertex.bones[3].bone);



          tform_mat:=MatrixMultiply4x4(BOne.AbsoluteTransformation,BOne.offMatrix);
          w:=vertex.bones[3].weight;
          ovx:=pos.x;
          ovy:=pos.y;
          ovz:=pos.z;
   		     x :=x+ ( tform_mat.v[0] * ovx + tform_mat.v[4] * ovy + tform_mat.v[8] * ovz  + tform_mat.v[12] ) * w;
			     y :=y+ ( tform_mat.v[1] * ovx + tform_mat.v[5] * ovy + tform_mat.v[9] * ovz  + tform_mat.v[13] ) * w;
		 	     z :=z+ ( tform_mat.v[2] * ovx + tform_mat.v[6] * ovy + tform_mat.v[10] * ovz + tform_mat.v[14] ) * w;
         end;


         end;

         end;
           
             

         end;
       end;

        vertex.Pos:=VectorCreate(x,y,z) ;
        subMesh.setVertexAt(v,vertex);

    end;



end;
end;

end;


end;

procedure TModel.render();
var
 j, i:Integer;
  subMesh:TMesh;
    BOne:TBone;
    Pos:Vector3D;
     rot:Quaternion;
     keymatrix:Matrix;
begin

    glBlend(1);


  glColor4f(1,1,1,1);
	glEnableClientState(GL_VERTEX_ARRAY);
	glEnableClientState(GL_TEXTURE_COORD_ARRAY);
	glEnableClientState(GL_NORMAL_ARRAY);

    glEnable(GL_TEXTURE_2D);

  	glMatrixMode(GL_MODELVIEW);
  	glPushMatrix();
    glMultMatrixf(@AbsoluteTransformation.v[0]);
    if (meshes.Count>0) then
    begin
    for i:=0 to meshes.Count-1 do
    begin
    subMesh:=TMesh(meshes[i]);
    subMesh.render;
    end;
    end;

  glPopMatrix();
  glDisableClientState(GL_VERTEX_ARRAY);
	glDisableClientState(GL_NORMAL_ARRAY);
	glDisableClientState(GL_TEXTURE_COORD_ARRAY);
  glActiveTextureARB(GL_TEXTURE0_ARB);

  glDisable (GL_BLEND);
  glDepthMask(true);
  glDisable(GL_TEXTURE_2D);
if( haveBones) then
begin
if ( bones.Count>=1) then
begin
  	glMatrixMode(GL_MODELVIEW);
  	glPushMatrix();
    glMultMatrixf(@AbsoluteTransformation.v[0]);

for i:=0 to bones.Count-1 do
begin
    BOne:=TBone(bones[i]);
    BOne.render();
end;
	glPopMatrix();
end;
end;
   {

 for i:=0 to bones.Count-1 do
   begin
      BOne:=TBone(bones[i]);
      for j:=0 to bone.PosKeys.Count-1 do
      begin
        glMatrixMode(GL_MODELVIEW);
        glPushMatrix();
        pos:=PosKeyFrame(bone.PosKeys[j]).pos;
        rot:=RotKeyFrame(BOne.RotKeys[j]).Rot;
        glTranslatef(pos.x, pos.y,pos.z);
        keymatrix:=QuaternionMatrix(rot);
        glMultMatrixf(@ keymatrix.v[0]);
        RenderCube(0,0,0,1,1,1);
        glPopMatrix();
       end;
   end;
    }

inherited render;
end;

function TModel.getBoneAt(index:Integer):TBone;
var
  i:Integer;
  bone:TBone;
begin
 if (index<0) then index:=0;
 if (index>=bones.count) then index:=bones.Count;
 bone:=TBone(bones[index]);
 Result:=bone;



end;



function TModel.getBoneByName(name:string):TBone;
var
  i:Integer;
  bone:TBone;
begin
  Result:=nil;//TBone(TEntity(Self));
   for i:=0  to bones.Count-1 do
   begin
       bone:=TBone(bones[i]);
       if (bone.Name=name) then
       begin
       Result:=bone;
       break;
       end;
   end;

end;
function TModel.findBoneByName(name:string):Integer;
var
  i:Integer;
  bone:TBone;
begin
   Result:=-1;
   for i:=0  to bones.Count-1 do
   begin
       bone:=TBone(bones[i]);
       if (bone.Name=name) then
       begin
       Result:=i;
       break;
       end;
   end;

end;
function TModel.getMeshAt(index:Integer):TMesh;
begin
   if (index<0) then index:=0;
   if (index>=meshes.count) then index:=meshes.Count;
   Result:=TMesh(meshes[index]);

end;


procedure resetVertex(var v:T3DVertex);
var
  i:Integer;
begin
   v.numBones:=0;

   for i:=0 to 3 do
   begin
     v.bones[i].bone:=-1;
     v.bones[i].weight:=1.0;
   end;

end;

procedure QuickSort(var A: array of integer; iLo, iHi: integer) ;
 var
   Lo, Hi, Pivot, T: Integer;
 begin
   Lo := iLo;
   Hi := iHi;
   Pivot := A[(Lo + Hi) div 2];
   repeat
     while A[Lo] < Pivot do Inc(Lo) ;
     while A[Hi] > Pivot do Dec(Hi) ;
     if Lo <= Hi then
     begin
       T := A[Lo];
       A[Lo] := A[Hi];
       A[Hi] := T;
       Inc(Lo) ;
       Dec(Hi) ;
     end;
   until Lo > Hi;
   if Hi > iLo then QuickSort(A, iLo, Hi) ;
   if Lo < iHi then QuickSort(A, Lo, iHi) ;
 end;

procedure Swap(var t1, t2: TWeight);
var
  temp: TWeight;
begin
  temp := t1;
  t1 := t2;
  t2 := temp;
end;
function Compare(const a, b: TWeight): Integer;
begin
    Result:=0;
    if (a.weight < b.weight)  then Result:= -1;
    if (a.weight > b.weight)  then Result:= 1;
end;

procedure Sort(var bones:array of  TWeight);
var
  i, n: Integer;
  Swapped: Boolean;
begin
  n := Length(bones);
  repeat
    Swapped := False;
    for i := 1 to n-1 do
    begin
      if Compare(bones[i-1], bones[i])>0 then
      begin
        Swap(bones[i-1], bones[i]);
        Swapped := True;
      end;
    end;
    dec(n);
  until not Swapped;
end;


procedure addBoneToVertex(var v:T3DVertex;id:Integer;w:Single);
var
  i:Integer;
begin

for i:=0 to 3 do
begin
    if (v.bones[i].bone=-1) then
    begin
       if (w < 0.001)  then w := 0.0;
			if (w > 0.999)  then w := 1.0;
       v.bones[i].weight:=w;
       v.bones[i].bone:=id;
       Inc(v.numBones);
      Break;
    end;
end;

Sort(v.bones);

end;


  procedure TModel.importb3d(f,path:string);
  var
  stream:TMemoryStream;
  b3d_tos:Integer;
  b3d_stack:array[0..100]of Integer;
  texturesList:TStringList;
  VerticesList:array of T3DVertex;
meshid,  numVertices:Integer;
v_surf:TMesh;


  function readInt:Integer;
begin
  stream.Read(result,SizeOf(Integer));
end;
function readFloat:single;
begin
  stream.read(result,SizeOf(Single));
end;
procedure writeInt(v: integer);
begin
  stream.Write(v,sizeof(v));
end;
procedure writeFloat(v: single);
begin
  stream.Write(v,SizeOf(v));
end;

function ReadString():pchar;
var
   MaxCount: Integer;
   index:Integer;
begin
   MaxCount:=255;
  index := 0;
Result:=  GetMemory(255);
  while index<MaxCount do
  begin
    stream.Read(result[index], sizeof(char));
    Inc(index);
    if result[index-1]=#0 then
      break;
  end;
  FreeMemory(result);
end;



function ReadChunk():string;
var
  size:Integer;

begin
    SetLength(result,4);
    stream.read(result[1],4);
    size:=readInt();
    Inc(b3d_tos);
    b3d_stack[b3d_tos]:=stream.Position +size;

end;
function ChunkSize():Integer;
begin
   Result:= b3d_stack[b3d_tos] - stream.Position;
end;
procedure ExitChunk();
begin
    stream.Seek(b3d_stack[b3d_tos],soFromBeginning);
    dec(b3d_tos);
end;
function ReadStringBuffer( buffer: PChar; MaxCount: Integer): Integer;
begin
  Result := 0;
  while Result<MaxCount do
  begin
    stream.Read(buffer[Result], sizeof(char));
    Inc(result);
    if buffer[result-1]=#0 then
      break;
  end;
end;


function RemoveUnixLastPart( s: string): string;
begin
  repeat
    delete(s, length(s),1)
  until (s[length(s)]='/') or (s='');
  result := s;
end;
procedure readTEX();
var
  filename:string;
  fn:array[0..255]of char;
begin
  debug('read tex');
  while (ChunkSize()<>0) do
  begin
        ReadStringBuffer(fn,255);

        filename:=ExtractFileName(StrPas(fn));



        texturesList.Add(filename);
        readInt;//flags
        readInt;//blend
        readfloat;//x
        readfloat;//y
        readfloat;//scale
        readfloat;//scale
        readfloat;//rotation
  end;


end;
procedure readBrus();
var
  texid,i,numtexture:Integer;
  name:array[0..255]of Char;
  red,green,blue,alpha,shine,blend:Single;
  fx:Integer;
  material:TMaterial;
    tex:GLuint;
begin
    debug('read brus');
   numtexture:=readInt;



  while (ChunkSize<>0) do
  begin
           material:=TMaterial.Create;



  ReadStringBuffer(name,255);

 red:=readFloat;
 green:=readFloat;
 blue:=readFloat;
 alpha:=readFloat;
 shine:=readFloat;
 blend:=readInt;
 fx:=readInt;

           material.name:=StrPas(name);
           material.r:=red;
           material.g:=green;
           material.b:=blue;
           material.a:=alpha;

   for i:=0 to numtexture-1 do
   begin
         texid:=readInt;//texid
         if (FileExists(path+texturesList[texid])) then
         begin

           material.texture:=path+texturesList[texid];

           if (LoadTexture(path+texturesList[texid],  tex)) then
         //  if (LoadTexture('E:\delphi\H3DViewer\media\Textures\Floor02.jpg',  tex,False)) then
           begin
           material.tex0:=tex;
           glTex_Enable(tex);
           debug(IntToStr(tex));

           debug('load :'+path+texturesList[texid]+' to index '+inttostr(texid));
           end else debug('error load load :'+path+texturesList[texid]+' to index '+inttostr(texid));

         end else
         begin
           debug('error load load :'+path+texturesList[texid]+' to index '+inttostr(texid));
         end;
         
   end;
   materials.Add(material);
 end;

end;
procedure readtris(v0:Integer);
var
  ChunkName:string;
vx,i0,i1,i2,i,brushId:Integer;
vertex:T3DVertex;
//mesh:TMesh;
 mat:TMaterial;
begin
       debug('read tris');

        brushId:= readInt();//brush id


        // mesh:=CreateMesh();
        // mesh.name:='surface_'+inttostr(Self.meshes.Count);
         v_surf.material:=TMaterial(materials[brushId]);


        while (ChunkSize<>0) do
        begin
          i0:= readInt();
          i1:= readInt();
          i2:= readInt();
          v_surf.addFace(i2,i1,i0) ;



        end;



  {
  for i:=0 to Length(VerticesList)-1 do
  begin
   vertex:=VerticesList[i];
   mesh.addVertex(vertex);
  end;
 }
  

end;

procedure readVRTS();
var
  ChunkName:string;
  VerticesCount,tex_coord,texsize,flags:Integer;
  Size:Integer;
  vertex:T3DVertex;
begin



            debug('read verts');
            flags:= readInt();//flags
            tex_coord:= readInt();//tex coords set
            texsize:= readInt();//tex coords sise

            Size:= 12 + tex_coord*texsize *4;
            if (flags and 1) >0 then
            begin
               Size:=size+12;
            end;
            if (flags and 2) >0 then
            begin
            Size:=size+16;
            end;

            VerticesCount:=ChunkSize() div size;

           // Writeln(VerticesCount*Size,',',getChunkSize, ' Vertex  :',flags,',',tex_coord,',',texsize);
        //   Writeln( ' Vertex  :',flags,', uvs:',tex_coord,', size',texsize);

     //   if( VerticesCount*Size =getChunkSize) then
      //  begin

       resetVertex(vertex);

        while (ChunkSize<>0) do
        begin

       vertex.Pos.x:=readFloat();
       vertex.Pos.y:=readFloat();
       vertex.Pos.z:=readFloat();




            if (flags and 1) >0 then
            begin
            vertex.Nor.x:=readFloat();
            vertex.Nor.y:=readFloat();
            vertex.Nor.z:=readFloat();
            end;
            if (flags and 2) >0 then
            begin

            readFloat();//r
            readFloat();//g
            readFloat();//b
            readFloat();//a
            end;

             
            if(tex_coord=1) then
            begin
               if(texsize=2) then
               begin
                vertex.coord0.X:=    readFloat();//u
                vertex.coord0.y:= 1*-   readFloat();//u

               end else
               begin
                vertex.coord0.X:=    readFloat();//u
                vertex.coord0.y:= 1*-   readFloat();//u
                readFloat();//v

               end;

            end else
            begin
              if(texsize=2) then
               begin
                vertex.coord0.X:=    readFloat();//u
                vertex.coord0.y:= 1*-   readFloat();//u
                vertex.coord1.X:=    readFloat();//u
                vertex.coord1.y:= 1* -  readFloat();//u

               end else
               begin
                vertex.coord0.X :=   readFloat();//u
                vertex.coord0.y := 1*-  readFloat();//u
               readFloat();//v
                vertex.coord1.X :=   readFloat();//u
                vertex.coord1.y := 1*-  readFloat();//u
               readFloat();//v

               end;

            end;

            SetLength(VerticesList,numVertices+1);
            VerticesList[numVertices]:=vertex;
            Inc(numVertices);

            v_surf.addVertex(vertex);
       end;


  // end else
  // begin
   //  Writeln('Illegal number of vertices in B3D model');
  // end;

end;

procedure readmesh();
var
 v0, CurBrushID:Integer;
  tag:string;
begin
    debug('read mehs');
  CurBrushID:=readint;

    v_surf:=CreateMesh();
    v_surf.name:='surface_'+inttostr(Self.meshes.Count);



   while (ChunkSize<>0) do
    begin

     tag:=ReadChunk();

     if(tag='VRTS') then
     begin
        readVRTS();
     end else
     if(tag='TRIS') then
     begin
      readtris(numVertices-1);
     end;

     ExitChunk;
    end;

end;

procedure readBONE(bone:Tbone);
var
vertexId,  i:Integer;
vertexWight:Single;
  vertex:T3DVertex;
  mesh:TMesh;
begin
   debug('read bone');
   haveBones:=True;

    while(ChunkSize<>0) do
  begin
    vertexId :=readInt;
    vertexWight:=readFloat;

    bone.vertexList.Add(vertexId);
    bone.WeightsList.Add(vertexWight);

              vertex:=v_surf.getVertexAt(VertexId);
              addBoneToVertex(vertex,bone.index,vertexWight);
              v_surf.setVertexAt(VertexId,vertex);



  //  debug(FloatToStr(vertexWight));
    debug(Format('Bone (%s) vertex ID(%d) Weight(%f)',[bone.Name,vertexId,vertexWight])) ;

  end;

end;
procedure readKEYS(bone:TBone);
var
 frame, flags:Integer;
 x,y,z,w:Single;
      PosKey:PosKeyFrame;
     RotKey:RotKeyFrame;
begin
    debug('read kesy');
  flags:=readInt;



  while(ChunkSize<>0) do
  begin
    frame:=readInt;


    if ((flags and 1)>0) then
    begin
        PosKey:=PosKeyFrame.Create;
       x:=readFloat;//x
       y:=readFloat;//y
       z:=readFloat;//z
        PosKey.Pos:=VectorCreate(x,y,z);
        PosKey.frame:=frame;
        bone.addPoskey(PosKey);


      //  debug('position key , time:'+inttostr(frame));

    end;
    if ((flags and 2)>0) then
    begin
        readFloat;
        readFloat;
        readFloat;
       debug('scale key , time:'+inttostr(frame));
    end;
   if ((flags and 4)>0) then
    begin
          RotKey:=RotKeyFrame.Create;
          w:= readFloat;//w
          x:= readFloat;
          y:= readFloat;
          z:= readFloat;
          RotKey.Rot:=QuaternionCreate(x,y,z,-w);
          RotKey.frame:=frame;
          bone.addRotkey(RotKey);

         //        debug('rotation key , time:'+inttostr(frame));

    end;

  end;


end;
procedure ReadANIM() ;
var
flags:Integer;
TicksPerSecond:Single;
  begin
      debug('read anim');
    flags   :=readInt;
    Duration:=readInt;
    TicksPerSecond   :=readfloat;
    if (TicksPerSecond<=0) then TicksPerSecond:=25.0;
  //  writeln('Number of frames: ',Duration,', fps ',TicksPerSecond,', flags ',flags);

  end;
function readnode(p:TBone):Tbone;
 var
   tag:string;
   child,bone:TBone;
   name:array[0..255] of char;




   qx,qy,qz,qw:Single;
begin

        debug('read nodes');




        bone:=TBone.Create();
        bone.parent:=p;
        ReadStringBuffer(name,255);
        bone.name:=StrPas(name);

        if (bone.parent<>nil) then
        begin
         debug('load node :' +bone.name+' parent :'+bone.parent.name);
         bone.parentName:=bone.parent.Name;
         end else
         begin
          debug('load node : ' +bone.name+' parent :nul');
         end;



      bone.position.x:= readfloat;//x
      bone.position.y:= readfloat;//y
      bone.position.z:= readfloat;//z
      bone.scale.x:=  readfloat;//x
      bone.scale.y:=  readfloat;//y
      bone.scale.z:=  readfloat;//z
      bone.orientarion.W:=-readfloat;//rx
      bone.orientarion.x:=readfloat;//ry
      bone.orientarion.y:=readfloat;//rz
      bone.orientarion.z:=readfloat;//rw
      bone.index:=bones.Count;
      bone.InitPose();

      bones.Add(bone);
        
    while (ChunkSize<>0) do
  begin
    tag:=ReadChunk();
     if(tag='MESH') then
     begin
       readmesh;
       Inc(meshid);
     end else
      if(tag='BONE') then
     begin
       readBONE(bone);
     end else
     if(tag='ANIM') then
     begin
      ReadANIM();
     end else
     if(tag='KEYS') then
     begin
       readKEYS(bone);
     end else
     if(tag ='NODE') then
     begin
        child:=readnode(bone);
       
     end;

    ExitChunk;
   end;

 //  Writeln('count:',bonescount);



   Result:=bone;
end;


var

   tag:string;
v,TextureCounter,VertexId, boneid, flags,j,i:Integer;

 bone:TBone;
 vertex:T3DVertex;
 mesh:TMesh;
 Weight:Single;


begin
  b3d_tos:=0;
  texturesList:=TStringList.Create;
   numVertices:=0;
  SetLength(VerticesList,numVertices+1);



   stream:=TMemoryStream.Create();
   stream.LoadFromFile(f);

  meshid:=0;
  readChunk();
  readint;

  while (ChunkSize<>0) do
  begin
    tag:=ReadChunk();
    if (tag='TEXS') then
    begin
     readTEX();
    end else
    if(tag='BRUS') then
    begin
      readBrus();
    end else
    if(tag='NODE') then
    begin
     root:=readnode(nil);
    end;

    ExitChunk();
  end;
  ExitChunk();



  texturesList.Destroy;
  stream.Destroy;


           {

   for   i:=0 to meshes.count-1 do
  begin
        mesh:=getMeshAt(i);



    for j:=0 to bones.Count-1 do
    begin
     bone:=bones[j];
     boneid:= findBoneByName(bone.Name);

        for v:=0 to bone.WeightsList.Count-1 do
        begin
              VertexId:=        bone.vertexList[v];
              Weight:=          bone.WeightsList[v];
              vertex:=mesh.getVertexAt(VertexId);
              addBoneToVertex(vertex,boneid,Weight);
              mesh.setVertexAt(VertexId,vertex);

        end;

    end;
 end;

 }


  end;


  Function aiStringGetValue(Str:aiString): String;
Begin
  SetLength(Result, str.length);
  If str.Length>0 Then
    Move(str.Data[0], Result[1], str.length);
End;


 procedure TModel.importassimp(f,path:string);
 var

      scene:PaiScene;


function GetGroupName(GroupID: Integer): String;
begin
  Result := aiMesh_GetName(scene,GroupID);
  Result := TrimLeft(TrimRight(Result));
  If (Result='') Then
    Result := 'surface_'+IntToString(GroupID);

end;


Function FindNode(Name: String; Root:pAiNode): pAiNode;
Var
  I:Integer;
Begin
  If (aiStringGetValue(Root.mName) = Name) Then
  Begin
    Result := Root;
    Exit;
  End;

  For I:=0 To Pred(Root.mNumChildren) Do
  Begin
    Result := FindNode(Name, root.mChildren[I]);
    If Assigned(Result) Then
      Exit;
  End;

  Result := Nil;
End;


procedure recursivelyProcessNodes(scene:PaiScene;nd:PaiNode);
var
    j, n:Integer;
     mesh:PaiMesh;
     BoneParent:TBone;
     scaling:aiVector3D;
      rotation:aiQuaternion;
       position:aiVector3D;
       bone:TBone;
       mTransformation:aiMatrix4x4;

begin


             bone:=TBone.Create;
             bone.Name:= aiNode_GetName(nd);
             bone.parentName:='';
             bone.parent:=Self;

     mTransformation:=aiNode_GetTransformation(nd);
     aiDecomposeMatrix(mTransformation,scaling,rotation,position);

      bone.position.x:=position.x;
      bone.position.y:=position.y;
      bone.position.z:=position.z;
      bone.orientarion.x:=rotation.x;
      bone.orientarion.y:=rotation.y;
      bone.orientarion.z:=rotation.z;
      bone.orientarion.w:=rotation.w;

    //  debug(Format(' %f %f %f ',[position.x,position.y,position.z]));



      For J:=0 To 15 Do
      bone.localMatrix.V[J] := mTransformation.V[J];


      if(aiNode_GetParent(nd)<>nil) then
      begin
        bone.parentName:= aiNode_GetName(aiNode_GetParent(nd));
      end;



      bones.Add(bone);

    for j:=0 to bones.Count-1 do
    begin
       BoneParent:=TBone(bones[j]);
       if (BoneParent.Name=Bone.parentName) then
       begin
         Bone.parent:=BoneParent;
         Bone.InitPose();
         Break;
       end;

    end;



for n:=0 to aiNode_GetNumChildren(nd)-1 do
begin
   recursivelyProcessNodes(scene,aiNode_GetChild(nd,n));
end;

end;

var
  i0,i1,i2, I,w,VertexId,boneid, J, N:Integer;
  Weight:Single;
 nodename, S:string;
  mat:TMaterial;
  mesh:TMesh;
  vertex:T3DVertex;
  Bone:Tbone;
  flags:Integer;
  pMesh:PaiMesh;
 pos, mVertices, mNormals:aiVector3D;
 rot:aiQuaternion;
  mTexCoord:aiVector2D;
  PosKey:PosKeyFrame;
RotKey:RotKeyFrame;
 begin


   flags :=
 	aiProcess_GenSmoothNormals				Or
 //aiProcess_GenNormals or
	//aiProcess_JoinIdenticalVertices			Or   //
//	aiProcess_ImproveCacheLocality			Or
	aiProcess_LimitBoneWeights				Or
//	aiProcess_RemoveRedundantMaterials  Or
//	aiProcess_SplitLargeMeshes				Or
	aiProcess_Triangulate					Or
	aiProcess_GenUVCoords  ;//          Or
//	aiProcess_SortByPType        ;//    Or
//  aiProcess_FindDegenerates        Or //
//	aiProcess_FindInvalidData;

   scene := aiImportFile(PChar(f), flags);
  If (Scene = Nil) Then
  begin
     aiReleaseImport(scene);
     Exit;
  end;


  for i:=0 to   aiScene_GetNumMaterials(scene)-1 do
  begin
      mat:=Tmaterial.Create;
     mat.name:='Material_'+inttostr(i);
     mat.r:=1;mat.g:=1;mat.b:=1;mat.a:=1;
     mat.ContainsLightmap:=False;
     mat.texture:=aiMaterial_GetTexture(scene,i,aiTextureType_DIFFUSE);

     if (FileExists(path+mat.texture)) then
     begin
     LoadTexture(path+mat.texture,mat.tex0);
     end else
     begin
       debug(Format('Texture %s dotn exits',[path+mat.texture]));
     end;

      materials.Add(mat);
  end;


   if(aiScene_HasMeshes(scene)) then
   begin


  debug(Format('Num meshes %d ',[aiScene_GetNumMeshes(scene)]));


    For I:=0 To aiScene_GetNumMeshes(scene)-1 Do
    begin
      mesh:=createMesh;
      mesh.name:=GetGroupName(i);
      mesh.numbones:=aiMesh_GetNumBones(scene,i);



 debug(Format('Mesh %s num vertices %d  num faces %d num bones %d',[mesh.name,aiMesh_GetNumVertices(scene,i),aiMesh_GetNumFaces(scene,i),aiMesh_GetNumBones(scene,i)]));


       mesh.material:=TMaterial(materials[aiMesh_GetMaterialIndex(scene,i)]);

       for j:=0 to aiMesh_GetNumVertices(scene,i)-1 do
       begin
       resetVertex(vertex);


       mVertices:=aiMesh_Vertex(scene,i,j);
       mNormals:=aiMesh_Normal(scene,i,j);
       mTexCoord:=aiMesh_TextureCoord(scene,i,j,0);

       vertex.Pos.x:=mVertices.x;
       vertex.Pos.y:=mVertices.y;
       vertex.Pos.z:=mVertices.z;


       vertex.Nor.x:=mNormals.x;
       vertex.Nor.y:=mNormals.y;
       vertex.Nor.z:=mNormals.z;



      vertex.coord0.x:=mTexCoord.x;
      vertex.coord0.y:=mTexCoord.y;



       mesh.addVertex(vertex);

       end;

        for j:=0 to aiMesh_GetNumFaces(scene,i)-1 do
       begin




         i0:=       aiMesh_Indice(scene,i,j,0);
         i1:=       aiMesh_Indice(scene,i,j,1);
         i2:=       aiMesh_Indice(scene,i,j,2);

         mesh.addFace(i2,i1,i0);
      end;

        


    end;
  end;

   recursivelyProcessNodes(scene,aiScene_GetRootNode(scene));

    haveBones:=bones.Count>=1;

  if(aiScene_HasAnimation(scene)) then
  begin
   duration:=aiAnim_GetDuration(scene);








   for i:=0 to aiAnim_GetNumChannels(scene)-1 do
   begin


      nodename:=  aiAnim_GetChannelName(scene,i);
      Bone:=getBoneByName(nodename);
      if(Assigned(Bone)) then
      begin
        bone.numPosKeys:=aiAnim_GetNumPositionKeys(scene,i);
        bone.numRotKeys:=aiAnim_GetNumRotationKeys(scene,i);
         for j:=0 to aiAnim_GetNumPositionKeys(scene,i)-1 do
         begin
          Pos:= aiAnim_GetPositionKey(scene,i,j);
          rot:=aiAnim_GetRotationKey(scene,i,j);

          PosKey:=PosKeyFrame.Create;
          PosKey.Pos.x:=pos.x;
          PosKey.Pos.y:=pos.y;
          PosKey.Pos.z:=pos.z;
          PosKey.frame:=aiAnim_GetPositionFrame(scene,i,j);

          RotKey:=RotKeyFrame.Create;
          RotKey.Rot.X:=rot.x;
          RotKey.Rot.y:=rot.y;
          RotKey.Rot.z:=rot.z;
          RotKey.Rot.w:=rot.w;
          RotKey.frame:=aiAnim_GetRotationFrame(scene,i,j);
          bone.addKeyFrame(PosKey,RotKey);


        //    debug(Format('bone %s %f %f %f %f ',[bone.Name,pos.x,pos.y,pos.z,PosKey.frame]));



         end;
         end;
        end;
    end;

          //atach skin

    For I:=0 To aiScene_GetNumMeshes(scene)-1 Do
    begin

    mesh:=getMeshAt(i);

     for j:=0 to aiMesh_GetNumBones(scene,i)-1 do
     begin
      nodename:= aiMesh_GetBoneName(scene,i,j);
      boneid:=findBoneByName(nodename);

       Bone:=getBoneByName(nodename);
      if(Assigned(Bone)) then
      begin
      for w:=0 to aiMesh_GetNumBoneWeights(scene,I,j)-1 do
       begin
           Weight:=aiMesh_GetBoneWeight(scene,i,j,w);
           VertexId:=aiMesh_GetBoneVertexId(scene,i,j,w);

           vertex:=mesh.getVertexAt(VertexId);
           addBoneToVertex(vertex,boneid,Weight);
           mesh.setVertexAt(VertexId,vertex);


           Bone.WeightsList.Add(Weight);
           Bone.vertexList.Add(VertexId);

       end;
      end;
     end;
   end;




    aiReleaseImport(scene);
 end;

 procedure TModel.importassimpanimation(f,path:string;merge:Boolean);
var

   scene:PaiScene;



var
  i0,i1,i2, I,w,VertexId,boneid, J, N:Integer;
lastframe,  Weight:Single;
 nodename, S:string;
  mat:TMaterial;
  mesh:TMesh;
  vertex:T3DVertex;
  Bone:Tbone;
  flags:Integer;
  pMesh:PaiMesh;
 pos, mVertices, mNormals:aiVector3D;
 rot:aiQuaternion;
  mTexCoord:aiVector2D;
  PosKey:PosKeyFrame;
RotKey:RotKeyFrame;

 begin


   flags :=	aiProcess_LimitBoneWeights	;

   scene := aiImportFile(PChar(f), 0);
  If (Scene = Nil) Then
  begin
     aiReleaseImport(scene);
     Exit;
  end;









  if(merge) then
  begin
    
  duration:=duration+aiAnim_GetDuration(scene);
   for i:=0 to aiAnim_GetNumChannels(scene)-1 do
   begin
      nodename:=  aiAnim_GetChannelName(scene,i);
      Bone:=getBoneByName(nodename);



    //   debug(Format('bone %s  PosKeys%d  RotKeys%d ',[Bone.Name,aiAnim_GetNumPositionKeys(scene,i),aiAnim_GetNumRotationKeys(scene,i)]));



      if(Assigned(Bone)) then
      begin
        bone.numPosKeys:=aiAnim_GetNumPositionKeys(scene,i);
        bone.numRotKeys:=aiAnim_GetNumRotationKeys(scene,i);
         for j:=0 to aiAnim_GetNumPositionKeys(scene,i)-1 do
         begin
          Pos:=aiAnim_GetPositionKey(scene,i,j);
          rot:=aiAnim_GetRotationKey(scene,i,j);



          PosKey:=PosKeyFrame.Create;
          PosKey.Pos.x:=pos.x;
          PosKey.Pos.y:=pos.y;
          PosKey.Pos.z:=pos.z;

          PosKey.frame:=lastKeyFrme+aiAnim_GetPositionFrame(scene,i,j);

          RotKey:=RotKeyFrame.Create;
          RotKey.Rot.X:=rot.x;
          RotKey.Rot.y:=rot.y;
          RotKey.Rot.z:=rot.z;
          RotKey.Rot.w:=rot.w;
          RotKey.frame:=lastKeyFrme+aiAnim_GetRotationFrame(scene,i,j);
          bone.addKeyFrame(PosKey,RotKey);

            maxkeyFrame:=aiAnim_GetRotationFrame(scene,i,j);





         end;
         end;

        end;

          debug(Format('add frame %f real frame %f ',[RotKey.frame,maxkeyFrame]));
          lastKeyFrme:=RotKey.frame;




  end else
  begin

  duration:=aiAnim_GetDuration(scene);
 for i:=0 to aiAnim_GetNumChannels(scene)-1 do
   begin
      nodename:=  aiAnim_GetChannelName(scene,i);
      Bone:=getBoneByName(nodename);

    //   debug(Format('bone %s  PosKeys%d  RotKeys%d ',[Bone.Name,aiAnim_GetNumPositionKeys(scene,i),aiAnim_GetNumRotationKeys(scene,i)]));



      if(Assigned(Bone)) then
      begin
        bone.numPosKeys:=aiAnim_GetNumPositionKeys(scene,i);
        bone.numRotKeys:=aiAnim_GetNumRotationKeys(scene,i);
         for j:=0 to aiAnim_GetNumPositionKeys(scene,i)-1 do
         begin
          Pos:=aiAnim_GetPositionKey(scene,i,j);
          rot:=aiAnim_GetRotationKey(scene,i,j);



          PosKey:=PosKeyFrame.Create;
          PosKey.Pos.x:=pos.x;
          PosKey.Pos.y:=pos.y;
          PosKey.Pos.z:=pos.z;
          PosKey.frame:=aiAnim_GetPositionFrame(scene,i,j);

          RotKey:=RotKeyFrame.Create;
          RotKey.Rot.X:=rot.x;
          RotKey.Rot.y:=rot.y;
          RotKey.Rot.z:=rot.z;
          RotKey.Rot.w:=rot.w;
          RotKey.frame:=aiAnim_GetRotationFrame(scene,i,j);
          bone.addKeyFrame(PosKey,RotKey);





         end;
         end;
            end;
    lastKeyFrme:=RotKey.frame;

     end;







    aiReleaseImport(scene);


 end;

 procedure TModel.importmsd3d(f,path:string);
 var
    aStream : TFileStream;
     ms3dHeader     : MS3D_header;
     c ,c2         : INTEGER;


 ms3dTriangle : MS3D_Triangle;
 ms3dVertex : MS3D_vertex;
 ms3dGroup : MS3D_Group;
 ms3dMaterial : MS3D_Material;
 ms3dJoint      : MS3D_Joint;
  ms3dKeyframe   : MS3D_Keyframe;
  AnimFPS        : SINGLE;
  TotalFrames    : LONGINT;
  TempStreamPos  : INTEGER;
  JointNameList  : TStringList;
  ParentIndex, i ,j,k      : INTEGER ;

                           maxFrameCount:Integer;
                              numGroups,
                             numMaterials,
                             numTriangles,
                             numVertices         : WORD;
                             Vertices            :array of MS3D_Vertex;
                             Triangles           :array of MS3D_Triangle;
                             Groups              :array of MS3D_Group;
                                Time,
                             MinTime,
                             MaxTime             : INTEGER;
                             FMin, FMax          : Vector3D;
                             numJoints           : WORD;
                              s:string;

                            BoneParent,  bone:TBone;

Pos:PosKeyFrame;
Rot:RotKeyFrame;

keyPos:Vector3D;
keyRot:Quaternion;

mesh:TMesh;
i0,i1,i2:Integer;
v:T3DVertex;

KeyMatrix:Matrix3x3;
cube:TCube;

mat:TMaterial;
 begin
    aStream := TFileStream.Create ( f, 0 );
    aStream.Read ( ms3dheader, SizeOf ( ms3dheader ) ) ;




      aStream.Read ( numVertices, SizeOf ( NumVertices ) );
      SetLength ( Vertices, numVertices );

        FOR c := 0 TO numVertices - 1 DO
        BEGIN
          aStream.Read ( ms3dVertex, SizeOf ( ms3dVertex ) );
          Vertices [ c ].Position := ms3dvertex.Position;
          Vertices [ c ].BoneID   := ms3dvertex.BoneID
        END;

      aStream.Read ( numTriangles, SizeOf ( NumTriangles ) );
      SetLength ( Triangles, numTriangles );
      FOR c := 0 TO NumTriangles - 1 DO
        BEGIN
          aStream.Read( ms3dTriangle, SizeOf ( ms3dTriangle ) );
           Triangles [ c ].VertexIndices    := ms3dtriangle.VertexIndices;
           Triangles [ c ].S                := ms3dtriangle.S;
           Triangles [ c ].T                := ms3dtriangle.T;
           Triangles [ c ].VertexNormals    := ms3dtriangle.VertexNormals
        END ;

      aStream.Read ( numGroups, SizeOf ( NumGroups ) );
      SetLength ( Groups, numGroups );

      FOR c := 0 TO NumGroups - 1 DO
      begin
        WITH Groups [ c ] DO
          BEGIN
            aStream.Read ( ms3dgroup.flags, SizeOf ( ms3dgroup.flags ) ); //2 byte
            aStream.Read ( ms3dgroup.name,  SizeOf ( ms3dgroup.name ) );  //32 byte
            aStream.Read ( nTriangles,      SizeOf ( nTriangles ) );      //2 byte
            SetLength ( TriangleIndices, nTriangles );
            FOR c2 := 0 TO nTriangles - 1 DO
              aStream.Read ( TriangleIndices [ c2 ], SizeOf ( TriangleIndices [ c2 ] ) );
            aStream.Read( materialIndex, SizeOf ( materialIndex ) )  //2 byte
          END;
       end;

      aStream.Read ( numMaterials, SizeOf ( NumMaterials ) );

      FOR c := 0 TO NumMaterials - 1 DO
        BEGIN
          aStream.Read ( ms3dmaterial, SizeOf ( ms3dmaterial ) );


          mat:=TMaterial.Create;
          mat.matId:=c;
          mat.r:= ms3dmaterial.Ambient[0];
          mat.g:= ms3dmaterial.Ambient[1];
          mat.b:= ms3dmaterial.Ambient[2];
          mat.texture:=extractfilename( ms3dmaterial.Texture);
          mat.name:=ms3dmaterial.Name;


               if (FileExists(path+           mat.texture)) then
               begin
                LoadTexture(path+          mat.texture,mat.tex0);
                end else
                begin
                  debug('File:'+path+           mat.texture+' not found');
                end;

          materials.Add(mat);
        END;



      aStream.Read ( AnimFPS, SizeOf ( AnimFPS ) );
      aStream.Position := aStream.Position + SizeOf ( Single ); //Skip CurrentTime
      aStream.Read ( TotalFrames, SizeOf ( TotalFrames ) );
      MaxTime := round ( TotalFrames * 1000 / AnimFPS );
      aStream.Read ( numJoints, SizeOf ( NumJoints ) );

      duration:=TotalFrames;
      framesPerSecond:=AnimFPS;
      Frames:=Round(duration)-1;


        FOR c := 0 TO NumJoints-1 DO
        BEGIN
        haveBones:=True;
        aStream.Read ( ms3dJoint, SizeOf ( ms3dJoint ) );
        bone:=TBone.Create();
        bone.index:=c;
        bone.name:=strpas(ms3dJoint.name);
        bone.parentName:=strpas(ms3dJoint.ParentName);

        bone.Name:=Trim(bone.Name);
        bone.ParentName:=Trim(bone.ParentName);



        bone.position:=VectorCreate(
        ms3dJoint.Translation.X,
        ms3dJoint.Translation.y,
        ms3dJoint.Translation.z);
        bone.orientarion:=QuaternionCreate(ms3dJoint.Rotation);
        bone.parent:= getBoneByName(bone.parentName);
        bone.InitPose;




   //     debug(Format('Bone %s %f %f %f',[bone.Name,ms3dJoint.Translation.x,ms3dJoint.Translation.y,ms3dJoint.Translation.z]));






        bone.numPosKeys:=ms3dJoint.nTransKeyframes;
        bone.numRotKeys:=ms3dJoint.nRotKeyframes;



           FOR c2 := 0 TO ms3dJoint.nRotKeyframes - 1 DO
            BEGIN
              aStream.Read(ms3dKeyframe,sizeof(ms3dKeyframe));
              Rot:=RotKeyFrame.Create;
              rot.frame:=ms3dKeyframe.Time*AnimFPS-1;
              keyRot:=QuaternionCreate(ms3dKeyframe.Parameter);
              rot.Rot:=QuaternionMultiply(QuaternionCreate(ms3dJoint.Rotation),keyRot);
              bone.addRotkey(Rot);


            END;
          FOR c2 := 0 TO ms3dJoint.nTransKeyframes - 1 DO
            BEGIN
              aStream.Read( ms3dKeyframe, SizeOf ( ms3dKeyframe ) );
              Pos:=PosKeyFrame.Create;


              keyPos:= VectorCreate(
              ms3dKeyframe.Parameter.x,
              ms3dKeyframe.Parameter.y,
              ms3dKeyframe.Parameter.z);

             Pos.Pos:= VectorCreate(
              keyPos.x+ms3dJoint.Translation.X,
              keyPos.y+ms3dJoint.Translation.y,
              keyPos.z+ms3dJoint.Translation.z);



              Pos.frame:=ms3dKeyframe.Time *AnimFPS-1;
              bone.addPoskey(pos);

            END;



          bones.Add(bone);


        END ;


   for i:=0 to bones.Count-1 do
    begin
      bone:=TBone(bones[i]);

      if (Assigned(bone.parent)) then
      begin
        debug(Format('Bone %s parent of %s',[bone.Name,bone.parent.Name]));
      end  else
      begin
              debug(Format('erro Bone %s parent of %s',[bone.Name,bone.parentName]));
      end;




    end;


    aStream.Free;


FOR i := 0 TO numGroups - 1 DO
begin

   mesh:=createMesh;


   mesh.material:=TMaterial(materials[Groups[i].MaterialIndex]);

  for j:=0 to   Groups[i].nTriangles-1 do
  begin

    resetVertex(v);
    index := Triangles[Groups[i].TriangleIndices[j]].VertexIndices[0];
    v.Pos.x:= Vertices[ index].Position.x;
    v.Pos.y:= Vertices[ index].Position.y;
    v.Pos.z:= Vertices[ index].Position.z;
    v.Nor:= Triangles[Groups[i].TriangleIndices[j]].VertexNormals[0];
    v.coord0:=VectorCreate2D( Triangles[Groups[i].TriangleIndices[j]].S[0],
                              1*-Triangles[Groups[i].TriangleIndices[j]].t[0]);
    addBoneToVertex(v,Vertices[ index].BoneID,1);
    i0:=  mesh.addVertex(v);

     resetVertex(v);
    index := Triangles[Groups[i].TriangleIndices[j]].VertexIndices[1];
     v.Pos.x:= Vertices[ index].Position.x;
    v.Pos.y:= Vertices[ index].Position.y;
    v.Pos.z:= Vertices[ index].Position.z;
   v.Nor:= Triangles[Groups[i].TriangleIndices[j]].VertexNormals[1];
    v.coord0:=VectorCreate2D( Triangles[Groups[i].TriangleIndices[j]].S[1],
                              1*-Triangles[Groups[i].TriangleIndices[j]].t[1]);
     addBoneToVertex(v,Vertices[ index].BoneID,1);
     i1:=  mesh.addVertex(v);

          resetVertex(v);
    index := Triangles[Groups[i].TriangleIndices[j]].VertexIndices[2];
     v.Pos.x:= Vertices[ index].Position.x;
    v.Pos.y:= Vertices[ index].Position.y;
    v.Pos.z:= Vertices[ index].Position.z;
   v.Nor:= Triangles[Groups[i].TriangleIndices[j]].VertexNormals[2];
    v.coord0:=VectorCreate2D( Triangles[Groups[i].TriangleIndices[j]].S[2],
                              1*-Triangles[Groups[i].TriangleIndices[j]].t[2]);
   addBoneToVertex(v,Vertices[ index].BoneID,1);
   i2:=  mesh.addVertex(v);


      mesh.addFace(i0,i2,i1);
  end;
end;




 end;

procedure TModel.loadh3d(f,path:string);
var
   data:LFileStream;
 bonename, name, headername:string;
 boneid,  v, a,b,c,i2,CountVertices,CountTriangles,CounTMeshs,flags,uvs,   i,header,nummaterials:Integer;
    j, matid:Integer;
    mesh:TMesh;
   time, x,y,z,nx,ny,nz,tv,tu,tv2,tu2:Single;
    material:TMaterial;
    vertex:T3DVertex;
    Bone,BoneParent:TBone;
    PosKey:PosKeyFrame;
    RotKey:RotKeyFrame;

    VertexId:Integer;
    Weight:Single;
    NumWeights:Integer;



begin



   data:=LFileStream.Open(f,smRead);
   headername:=   data.ReadChar;
   header:=   data.readint;
   nummaterials:=  data.readint;

  // log.Add('Header:'+headername);
   //log.Add('HeaderID:'+inttostr(header));
  // log.Add('Numer of materials:'+inttostr(nummaterials));



   for i:=0 to nummaterials-1 do
   begin
     material:=TMaterial.Create;
     material.matId:=i;
     material.flags:=data.readint();
     material.r:=data.readfloat;
     material.g:=data.readfloat;
     material.b:=data.readfloat;
     material.a:=data.readfloat;
     material.texture:=ExtractFileName(data.ReadChar);


     // ShowMessage(path+ material.texture);
     if (FileExists(path+ material.texture)) then
     begin
       LoadTexture(path+ material.texture,material.tex0);
     end;


     uvs:=data.readint();
     if(uvs=2) then
     begin
       material.ContainsLightmap:=True;
       material.textureDetail:=data.ReadChar;


     if (FileExists(path+ material.textureDetail)) then
     begin
       LoadTexture(path+ material.textureDetail,material.tex1);
     end;
     end ;

       materials.Add(material);
   end;


   CounTMeshs:=data.readint();


  for   i:=0 to CounTMeshs-1 do
  begin

      name:=data.ReadChar;
      flags:=data.readint;
      matid:=data.readint;
      CountVertices:=data.readint;
      CountTriangles:=data.readint();
      uvs:=data.readint;



       mesh:=CreateMesh();
       mesh.name:=name;

       mesh.material:=TMaterial(materials[matid]);



           for i2:=0 to CountVertices-1 do
           begin
               x:=data.readfloat;
               y:=data.readfloat;
               z:=data.readfloat;
               nx:=data.readfloat;
               ny:=data.readfloat;
               nz:=data.readfloat;
               tu:=data.readfloat;
               tv:=data.readfloat;
               tu2:=tu;
               tv2:=tv;
               if(uvs=2) then
               begin
               tu2:=data.readfloat;
               tv2:=data.readfloat;
               end;


               resetVertex(vertex);
               vertex.Pos:=VectorCreate(x,y,z);
               vertex.Nor:=VectorCreate(nx,ny,nz);
               vertex.coord0:=VectorCreate2d(tu,tv);
               vertex.coord1:=VectorCreate2d(tu2,tv2);

               mesh.addVertex(vertex);


           end;
             for i2:=0 to  CountTriangles-1 do
             begin
                a:=data.readint;
                b:=data.readint;
                c:=data.readint;
                mesh.addFace(c,b,a)
             end;

  end;

 numBones:= data.readint;
 if (numBones<>0) then
 begin

    framesPerSecond:=data.readfloat;
    duration:=data.readfloat;

    Frames:=Round(duration)-1;
    MaxTime := Frames * 100 div 2;
    AnimCur:=0;

        haveBones:=True;


    for i:=0 to numBones-1 do
    begin
          Bone:=TBone.Create;

          bone.Name:=data.ReadChar();
          bone.parentName:=data.ReadChar();
         // BOne.numPosKeys:=data.readint;
        //  BOne.numRotKeys:=data.readint;

          BOne.numKeys:=data.readint;
          BOne.position.x:=data.readfloat;
          BOne.position.y:=data.readfloat;
          BOne.position.z:=data.readfloat;

          BOne.orientarion.X:=data.readfloat;
          BOne.orientarion.y:=data.readfloat;
          BOne.orientarion.Z:=data.readfloat;
          BOne.orientarion.w:=data.readfloat;






        for j:=0 to  BOne.numKeys-1 do
        begin

            Poskey:=PosKeyFrame.Create;
            RotKey:=RotKeyFrame.Create;


            time:=data.readfloat;

            Poskey.frame:=time;
            RotKey.frame:=time;


            Poskey.Pos.X:= data.readfloat;
            Poskey.Pos.Y:= data.readfloat;
            Poskey.Pos.Z:= data.readfloat;


             


            RotKey.Rot.X:=data.readfloat;
            RotKey.Rot.y:=data.readfloat;
            RotKey.Rot.Z:=data.readfloat;
            RotKey.Rot.w:=data.readfloat;

            Bone.addKeyFrame(PosKey,Rotkey);



        end;

 
      Bone.parent:=Self;
      bones.Add(bone);

    for j:=0 to bones.Count-1 do
    begin
       BoneParent:=TBone(bones[j]);
       if (BoneParent.Name=Bone.parentName) then
       begin
         Bone.parent:=BoneParent;
         Bone.InitPose();
         Break;
       end;

    end;


    end;





    for   i:=0 to meshes.count-1 do
  begin
    numBones:= data.readint;

    mesh:=getMeshAt(i);

  //  log.Lines.Add(Format('Mesh (%s)',[mesh.name])) ;


    for j:=0 to numBones-1 do
    begin
        BOneName:=data.ReadChar;
        NumWeights:=data.readint;


       boneid:= findBoneByName(bonename);
       Bone:=bones[boneid];

     // log.Lines.Add(Format('Bone (%s) id(%d) NumWeights(%d)',[BOneName,boneid,NumWeights])) ;


        for v:=0 to NumWeights-1 do
        begin
              VertexId:=data.readint;
              Weight:=data.readfloat;
              vertex:=mesh.getVertexAt(VertexId);
              addBoneToVertex(vertex,boneid,Weight);
              mesh.setVertexAt(VertexId,vertex);
              Bone.vertexList.Add(VertexId);
              Bone.WeightsList.Add(Weight);

        end;

    end;


  end;

 end;


 data.Destroy;

end;


procedure TCube.render();
var
  i:Integer;
  pos, vector , parentvector : Vector3D;
 rot:Quaternion;
 keymatrix:Matrix;
begin

  	  glMatrixMode(GL_MODELVIEW);
    	glPushMatrix();
      glMultMatrixf(@ AbsoluteTransformation.v[0]);
      glColor3f(random,random,random);
      RenderCube(0,0,0,1,1,1);
    	glPopMatrix();

    debug('render');



	vector.x := 0;
	vector.y := 0;
	vector.z := 0;
 vector:= AbsoluteTransformation.Transform(vector);


	if( parent <> nil ) then
	begin
		parentvector.x := 0;
		parentvector.y := 0;
		parentvector.z := 0;

     parentvector:= parent.AbsoluteTransformation.Transform(parentvector);

	end;

	glDisable( GL_TEXTURE_2D );

	// render bone as a line
	glLineWidth(3.0);
	glColor3f(0, 1, 1);
	glBegin(GL_LINES);
    glVertex3f( vector.x, vector.y, vector.z );
    if( parent <> nil ) then
      glVertex3f( parentvector.x, parentvector.y, parentvector.z )
    else
      glVertex3f( vector.x, vector.y, vector.z );

	glEnd();


	glPointSize(5.0);
	glColor3f(1.0, 0, 1.0);
	glBegin(GL_POINTS);
	glVertex3f( vector.x, vector.y, vector.z );
	if( parent <> nil ) then
		glVertex3f( parentvector.x, parentvector.y, parentvector.z );
	glEnd();
	glColor3f(1.0, 1.0, 1.0);


  	glLineWidth(1.0);
inherited render;


end;

procedure TBone.render();
var
  i:Integer;
  pos, vector , parentvector : Vector3D;
 rot:Quaternion;
 keymatrix:Matrix;
begin
     {
     glMatrixMode(GL_MODELVIEW);
    	glPushMatrix();

    for i:=0 to PosKeys.Count-1 do
    begin

     if (Assigned(parent)) then
     begin
      keymatrix:=MatrixMultiply4x4(parent.AbsoluteTransformation,makeTransform(PosKeyFrame(PosKeys[i]).pos, VectorOne, RotKeyFrame(RotKeys[i]).Rot));
     end else
     begin
      keymatrix:=makeTransform(PosKeyFrame(PosKeys[i]).pos, VectorOne, RotKeyFrame(RotKeys[i]).Rot);
     end;
     glMultMatrixf(@ keymatrix.v[0]);
      glColor3f(0,1,1);
       RenderCube(0,0,0,1,1,1);
     end;
   	glPopMatrix();
   }

      {
  	  glMatrixMode(GL_MODELVIEW);
    	glPushMatrix();
      glMultMatrixf(@ AbsoluteTransformation.v[0]);
      	glColor3f(random,random,random);
      RenderCube(0,0,0,1,1,1);
    	glPopMatrix();
       }




	vector.x := 0;
	vector.y := 0;
	vector.z := 0;
 vector:= AbsoluteTransformation.Transform(vector);


	if( parent <> nil ) then
	begin
		parentvector.x := 0;
		parentvector.y := 0;
		parentvector.z := 0;

     parentvector:= parent.AbsoluteTransformation.Transform(parentvector);

	end;

	glDisable( GL_TEXTURE_2D );

	// render bone as a line
	glLineWidth(3.0);
	glColor3f(1, 1, 0);
	glBegin(GL_LINES);
    glVertex3f( vector.x, vector.y, vector.z );
    if( parent <> nil ) then
      glVertex3f( parentvector.x, parentvector.y, parentvector.z )
    else
      glVertex3f( vector.x, vector.y, vector.z );

	glEnd();


	glPointSize(5.0);
	glColor3f(1.0, 0, 0);
	glBegin(GL_POINTS);
	glVertex3f( vector.x, vector.y, vector.z );
	if( parent <> nil ) then
		glVertex3f( parentvector.x, parentvector.y, parentvector.z );
	glEnd();
	glColor3f(1.0, 1.0, 1.0);


  	glLineWidth(1.0);

   inherited render;



end;


constructor TBone.Create;
begin
inherited Create;
  PosKeys:=TList.Create;
  RotKeys:=TList.Create;
   vertexList:=TIntegerList.Create;
  WeightsList:=TSingleList.Create;
   localMatrix:=MatrixIdentity;
end;
destructor TBone.Destroy;
begin
 inherited Destroy();
          PosKeys.Clear;
          RotKeys.Clear;
   PosKeys.Destroy;
   RotKeys.Destroy;
   vertexList.Clear;
   vertexList.Destroy;
   WeightsList.Clear;
   WeightsList.Destroy;

   PosKeys:=nil;
   RotKeys:=nil;
end;
procedure  Tbone.InitPose();
begin

UpdateAbsolutePosition();
offMatrix:=MatrixInverse(AbsoluteTransformation);



end;


  procedure TBone.addKeyFrame(Pos:PosKeyFrame;Rot:RotKeyFrame);
  begin
          PosKeys.Add(Pos);
          RotKeys.Add(Rot);
  end;
    procedure TBone.addPoskey(Pos:PosKeyFrame);
    begin
          PosKeys.Add(Pos);
    end;
   procedure TBone.addRotkey(Rot:RotKeyFrame);
   begin
          RotKeys.Add(Rot);
   end;


procedure TBone.animteRotation(movetime:Single);
 VAR
     quad:Quaternion;
     currentIndex,nextIndex:Integer;
    factor, DeltaTime:Single;
   begin
	        currentIndex:= FindRotation(movetime);
          nextIndex:= (currentIndex + 1);

			if (nextIndex > RotKeys.Count-1) then Exit;

			  DeltaTime := (RotKeyFrame(RotKeys[nextIndex]).frame -RotKeyFrame(RotKeys[currentIndex]).frame);
        Factor    := (movetime - RotKeyFrame(RotKeys[currentIndex]).frame) / DeltaTime;


		 orientarion :=QuaternionSlerp(RotKeyFrame(RotKeys[currentIndex]).Rot, RotKeyFrame(RotKeys[nextIndex]).Rot, Factor);

      

	end;

	 procedure TBone.animtePosition(movetime:Single);
   var
     currentIndex,nextIndex:Integer;
    factor, DeltaTime:Single;
   begin
	        currentIndex:= FindPosition(movetime);
          nextIndex:= (currentIndex + 1);

			if (nextIndex > PosKeys.Count-1) then Exit;

			  DeltaTime := (PosKeyFrame(PosKeys[nextIndex]).frame -PosKeyFrame(PosKeys[currentIndex]).frame);
        Factor    := (movetime - PosKeyFrame(PosKeys[currentIndex]).frame) / DeltaTime;


	 	 position :=VectorInterpolate(PosKeyFrame(PosKeys[currentIndex]).Pos, PosKeyFrame(PosKeys[nextIndex]).Pos, Factor);




 
	end;



  procedure TBone.animte(movetime:Single);
   var
     currentIndex,nextIndex:Integer;
    newPosition:Vector3D;
    newOrientarion:Quaternion;
    factor, DeltaTime:Single;
   begin


   if (PosKeys.count<>RotKeys.count) then
   Assert(PosKeys.count<>RotKeys.count,Format('bone (%s) positions(%d) rotations(%d)',[name,PosKeys.count,RotKeys.count]));


	         animtePosition(movetime);
           animteRotation(movetime);
           UpdateAbsolutePosition();


  end;

 function TBone.FindPosition(time:Single):Integer  ;
 var
   i:Integer;
	begin
    for i:=0 to PosKeys.Count-2 do
    begin
      if(time<PosKeyFrame(PosKeys[i+1]).frame) then
      begin
        Result:=i;
        Exit;
      end;
    end;
    Result:=0;
  end;
 function TBone.FindRotation(time:Single):Integer  ;
 var
   i:Integer;
	begin
    for i:=0 to RotKeys.Count-2 do
    begin
      if(time<RotKeyFrame(RotKeys[i+1]).frame) then
      begin
        Result:=i;
        Exit;
      end;
    end;
    Result:=0;
  end;


constructor TEntity.Create;
begin
childs:=TList.Create;

position:=VectorCreate(0,0,0);
scale:=VectorCreate(1,1,1);
orientarion:= QuaternionRotation(VectorCreate(0,0,0));
AbsoluteTransformation := MatrixIdentity;
LocalWorld:= MatrixIdentity;
WorldMatrix:= MatrixIdentity;


parent:=nil;

end;
destructor TEntity.Destroy;
begin
childs.Destroy;
end;

procedure TEntity.addChild(child:TEntity);
begin
child.parent:=Self;
childs.Add(child);
end;
procedure TEntity.Update;
var
  i:Integer;
begin
  UpdateAbsolutePosition();

 for i:=0 to childs.Count-1 do
 begin
    TEntity(childs[i]).Update(timedelta);
 end;

end;
procedure TEntity.Render;
var
  i:Integer;
begin
 for i:=0 to childs.Count-1 do
 begin
    TEntity(childs[i]).Render;
 end;
 
end;

procedure TEntity.rotate(yaw:Single; pitch:Single; roll:Single);
begin
orientarion:=RotationYawPitchRollTo(yaw,pitch,roll);
end;

function TEntity.getRelativeTransformation():Matrix;
var
   m_rel:Matrix;
begin
LocalWorld:=makeTransform(position, scale, orientarion);

if Assigned(parent) then
begin
Result:=MatrixMultiply4x4(parent.AbsoluteTransformation,LocalWorld);
end else
begin
Result:=LocalWorld;
end;

end;
procedure TEntity.UpdateAbsolutePosition();
begin
  AbsoluteTransformation := getRelativeTransformation();
end;






//------------------------------------------------------------------------------
constructor TArrayVertex.Create;
begin
  FCount:=0;
end;

//------------------------------------------------------------------------------
destructor TArrayVertex.Destroy;
var
  i:Integer;
begin

//for i:=0 to count-1 do
//items[i].Destroy;


  SetLength(FItems, 0);
  inherited;
end;

//------------------------------------------------------------------------------
procedure TArrayVertex.Clear;
begin
  FCount:=0;
  SetLength(FItems, 0);
end;

//------------------------------------------------------------------------------
procedure TArrayVertex.Add(Value: T3DVertex);
begin
  Inc(FCount);
  SetLength(FItems, FCount);

  FItems[FCount-1]:=Value;
end;

//------------------------------------------------------------------------------
procedure TArrayVertex.SetCount(const Value: Integer);
begin
  FCount:=Value;
  SetLength(FItems, Value);
end;

//------------------------------------------------------------------------------
function TArrayVertex.GetItem(Index: Integer):T3DVertex ;
begin
  Result:=FItems[Index];
end;

//------------------------------------------------------------------------------
procedure TArrayVertex.SetItem(Index: Integer;const Value: T3DVertex);
begin
  FItems[Index]:=Value;
end;

//------------------------------------------------------------------------------
function TArrayVertex.getData: P3DVertex;
begin
   Result:=@FItems[0];
end;


end.
