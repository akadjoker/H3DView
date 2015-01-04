unit Model_2;

interface
uses Windows,SysUtils,classes,Controls,ComCtrls,ExtCtrls, Buttons, StdCtrls, Forms,morphio,dialogs,morphutils,textures,morphmath,dglopengl;



type

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
    bones:array[0..3]of Integer;
    weights:array[0..3]of single;
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

  offMatrix:Matrix;
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


TMesh=class

public
 tris:TIntegerList;
 vertex:TArrayVertex;
 no_tris:Integer;
 name:string;
 material:TMaterial;
 positions:array of Vector3D;
constructor Create;
destructor Destroy;

function addVertex(v:T3DVertex):Integer;
function addFace(a,b,c:Integer):Integer;

function getVertexPosition(index:Integer):Vector3d;

function  getVertexAt(index:Integer):T3DVertex;
procedure setVertexAt(index:Integer;v:T3DVertex);







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
   Frames  : integer;
   m_GlobalInverseTransform:Matrix;

  constructor Create;
  destructor Destroy;
  function createMesh():TMesh;
  procedure render(); overload;
procedure Update(TimeDelta: integer);overload;
  
  procedure loadh3d(f,path:string);
  procedure setTexture(filename:string);

  function getBoneAt(index:Integer):TBone;
   function getBoneByName(name:string):TBone;
   function findBoneByName(name:string):Integer;

   function getMeshAt(index:Integer):Tmesh;


end;


procedure RenderCube(x, y, z, Width, Height, Deapth: Single);


var
  mesh:TModel=nil;
  log:TMemo;

implementation



procedure RenderCube(x, y, z, Width, Height, Deapth: Single);
var w,h,d: glFloat;
begin
  // Calculate the half width, deapth and height
  w:=Width  / 2;
  h:=Height / 2;
  d:=Deapth / 2;

  glColor3f(1,0,0);

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
LoadTexture(filanem,tex0,False);
end;
procedure TMaterial.setTexture1(filanem:string);
begin
LoadTexture(filanem,tex1,False);
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


   if (material.tex0<>0) then
   begin
     glTex_Enable(material.tex0);
   end;


   glDrawElements(GL_TRIANGLES, no_tris * 3, GL_UNSIGNED_INT, tris.List);


end;

constructor TModel.Create;
begin
   inherited create;
     meshes:=TList.Create;
      materials:=TList.Create;
        bones:=TList.Create;
        AnimCur := -1;

Anim    := nil;
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

  LoadTexture(filename,tex,false);
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




      
for i:=0 to bones.Count-1 do
begin
   BOne:=TBone(bones[i]);
   BOne.animte(frame);
end;




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

        if (vertex.bones[0]<>-1) then
        begin

          Bone:= getBoneAt(vertex.bones[0]);
          Pos:=subMesh.getVertexPosition(v);


          tform_mat:=MatrixMultiply4x4(BOne.AbsoluteTransformation,BOne.offMatrix);
          w:=vertex.weights[0];
          ovx:=pos.x;
          ovy:=pos.y;
          ovz:=pos.z;
   		     x := ( tform_mat.v[0] * ovx + tform_mat.v[4] * ovy + tform_mat.v[8] * ovz  + tform_mat.v[12] ) * w;
			     y := ( tform_mat.v[1] * ovx + tform_mat.v[5] * ovy + tform_mat.v[9] * ovz  + tform_mat.v[13] ) * w;
		 	     z := ( tform_mat.v[2] * ovx + tform_mat.v[6] * ovy + tform_mat.v[10] * ovz + tform_mat.v[14] ) * w;
               {

        if (BoneWights.bones[1]<>-1) then
        begin
          Pos:=BoneWights.Pos;
          BOne:= getBoneAt(BoneWights.bones[1]);
          tform_mat:=MatrixMultiply4x4(BOne.AbsoluteTransformation,BOne.offMatrix);
          w:=BoneWights.weights[1];
          ovx:=pos.x;
          ovy:=pos.y;
          ovz:=pos.z;
   		     x :=x+ ( tform_mat.v[0] * ovx + tform_mat.v[4] * ovy + tform_mat.v[8] * ovz  + tform_mat.v[12] ) * w;
			     y :=y+ ( tform_mat.v[1] * ovx + tform_mat.v[5] * ovy + tform_mat.v[9] * ovz  + tform_mat.v[13] ) * w;
		 	     z :=z+ ( tform_mat.v[2] * ovx + tform_mat.v[6] * ovy + tform_mat.v[10] * ovz + tform_mat.v[14] ) * w;


           if (BoneWights.bones[2]<>-1) then
         begin
          Pos:=BoneWights.Pos;
          BOne:= getBoneAt(BoneWights.bones[2]);
          tform_mat:=MatrixMultiply4x4(BOne.AbsoluteTransformation,BOne.offMatrix);
          w:=BoneWights.weights[2];
          ovx:=pos.x;
          ovy:=pos.y;
          ovz:=pos.z;
   		     x :=x+ ( tform_mat.v[0] * ovx + tform_mat.v[4] * ovy + tform_mat.v[8] * ovz  + tform_mat.v[12] ) * w;
			     y :=y+ ( tform_mat.v[1] * ovx + tform_mat.v[5] * ovy + tform_mat.v[9] * ovz  + tform_mat.v[13] ) * w;
		 	     z :=z+ ( tform_mat.v[2] * ovx + tform_mat.v[6] * ovy + tform_mat.v[10] * ovz + tform_mat.v[14] ) * w;


         if (BoneWights.bones[3]<>-1) then
         begin
          Pos:=BoneWights.Pos;
          BOne:= getBoneAt(BoneWights.bones[3]);
          tform_mat:=MatrixMultiply4x4(BOne.AbsoluteTransformation,BOne.offMatrix);
          w:=BoneWights.weights[3];
          ovx:=pos.x;
          ovy:=pos.y;
          ovz:=pos.z;
   		     x :=x+ ( tform_mat.v[0] * ovx + tform_mat.v[4] * ovy + tform_mat.v[8] * ovz  + tform_mat.v[12] ) * w;
			     y :=y+ ( tform_mat.v[1] * ovx + tform_mat.v[5] * ovy + tform_mat.v[9] * ovz  + tform_mat.v[13] ) * w;
		 	     z :=z+ ( tform_mat.v[2] * ovx + tform_mat.v[6] * ovy + tform_mat.v[10] * ovz + tform_mat.v[14] ) * w;
         end;


         end;

         end;

            }

         end;

        vertex.Pos:=VectorCreate(x,y,z) ;
        subMesh.setVertexAt(v,vertex);

    end;



end;

    

end;

procedure TModel.render();
var
  i:Integer;
  subMesh:TMesh;
    BOne:TBone;
begin


  glColor4f(1,1,1,1);
	glEnableClientState(GL_VERTEX_ARRAY);
	glEnableClientState(GL_TEXTURE_COORD_ARRAY);
	glEnableClientState(GL_NORMAL_ARRAY);


  	glMatrixMode(GL_MODELVIEW);
  	glPushMatrix();
    glMultMatrixf(@AbsoluteTransformation.v[0]);
    for i:=0 to meshes.Count-1 do
    begin
    subMesh:=TMesh(meshes[i]);
    subMesh.render;
    end;
	glPopMatrix();
  glDisableClientState(GL_VERTEX_ARRAY);
	glDisableClientState(GL_NORMAL_ARRAY);
	glDisableClientState(GL_TEXTURE_COORD_ARRAY);
  glActiveTextureARB(GL_TEXTURE0_ARB);



for i:=0 to bones.Count-1 do
begin
    BOne:=TBone(bones[i]);
    BOne.render();
end;


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
  Result:=nil;
   for i:=0  to bones.Count-1 do
   begin
       bone:=TBone(bones[i]);
       Result:=bone;
       break;
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
     v.bones[i]:=-1;
     v.weights[i]:=1;
   end;

end;
procedure addBoneToVertex(var v:T3DVertex;id:Integer;w:Single);
var
  i:Integer;
begin

for i:=0 to 3 do
begin
    if (v.bones[i]=-1) then
    begin
       v.weights[i]:=w;
       v.bones[i]:=id;
       Inc(v.numBones);
       Break;
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

    log.Lines.Clear;


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

     log.Lines.Add(Format('Main Texture (%s)',[material.texture]));


     if (FileExists(path+ material.texture)) then
     begin
       LoadTexture(path+ material.texture,material.tex0,False);
     end;

     log.Lines.Add(Format('Material Index(%d)',[i]));

     uvs:=data.readint();
     if(uvs=2) then
     begin
       material.ContainsLightmap:=True;
       material.textureDetail:=data.ReadChar;

            log.Lines.Add(Format('Detail Texture (%s)',[material.texture]));

     if (FileExists(path+ material.textureDetail)) then
     begin
       LoadTexture(path+ material.textureDetail,material.tex1,False);
     end;
     end ;

       materials.Add(material);
   end;


   CounTMeshs:=data.readint();

   log.Lines.Add(Format('Sub Meshes (%d)',[CounTMeshs]));

  for   i:=0 to CounTMeshs-1 do
  begin

      name:=data.ReadChar;
      flags:=data.readint;
      matid:=data.readint;
      CountVertices:=data.readint;
      CountTriangles:=data.readint();
      uvs:=data.readint;

         log.Lines.Add(Format('Mesh (%s) Vertices(%d) Triangles(%d) TexCoord(%d)',[name,CountVertices,CountTriangles,uvs]));


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

       log.Lines.Add(Format('Mesh  Frames(%n) Duration(%n)',[framesPerSecond,duration]));



    for i:=0 to numBones-1 do
    begin
          Bone:=TBone.Create;

          bone.Name:=data.ReadChar();
          bone.parentName:=data.ReadChar();
          BOne.numPosKeys:=data.readint;
          BOne.numRotKeys:=data.readint;

          BOne.position.x:=data.readfloat;
          BOne.position.y:=data.readfloat;
          BOne.position.z:=data.readfloat;

          BOne.orientarion.X:=data.readfloat;
          BOne.orientarion.y:=data.readfloat;
          BOne.orientarion.Z:=data.readfloat;
          BOne.orientarion.w:=data.readfloat;



    log.Lines.Add(Format('Bone  Name(%s) Parent (%s) Num PosKeys(%d) Num RotKeys(%d)',[Bone.Name,Bone.parentName,Bone.numPosKeys,bone.numRotKeys]));


      for j:=0 to  Bone.numPosKeys-1 do
        begin
           Poskey:=PosKeyFrame.Create;
            Poskey.frame:=data.readfloat;
            Poskey.Pos.X:= data.readfloat;
            Poskey.Pos.Y:= data.readfloat;
            Poskey.Pos.Z:= data.readfloat;
            Bone.addPoskey(poskey);
        end;
          for j:=0 to  Bone.numRotKeys-1 do
        begin
            RotKey:=RotKeyFrame.Create;
            RotKey.frame:=data.readfloat;
            RotKey.Rot.X:=data.readfloat;
            RotKey.Rot.y:=data.readfloat;
            RotKey.Rot.Z:=data.readfloat;
            RotKey.Rot.w:=data.readfloat;
            Bone.addRotkey(rotkey);
        end;

               {
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
        }
 
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

     // log.Lines.Add(Format('Bone (%s) id(%d) NumWeights(%d)',[BOneName,boneid,NumWeights])) ;


        for v:=0 to NumWeights-1 do
        begin
              VertexId:=data.readint;
              Weight:=data.readfloat;
              vertex:=mesh.getVertexAt(VertexId);
              addBoneToVertex(vertex,boneid,Weight);
              mesh.setVertexAt(VertexId,vertex);

        end;

    end;


  end;

 end;


 data.Destroy;

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
      glMultMatrixf(@ AbsoluteTransformation.v[0]);
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

	// render bone-ends as fat points
	glPointSize(4.0);
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

end;
destructor TBone.Destroy;
begin
 inherited Destroy();
          PosKeys.Clear;
          RotKeys.Clear;
   PosKeys.Destroy;
   RotKeys.Destroy;

   PosKeys:=nil;
   RotKeys:=nil;
end;
procedure  Tbone.InitPose();
begin
LocalWorld:=makeTransform(position, scale, orientarion);
if Assigned(parent) then
begin
LocalWorld.postMultiply(parent.AbsoluteTransformation);
end;
offMatrix:=MatrixInverse(LocalWorld);

UpdateAbsolutePosition();

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
