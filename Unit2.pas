unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls;

type
  TForm2 = class(TForm)
    RichEdit: TRichEdit;
    Button: TButton;
    procedure ButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation
uses Model;

{$R *.dfm}

procedure TForm2.ButtonClick(Sender: TObject);
begin
close;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  RichEdit.Lines.Clear;
end;

procedure TForm2.FormShow(Sender: TObject);
var
b, j, i:Integer;
  subMesh:TMesh;
  vertex:T3DVertex;

begin
    RichEdit.Lines.Clear;
if Assigned(mesh) then
begin
   for i:=0 to mesh.meshes.Count-1 do
   begin
     subMesh:= mesh.getMeshAt(i);
     RichEdit.Lines.Add('------------------------');
     RichEdit.Lines.Add('Name:'+subMesh.name);
     RichEdit.Lines.Add(Format('Num Vertex(%d) Num Faces (%d)', [subMesh.vertex.Count,subMesh.tris.Count]));

           RichEdit.Lines.Add('                  ');
     for j:=0 to subMesh.vertex.Count-1 do
     begin
       vertex:=subMesh.getVertexAt(j);



       RichEdit.Lines.Add(Format('Vertex Index(%d)  Num Bones (%d)', [j,vertex.numBones]));
       {RichEdit.Lines.Add(Format('Position x(%n) y(%n) z(%n)', [vertex.Pos.x,vertex.Pos.y,vertex.Pos.z]));
       RichEdit.Lines.Add(Format('Normal x(%n) y(%n) z(%n)', [vertex.nor.x,vertex.Nor.y,vertex.Nor.z]));
       RichEdit.Lines.Add(Format('TexCoord x(%n) y(%n) ', [vertex.coord0.x,vertex.coord0.y]));
        }


       for b:=0 to vertex.numBones-1 do
       begin
          RichEdit.Lines.Add(Format('Bone(%d) Index(%d) Wight(%n)', [b,vertex.bones[b],vertex.weights[b]]))
       end;

       RichEdit.Lines.Add('                  ');

     end;
         RichEdit.Lines.Add('------------------------');

   end;
end;

end;

end.
