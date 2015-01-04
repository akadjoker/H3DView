Unit morphmath;


Interface
Uses Windows;

Type


  

  PVector3D = ^Vector3D;
  Vector3D=Packed Object
    X:Single;
    Y:Single;
    Z:Single;


    Function Equals(Const B:Vector3D):Boolean;
    Function Dot(Const B:Vector3D):Single;

    Procedure Add(Const B:Vector3D);
    Procedure Subtract(Const B:Vector3D);

    Procedure Scale(Const S:Single);
    Procedure Multiply(Const B:Vector3D);

    Function Get(Index:Integer):Single;
    Procedure SetValue(Index:Integer; Value:Single);

    // Normalizes the vector
    Procedure Normalize;
    Function Length:Single;
    Function LengthSquared:Single;
    Function Distance(Const B:Vector3D):Single;
    Function Distance2D(Const B:Vector3D):Single;



    Procedure Rotate(Const Axis:Vector3D; Const Angle:Single);
  End;


Const
// Vector constants
  VectorZero: Vector3D = (X:0.0; Y:0.0; Z:0.0);
  VectorOne:  Vector3D = (X:1.0; Y:1.0; Z:1.0);
  VectorUp:   Vector3D = (X:0.0; Y:1.0; Z:0.0);

  
Type
  PVector2D=^Vector2D;
  Vector2D=Packed Object
    X,Y:Single;

    Function Equals(Const B:Vector2D):Boolean;

    Procedure Rotate(Const Angle:Single); Overload;
    Procedure Rotate(Const Angle:Single; Const Center:Vector2D); Overload;

    Procedure Add(Const B:Vector2D);
    Procedure Subtract(Const B:Vector2D);

    Procedure Scale(Const S:Single);Overload;
    Procedure Scale(Const B:Vector2D);Overload;

    Procedure Project(Const V:Vector2D);

    Procedure Normalize;

    Function Length:Single;
    Function Distance(Const B:Vector2D):Single;

    Function Dot(B:Vector2D):Single;


  End;

type

  PMatrix = ^Matrix;
  Matrix=Packed Object
    V:Array [0..15] Of Single;

    //Function GetAngles:Vector3D;

    procedure postMultiply(mat:Matrix);

    procedure Copy(mat:Matrix);

    Function Transform(P:Vector3D):Vector3D;

    Function inverseTranslateVect(var P:Vector3D):Vector3D;
    Function inverseRotateVect(var P:Vector3D):Vector3D;
    Function TranslateVect(var P:Vector3D):Vector3D;
    Function RotateVect(var P:Vector3D):Vector3D;

    Function TransformNormal(P:Vector3D):Vector3D;

        procedure setTranslation(x,y,z:single);

    Procedure Orthonormalize;

    Function Get(I,J:Integer):Single;
    procedure SetData(I, J: Integer;d:single);
    Function GetTranslation:Vector3D;


  End;

  Matrix3x3=Packed Object
    m:Array [0..8] Of Single;

    function MulVec(v:Vector3D):Vector3D;
  End;

  PMatrixArray=^MatrixArray;
  MatrixArray=Array[0..255] Of Matrix;

Const
 MatrixIdentity:Matrix= (V:(1.0, 0.0, 0.0, 0.0,
                            0.0, 1.0, 0.0, 0.0,
                            0.0, 0.0, 1.0, 0.0,
                            0.0, 0.0, 0.0, 1.0));
 Matrix3xIdentity:Matrix3x3= (m:
                            (1.0,
0.0,
0.0,
0.0,
1.0,
0.0,
0.0,
0.0,
1.0));
  
Type
  PQuaternion = ^Quaternion;
  Quaternion=Packed Object
    X:Single;
    Y:Single;
    Z:Single;
    W:Single;

    Function Equals(Const B:Quaternion):Boolean;

    Procedure Transform(Const M:Matrix);
    // Returns a normalized quaternion
    Procedure Normalize;

     function Inverse():quaternion;

    Procedure Add(Const B:Quaternion);
    Procedure Subtract(Const B:Quaternion);

    Function Length:Single;
  End;

  Vector4D = Quaternion;
  Color4F = Vector4D;




Const
  QuaternionOne:Vector4D=(X:1.0; Y:1.0; Z:1.0; W:1.0);
  QuaternionZero:Vector4D=(X:0.0; Y:0.0; Z:0.0; W:1.0);

// Returns a rotation matrix
Function MatrixRotation(Const Rotation:Vector3D):Matrix; Overload; 
Function MatrixRotation(Const X,Y,Z:Single):Matrix; Overload; 

Function MatrixRotation(Const Axis:Vector3D; Const Angle:Single):Matrix; Overload;

// Returns a translation matrix
Function MatrixTranslation(Const Translation:Vector3D):Matrix;Overload; 
Function MatrixTranslation(Const X,Y,Z:Single):Matrix;Overload; 

Function MatrixTransform(Const Position,Rotation,Scale:Vector3D):Matrix;

Function MatrixOrientation(Const Position,Direction,Up,Scale:Vector3D):Matrix;

Function MatrixLerp(Const A,B:Matrix; Const S:Single):Matrix;

// Inverts a matrix
Function MatrixInverse(A:Matrix):Matrix;

Function MatrixScale(Const Scale:Vector3D):Matrix;Overload; 
Function MatrixScale(Const X,Y,Z:Single):Matrix;Overload;   

// Returns a reflection matrix
Function MatrixMirror(Const Source,Normal:Vector3D):Matrix;

Function MatrixTranspose(Const Source:Matrix):Matrix;

// Multiplys two matrices
Function MatrixMultiply4x3(Const A,B:Matrix):Matrix;
Function MatrixMultiply4x4(Const A,B:Matrix):Matrix;




Function MatrixPerspective(FOV, AspectRatio, zNear, zFar:Single):Matrix;
Function MatrixLookAt(Eye, LookAt, Roll:Vector3D):Matrix;
Function MatrixOrtho(left, right,  bottom,  top,  nearVal,  farVal:Single):Matrix;

Function MatrixIsometric(X,Y, Height:Single):Matrix;


// Vector functions
Function VectorCreate(Const X,Y,Z:Single):Vector3D;

Function VectorUniform(Const N:Single):Vector3D;

Function VectorMax(Const A,B:Vector3D):Vector3D;
Function VectorMin(Const A,B:Vector3D):Vector3D;

Function VectorAdd(Const A,B:Vector3D):Vector3D;
Function VectorSubtract(Const A,B:Vector3D):Vector3D;
Function VectorMultiply(Const A,B:Vector3D):Vector3D;
Function VectorCross(Const A,B:Vector3D):Vector3D;

// Returns the dot product between two vectors
Function VectorDot(Const A,B:Vector3D):Single;

// Scales a vector by S
Function VectorScale(Const A:Vector3D; S:Single):Vector3D;

// Reflect two vectors
Function VectorReflect(Const Source,Normal:Vector3D):Vector3D;

Function VectorInterpolate(Const A,B:Vector3D; Const S:Single):Vector3D;

// Halve arc between unit vectors v0 and v1.
Function VectorBisect(Const A,B:Vector3D):Vector3D;

// Returns a triangle normal
Function TriangleNormal(Const V0,V1,V2:Vector3D):Vector3D;

//  Quad functions
Function GetTriangleHeight(H0,H1,H2:Single; X,Y:Single; Normal:PVector3D=Nil):Single;




Function QuaternionCreate(Const X,Y,Z,W:Single):Quaternion;Overload;
Function QuaternionCreate(Const V:Vector3D):Quaternion;Overload;
Function QuaternionCreate(Const pMatrix:Matrix):Quaternion;Overload;

// Creates a quaterion with specified rotation
Function QuaternionRotation(Const Rotation:Vector3D):Quaternion;

// Returns a matrix representing the quaternion
Function QuaternionMatrix(Const q:Quaternion):Matrix;
Function QuaternionToMatrix(Pos:Vector3D;q:Quaternion):Matrix;

Function QuaternionRotationMatrix(Const q:Quaternion):Matrix3x3;

// Slerps two quaternions
Function QuaternionSlerp(A,B:Quaternion; Const T:Single):Quaternion;



  function RotationYawPitchRollTo(yaw:Single; pitch:Single; roll:Single):Quaternion;

// Returns the conjugate of a quaternion
Function QuaternionConjugate(Const Q:Quaternion):Quaternion;

// Multiplies two quaternions
Function QuaternionMultiply( Ql,Qr:Quaternion):Quaternion;

Function QuaternionAdd(Const A,B:Quaternion):Quaternion;

Function QuaternionScale(Const Q:Quaternion; Const Scale:Single):Quaternion;

Function QuaternionFromBallPoints(Const arcFrom,arcTo:Vector3D):Quaternion;
Procedure QuaternionToBallPoints(Var Q:Quaternion; arcFrom,arcTo:Vector3D);

Function QuaternionFromAxisAngle(Const Axis:Vector3D; Const Angle:Single):Quaternion;

Function QuaternionToEuler(Const Q:Quaternion):Vector3D;

function makeTransform( position:Vector3d; scale:Vector3d; orientation:Quaternion):Matrix;




Function VectorCreate2D(Const X,Y:Single):Vector2D;
Function VectorCross2D(Const A,B:Vector2D):Single;

Function VectorAdd2D(Const A,B:Vector2D):Vector2D;
Function VectorSubtract2D(Const A,B:Vector2D):Vector2D;

Const
  Epsilon = 0.00001;

  Rad   = 0.017453292519;   // Pi/180
  Deg   = 57.29577951308;   // 180/Pi

  RAND_MAX = Pred(MAXINT);
  INV_RAND_MAX = 1.0 / (RAND_MAX + 1);

Function FloatMax(Const A,B:Single):Single; {$IFDEF FPC} Inline;{$ENDIF}
Function FloatMin(Const A,B:Single):Single; {$IFDEF FPC} Inline;{$ENDIF}

Function RandomFloat:Single; Overload;
Function RandomFloat(Const min,max:Single):Single; Overload;

Function RealMod(Const n,d: Single): Single;



Function SmoothStep(A,B,X:Single):Single;

Function NearestPowerOfTwo(P:Cardinal):Cardinal;

Function LinearInterpolate(a,b, mu:Single):Single; {$IFDEF FPC} Inline;{$ENDIF}
Function CubicInterpolate(y0, y1, y2, y3, mu:Single):Single; {$IFDEF FPC} Inline;{$ENDIF}
Function CatmullRomInterpolate(y0, y1, y2, y3, mu:Single):Single; {$IFDEF FPC} Inline;{$ENDIF}

Function QuadraticBezierCurve(y0, y1, y2, mu:Single):Single; {$IFDEF FPC} Inline;{$ENDIF}
Function CubicBezierCurve(y0, y1, y2, y3, mu:Single):Single; {$IFDEF FPC} Inline;{$ENDIF}

Function InvSqrt(X:Single):Single; {$IFDEF FPC} Inline;{$ENDIF}


Function Pow(X, Y:Single):Single;

Implementation
Uses Math;




Function RealMod(Const n,d: Single): Single;
Var
  i: integer;
Begin
  i := trunc(n / d);
  result := n - d * i;
End;

Function IntPower(X:Single; I: Integer): Single;
var
  Y: Integer;
begin
  Y := Abs(I);
  Result := 1.0;
  While Y > 0 do
  Begin
    While Not Odd(Y) do
    Begin
      Y := Y Shr 1;
      X := X * X;
    End;
    Dec(Y);
    Result := Result * X;
  End;
  if I < 0.0 Then
    Result := 1.0 / Result;
End;

Function Pow(X, Y:Single):Single;
Begin
  If Y = 0.0 Then
    Result := 1.0
  Else if (X = 0.0) and (Y > 0.0) Then
    Result := 0.0
  Else if (Frac(Y) = 0.0) and (Abs(Y) <= MaxInt) then
    Result := IntPower(X, Integer(Trunc(Y)))
  Else
    Result := Exp(Y * Ln(X))
End;

Function NearestPowerOfTwo(P:Cardinal):Cardinal;
Var
  I,N:Cardinal;
Begin
  Result := 0;
  For I:=14 DownTo 2 Do
  Begin
    N:=(1 Shl I);
    If N<P Then
     Break
    Else
      Result:=N;
  End;
End;

Function LinearInterpolate(a,b, mu:Single):Single; {$IFDEF FPC} Inline;{$ENDIF}
Begin
  Result := (B * Mu) + A * (1.0 - Mu);
End;

Function CubicInterpolate(y0, y1, y2, y3, mu:Single):Single; {$IFDEF FPC} Inline;{$ENDIF}
Var
   a0,a1,a2,a3,mu2:Single;
Begin
   mu2 := Sqr(mu);
   a0 := y3 - y2 - y0 + y1;
   a1 := y0 - y1 - a0;
   a2 := y2 - y0;
   a3 := y1;
   Result := (a0 * mu * mu2) + (a1 * mu2) + (a2 * mu) + a3;
End;

Function CatmullRomInterpolate(y0, y1, y2, y3, mu:Single):Single; {$IFDEF FPC} Inline;{$ENDIF}
Var
   a0,a1,a2,a3,mu2:Single;
Begin
   mu2 := Sqr(mu);
   a0 := (-0.5 * y0) + (1.5 * y1) - (1.5 * y2) + (0.5 * y3);
   a1 := y0 - (2.5 * y1) + (2.0 * y2) - (0.5 * y3);
   a2 := (-0.5 * y0) + (0.5 * y2);
   a3 := y1;
   Result := (a0 * mu * mu2) + (a1 * mu2) + (a2 * mu) + a3;
End;

Function QuadraticBezierCurve(y0, y1, y2,  mu:Single):Single; {$IFDEF FPC} Inline;{$ENDIF}
Begin
  Result := Sqr(1-mu) * y0 + 2 * (1-mu) * y1  + Sqr(mu) * y2;
End;

Function CubicBezierCurve(y0, y1, y2, y3, mu:Single):Single; {$IFDEF FPC} Inline;{$ENDIF}
Begin
  Result := (1-mu) * Sqr(1-mu) * y0 + 3 * Sqr(1-mu) * y1  + 3 * (1-mu) * Sqr(mu) * y2 + Sqr(mu) * mu * y3;
End;

Function FloatMax(Const A,B:Single):Single; {$IFDEF FPC} Inline;{$ENDIF}
Begin
  If A>B Then Result:=A Else Result:=B;
End;

Function FloatMin(Const A,B:Single):Single; {$IFDEF FPC} Inline;{$ENDIF}
Begin
  If A<B Then Result:=A Else Result:=B;
End;

Function RandomFloat:Single; Overload;
Begin
  Result := System.Random(RAND_MAX) * INV_RAND_MAX;
End;

Function RandomFloat(Const min,max:Single):Single; Overload;
Begin
	Result := Min + ((max - min) * (System.Random(RAND_MAX) * INV_RAND_MAX));
End;

Function SmoothStep(A, B, X:Single):Single;
Begin
  If (x < a) Then
    Result := 0.0
  Else
  If (x >= b) Then
    Result := 1.0
  Else
  Begin
    x := (x-a) / (b-a);
    Result := (x*x) * (3-2*x);
  End;
End;


{$OVERFLOWCHECKS OFF}
Function InvSqrt(X:Single):Single; {$IFDEF FPC} Inline;{$ENDIF}
Var
  I:Cardinal;
  xhalf:Single;
Begin
  xhalf := 0.5*x;
  i := PCardinal(@x)^;         // get bits for floating value
  i := $5f3759df - (i Shr 1);   // give initial guess y0
  x := PSingle(@i)^;           // convert bits back to float
  x := X * (1.5 - xhalf*x*x);     // newton step, repeating this step
  x := X * (1.5 - xhalf*x*x); // increases accuracy
  Result := X;
End;


Function MatrixIsometric(X,Y, Height:Single):Matrix;
Begin
//http://toxicdump.org/blog/view/7/Haunted-Mansion-DevBlog-_02---Oblique-vs-Isometric-perspective-and-perfect-depth-sorting
//http://www.xnaresources.com/default.asp?page=Tutorial:TileEngineSeries:4
//http://www.gamedev.net/topic/226797-isometric-camera-problems/
//glRotatef(30.0, 1,0,0);
//glRotatef(-45.0, 0,1,0);
//glTranslatef(-x, -m_cameraheight, -z);
End;

Function MatrixTranspose(Const Source:Matrix):Matrix;
Begin
  Result.V[0] := Source.V[0];
  Result.V[1] := Source.V[4];
  Result.V[2] := Source.V[8];
  Result.V[3] := Source.V[12];

  Result.V[4] := Source.V[1];
  Result.V[5] := Source.V[5];
  Result.V[6] := Source.V[9];
  Result.V[7] := Source.V[13];

  Result.V[8] := Source.V[2];
  Result.V[9] := Source.V[6];
  Result.V[10] := Source.V[10];
  Result.V[11] := Source.V[14];

  Result.V[12] := Source.V[3];
  Result.V[13] := Source.V[7];
  Result.V[14] := Source.V[11];
  Result.V[15] := Source.V[15];
End;

Function MatrixOrtho(left, right,  bottom,  top,  nearVal,  farVal:Single):Matrix;
Var
  Tx, Ty, Tz:Single;
Begin
  TX := -(Right + Left)/(Right - Left);
  TY := -(Top + Bottom)/(Top - Bottom);
  TZ := -(farVal + nearVal)/(farVal - nearVal);

  Result.V[0] := 2 / (Right - Left);
  Result.V[1] := 0;
  Result.V[2] := 0;
  Result.V[3] := 0;

  Result.V[4] := 0;
  Result.V[5] := 2 / (Top - Bottom);
  Result.V[6] := 0;
  Result.V[7] := 0;

  Result.V[8] := 0;
  Result.V[9] := 0;
  Result.V[10] := -2 / (farVal - nearVal);
  Result.V[11] := 0;

  Result.V[12] := tx;
  Result.V[13] := ty;
  Result.V[14] := tz;
  Result.V[15] := 1.0;
  // + 0.375
End;

//TESTME
Function Matrix.Get(I, J: Integer): Single;
Begin
  Result := V[J*4+I];
End;
procedure Matrix.SetData(I, J: Integer;d:single);
Begin
 V[J*4+I]:=d;
End;
procedure Matrix.setTranslation(x,y,z:single);
begin
  V[12] := X;
  V[13] := Y;
  V[14] := Z;
end;

procedure Matrix.Copy(mat:Matrix);
var i : integer;
begin
  for i := 0 to 15 do
   v[i] := mat.v[i];

end;



Function Matrix.GetTranslation:Vector3D;
Begin
  Result.X := V[12];
  Result.Y := V[13];
  Result.Z := V[14];
End;

Procedure Matrix.Orthonormalize;
Var
  x,y,z:Vector3D;
Begin
  // It probably should be going 11, 12, 13 for x
  x := VectorCreate(V[0], V[1], V[2]);
  // And 21, 22, 23 for y.
  x := VectorCreate(V[4], V[5], V[6]);

  x.Normalize();
  z := VectorCross(x, y);
  z.Normalize;
  y := VectorCross(z, x);
  y.Normalize;
  V[0] := x.X;
  V[1] := y.X;
  V[2] := z.X;
  V[4] := x.Y;
  V[5] := y.Y;
  V[6] := z.Y;
  V[8] := x.Z;
  V[9] := y.Z;
  V[10] := z.Z;
End;


Function Matrix.inverseTranslateVect(var P:Vector3D):Vector3D;
begin
  p.X := P.X- V[12];
  p.Y := P.Y- V[13];
  p.Z := P.Z- V[14];
  result:=p;
end;
Function Matrix.inverseRotateVect(var P:Vector3D):Vector3D;

begin

p.x := p.x*v[0]+p.y*v[1]+p.z*v[2];
p.y := p.x*v[4]+p.y*v[5]+p.z*v[6];
p.z := p.x*v[8]+p.y*v[9]+p.z*v[10];
Result:=p;

end;
Function Matrix.TranslateVect(var P:Vector3D):Vector3D;
begin
  p.X := P.X+ V[12];
  p.Y := P.Y+ V[13];
  p.Z := P.Z+ V[14];
  result:=p;
end;
Function Matrix.RotateVect(var P:Vector3D):Vector3D;

begin

p.x := p.x*v[0]+p.y*v[4]+p.z*v[8];
p.y := p.x*v[1]+p.y*v[5]+p.z*v[9];
p.z := p.x*v[2]+p.y*v[6]+p.z*v[10];
Result:=p;

end;

function Matrix3x3.MulVec(v:Vector3D):Vector3D;
begin
 	Result.x:=m[0] * v.x + m[3] * v.y + m[6] * v.z;
  Result.y:=m[1] * v.x + m[4] * v.y + m[7] * v.z;
  Result.z:=m[2] * v.x + m[5] * v.y + m[8] * v.z;

end;


Function Matrix.Transform(P:Vector3D):Vector3D; 
Begin
  Result.X := P.X*V[0] + P.Y*V[4] + P.Z*V[8]  + V[12];
  Result.Y := P.X*V[1] + P.Y*V[5] + P.Z*V[9]  + V[13];
  Result.Z := P.X*V[2] + P.Y*V[6] + P.Z*V[10] + V[14];


End;
procedure Matrix.postMultiply(mat:Matrix);
var
newMat :Matrix;
begin
		newmat.v[0] := v[0]*mat.v[0] + v[4]*mat.v[1] + v[8]*mat.v[2];
	newmat.v[1] := v[1]*mat.v[0] + v[5]*mat.v[1] + v[9]*mat.v[2];
	newmat.v[2] := v[2]*mat.v[0] + v[6]*mat.v[1] + v[10]*mat.v[2];
	newmat.v[3] := 0;

	newmat.v[4] := v[0]*mat.v[4] + v[4]*mat.v[5] + v[8]*mat.v[6];
	newmat.v[5] := v[1]*mat.v[4] + v[5]*mat.v[5] + v[9]*mat.v[6];
	newmat.v[6] := v[2]*mat.v[4] + v[6]*mat.v[5] + v[10]*mat.v[6];
	newmat.v[7] := 0;

	newmat.v[8] := v[0]*mat.v[8] + v[4]*mat.v[9] + v[8]*mat.v[10];
	newmat.v[9] := v[1]*mat.v[8] + v[5]*mat.v[9] + v[9]*mat.v[10];
	newmat.v[10] := v[2]*mat.v[8] + v[6]*mat.v[9] + v[10]*mat.v[10];
	newmat.v[11] := 0;

	newmat.v[12] := v[0]*mat.v[12] + v[4]*mat.v[13] + v[8]*mat.v[14] + v[12];
	newmat.v[13] := v[1]*mat.v[12] + v[5]*mat.v[13] + v[9]*mat.v[14] + v[13];
	newmat.v[14] := v[2]*mat.v[12] + v[6]*mat.v[13] + v[10]*mat.v[14] + v[14];
	newmat.v[15] := 1;
 Copy(newMat);
end;


Function Matrix.TransformNormal(P:Vector3D):Vector3D; 
Begin
  Result.X := P.X*V[0] + P.Y*V[4] + P.Z*V[8];
  Result.Y := P.X*V[1] + P.Y*V[5] + P.Z*V[9];
  Result.Z := P.X*V[2] + P.Y*V[6] + P.Z*V[10];
End;



Function MatrixTransform(Const Position,Rotation,Scale:Vector3D):Matrix;
Var
  CosRx,CosRy,CosRz:Single;
  SinRx,SinRy,SinRz:Single;
Begin
  CosRx := Cos(Rotation.x); //Used 6x
  CosRy := Cos(Rotation.y); //Used 4x
  CosRz := Cos(Rotation.z); //Used 4x
  SinRx := Sin(Rotation.x); //Used 5x
  SinRy := Sin(Rotation.y); //Used 5x
  SinRz := Sin(Rotation.z); //Used 5x

  Result.V[0] := CosRy*CosRz*Scale.x;
  Result.V[1] := CosRy*SinRz*Scale.x;
  Result.V[2] := -SinRy*Scale.x;
  Result.V[3] := 0.0;

  Result.V[4] := (CosRz*SinRx*SinRy*Scale.y) - (CosRx*SinRz*Scale.y);
  Result.V[5] := (CosRx*CosRz*Scale.y) + (SinRx*SinRy*SinRz*Scale.y);
  Result.V[6] := CosRy*SinRx*Scale.y;
  Result.V[7] := 0.0;

  Result.V[8] := (CosRx*CosRz*SinRy*Scale.z) + (SinRx*SinRz*Scale.z);
  Result.V[9] := (-CosRz*SinRx*Scale.z) + (CosRx*SinRy*SinRz*Scale.z);
  Result.V[10] := CosRx*CosRy*Scale.z;
  Result.V[11] := 0.0;

  Result.V[12] := Position.x;
  Result.V[13] := Position.y;
  Result.V[14] := Position.z;
  Result.V[15] := 1.0;
End;

Function MatrixOrientation(Const Position,Direction,Up,Scale:Vector3D):Matrix;
Var
  TX,TZ:Vector3D;
Begin
  TZ := VectorCross(Direction, Up);
  TZ.Normalize;
  TX := VectorCross(Up, TZ);
  TX.Normalize;

  Result.V[0] := TX.X * Scale.X;
  Result.V[1] := TX.Y * Scale.X;
  Result.V[2] := TX.Z * Scale.X;
  Result.V[3] := 0.0;

  Result.V[4] := Up.X * Scale.y;
  Result.V[5] := Up.Y * Scale.y;
  Result.V[6] := Up.Z * Scale.Y;
  Result.V[7] := 0.0;

  Result.V[8] := TZ.X * Scale.Z;
  Result.V[9] := TZ.Y * Scale.Z;
  Result.V[10] := TZ.Z * Scale.Z;
  Result.V[11] := 0.0;

  Result.V[12] := Position.x;
  Result.V[13] := Position.y;
  Result.V[14] := Position.z;
  Result.V[15] := 1.0;
End;

Function MatrixRotation(Const Axis:Vector3D; Const Angle:Single):Matrix;
Var
  C,S,T:Single;
  X,Y,Z:Single;
Begin
  C := Cos(Angle);
  S := Sin(Angle);
  T := 1-C;

  X := Axis.X;
  Y := Axis.Y;
  Z := Axis.Z;

	Result.V[0] := T * Sqr(X) + C;
	Result.V[1] := (T * X * Y) - (s *Z);
	Result.V[2] := (T * X * Z) + (s * Y);
  Result.V[3] := 0.0;

	Result.V[4] := (t * X * Y) + (s * Z);
	Result.V[5] := (t * Y * Y)+ C;
	Result.V[6] := (T * Y * Z) - (S * X);
  Result.V[7] := 0.0;

	Result.V[8] := (T * X * Z) - (S * Y);
	Result.V[9] := (T * Y * Z) + (S * X);
	Result.V[10] := (T * Z * Z) +  C;
  Result.V[11] := 0.0;
  Result.V[12] := 0.0;
  Result.V[13] := 0.0;
  Result.V[14] := 0.0;
  Result.V[15] := 1.0;
End;

Function MatrixRotation(Const Rotation:Vector3D):Matrix;  
Begin
  Result := MatrixRotation(Rotation.X, Rotation.Y, Rotation.Z);
End;

Function MatrixRotation(Const X,Y,Z:Single):Matrix;  
Var
  Cr,Sr,Cp,Sp,Cy,Sy,Srsp,Crsp:Single;
Begin
  cr := Cos(X);
	sr := Sin(X);
	cp := Cos(Y);
	sp := Sin(Y);
	cy := Cos(Z);
	sy := Sin(Z);

	Result.V[0] := cp * cy;
	Result.V[1] := cp * sy;
	Result.V[2] := -sp;

  If Result.V[2] = -0 Then
    Result.V[2] := 0;
  Result.V[3] := 0.0;

	srsp := sr * sp;
	crsp := cr * sp;

	Result.V[4] := (srsp * cy) - (cr * sy);
	Result.V[5] := (srsp * sy) + (cr * cy);
	Result.V[6] := (sr * cp);
  Result.V[7] := 0.0;

	Result.V[8] := (crsp * cy) + (sr * sy);
	Result.V[9] := (crsp * sy) - (sr * cy);
	Result.V[10] := (cr * cp);
  Result.V[11] := 0.0;
  Result.V[12] := 0.0;
  Result.V[13] := 0.0;
  Result.V[14] := 0.0;
  Result.V[15] := 1.0;
End;

Function MatrixTranslation(Const Translation:Vector3D):Matrix;  
Begin
  Result := MatrixTranslation(Translation.X,Translation.Y,Translation.Z);
End;

Function MatrixTranslation(Const X,Y,Z:Single):Matrix;  
Begin
  Result.V[0] := 1.0;
  Result.V[1] := 0.0;
  Result.V[2] := 0.0;
  Result.V[3] := 0.0;
  Result.V[4] := 0.0;
  Result.V[5] := 1.0;
  Result.V[6] := 0.0;
  Result.V[7] := 0.0;
  Result.V[8] := 0.0;
  Result.V[9] := 0.0;
  Result.V[10] := 1.0;
  Result.V[11] := 0.0;
  Result.V[12] := X;
  Result.V[13] := Y;
  Result.V[14] := Z;
  Result.V[15] := 1.0;
End;


Function MatrixScale(Const Scale:Vector3D):Matrix;  
Begin
  Result := MatrixScale(Scale.X,Scale.Y,Scale.Z);
End;

Function MatrixScale(Const X,Y,Z:Single):Matrix;  
Begin
  Result.V[0] := X;
  Result.V[1] := 0.0;
  Result.V[2] := 0.0;
  Result.V[3] := 0.0;
  Result.V[4] := 0.0;
  Result.V[5] := Y;
  Result.V[6] := 0.0;
  Result.V[7] := 0.0;
  Result.V[8] := 0.0;
  Result.V[9] := 0.0;
  Result.V[10] := Z;
  Result.V[11] := 0.0;
  Result.V[12] := 0.0;
  Result.V[13] := 0.0;
  Result.V[14] := 0.0;
  Result.V[15] := 1.0;
End;

Function MatrixPerspective(FOV, aspectRatio, znear, zfar:Single):Matrix;
Var
  left, right, bottom, top:Single;
  ymax, xmax:Single;
  temp, temp2, temp3, temp4:Single;
Begin
  ymax := znear * Tan(FOV * 0.5 * Rad);
  xmax := ymax * aspectRatio;

  left := -xmax;
  right := xmax;
  bottom := -ymax;
  top := ymax;

  temp := znear * 2.0;
  temp2 := (xmax * 2.0);
  temp3 :=  (top - bottom);
  temp4 := 1.0 / (zfar - znear);
  Result.V[0] := temp / temp2;
  Result.V[1] := 0.0;
  Result.V[2] := 0.0;
  Result.V[3] := 0.0;
  Result.V[4] := 0.0;
  Result.V[5] := temp / temp3;
  Result.V[6] := 0.0;
  Result.V[7] := 0.0;
  Result.V[8] := (right + left) / temp2;
  Result.V[9] := (top + bottom) / temp3;
  Result.V[10] := (-zfar - znear) * temp4;
  Result.V[11] := -1.0;
  Result.V[12] := 0.0;
  Result.V[13] := 0.0;
  Result.V[14] := (-temp * zfar) * temp4;
  Result.V[15] := 0.0;
End;

Function MatrixLookAt(Eye, LookAt, Roll:Vector3D):Matrix;
Var
  xaxis, yaxis, zaxis:Vector3D;
Begin
  zaxis := VectorSubtract(Eye, lookAt);
  zaxis.Normalize();
  xaxis := VectorCross(Roll, zaxis);
  xaxis.Normalize();
  yaxis := VectorCross(zaxis, xaxis);

  Result.V[0] := xaxis.x;
  Result.V[1] := yaxis.x;
  Result.V[2] := zaxis.x;
  Result.V[3] := 0.0;
  Result.V[4] := xaxis.y;
  Result.V[5] := yaxis.y;
  Result.V[6] := zaxis.y;
  Result.V[7] := 0.0;
  Result.V[8] := xaxis.z;
  Result.V[9] := yaxis.z;
  Result.V[10] := zaxis.z;
  Result.V[11] := 0.0;
  Result.V[12] := -xaxis.dot(eye);
  Result.V[13] := -yaxis.dot(eye);
  Result.V[14] := -zaxis.dot(eye);
  Result.V[15] := 1.0;
End;

Function MatrixMirror(Const Source,Normal:Vector3D):Matrix;
Var
  Dot:Single;
Begin
  Dot := VectorDot(Source,Normal);

  Result.V[0] := 1.0 - (2.0 *Normal.X * Normal.X);
  Result.V[1] := - (2.0 * Normal.Y * Normal.X);
  Result.V[2] := - (2.0 * Normal.Z * Normal.X);
  Result.V[3] := 0.0;

  Result.V[4] := - (2.0 * Normal.X * Normal.Y);
  Result.V[5] := 1.0 - (2.0 * Normal.Y * Normal.Y);
  Result.V[6] := - (2.0 * Normal.Z * Normal.Y);
  Result.V[7] := 0.0;

  Result.V[8] := - (2.0 * Normal.X * Normal.Z);
  Result.V[9] := - (2.0 * Normal.Y * Normal.Z);
  Result.V[10] := 1.0 - (2.0 * Normal.Z * Normal.Z);
  Result.V[11] := 0.0;

  Result.V[12]:= 2.0 * Dot * Normal.X;
  Result.V[13]:= 2.0 * Dot * Normal.Y;
  Result.V[14]:= 2.0 * Dot * Normal.Z;
  Result.V[15]:= 1.0;
End;

Function MatrixGetTranslation(Const A:Matrix):Vector3D;
Begin
  Result.X := A.V[12];
  Result.Y := A.V[13];
  Result.Z := A.V[14];
End;

Function MatrixGetScale(Const A:Matrix):Vector3D;
Begin
  Result.X := A.V[0];
  Result.Y := A.V[5];
  Result.Z := A.V[10];
End;

// 4x4 matrix inverse using Gauss-Jordan algorithm with row pivoting
// originally written by Nathan Reed, now released into the public domain.
Function MatrixInverse(A:Matrix):Matrix;
Var
  I:Integer;
  a0, a1, a2, a3, a4, a5: Single;
  b0, b1, b2, b3, b4, b5: Single;
  Det, invDet:Single;
Begin
  a0 := A.V[ 0] * A.V[ 5] - A.V[ 1] *A.V[ 4];
  a1 := A.V[ 0] * A.V[ 6] - A.V[ 2] *A.V[ 4];
  a2 := A.V[ 0] * A.V[ 7] - A.V[ 3] *A.V[ 4];
  a3 := A.V[ 1] * A.V[ 6] - A.V[ 2] *A.V[ 5];
  a4 := A.V[ 1] * A.V[ 7] - A.V[ 3] *A.V[ 5];
  a5 := A.V[ 2] * A.V[ 7] - A.V[ 3] *A.V[ 6];
  b0 := A.V[ 8] * A.V[13] - A.V[ 9] *A.V[12];
  b1 := A.V[ 8] * A.V[14] - A.V[10] *A.V[12];
  b2 := A.V[ 8] * A.V[15] - A.V[11] *A.V[12];
  b3 := A.V[ 9] * A.V[14] - A.V[10] *A.V[13];
  b4 := A.V[ 9] * A.V[15] - A.V[11] *A.V[13];
  b5 := A.V[10] * A.V[15] - A.V[11] *A.V[14];

  Det := a0*b5 - a1*b4 + a2*b3 + a3*b2 - a4*b1 + a5*b0;
  If (Abs(Det) > Epsilon) Then
  Begin
    Result.V[ 0] := + A.V[ 5]*b5 - A.V[ 6]*b4 + A.V[ 7]*b3;
    Result.V[ 4] := - A.V[ 4]*b5 + A.V[ 6]*b2 - A.V[ 7]*b1;
    Result.V[ 8] := + A.V[ 4]*b4 - A.V[ 5]*b2 + A.V[ 7]*b0;
    Result.V[12] := - A.V[ 4]*b3 + A.V[ 5]*b1 - A.V[ 6]*b0;
    Result.V[ 1] := - A.V[ 1]*b5 + A.V[ 2]*b4 - A.V[ 3]*b3;
    Result.V[ 5] := + A.V[ 0]*b5 - A.V[ 2]*b2 + A.V[ 3]*b1;
    Result.V[ 9] := - A.V[ 0]*b4 + A.V[ 1]*b2 - A.V[ 3]*b0;
    Result.V[13] := + A.V[ 0]*b3 - A.V[ 1]*b1 + A.V[ 2]*b0;
    Result.V[ 2] := + A.V[13]*a5 - A.V[14]*a4 + A.V[15]*a3;
    Result.V[ 6] := - A.V[12]*a5 + A.V[14]*a2 - A.V[15]*a1;
    Result.V[10] := + A.V[12]*a4 - A.V[13]*a2 + A.V[15]*a0;
    Result.V[14] := - A.V[12]*a3 + A.V[13]*a1 - A.V[14]*a0;
    Result.V[ 3] := - A.V[ 9]*a5 + A.V[10]*a4 - A.V[11]*a3;
    Result.V[ 7] := + A.V[ 8]*a5 - A.V[10]*a2 + A.V[11]*a1;
    Result.V[11] := - A.V[ 8]*a4 + A.V[ 9]*a2 - A.V[11]*a0;
    Result.V[15] := + A.V[ 8]*a3 - A.V[ 9]*a1 + A.V[10]*a0;

    invDet := 1.0 / Det;
    For I:=0 To 15 Do
      Result.V[ I] := Result.V[ I] * invDet;
  End Else
    FillChar(Result, SizeOf(Result), 0);
End;

Function MatrixLerp(Const A,B:Matrix; Const S:Single):Matrix;
Var
  I:Integer;
Begin
  For I:=0 To 15 Do
   Result.V[I] := A.V[I] * S + B.V[I] * (1.0 - S);
End;


Function MatrixMultiply4x4(Const A,B:Matrix):Matrix;
Begin
	Result.V[0] := A.V[0]*B.V[0] + A.V[4]*B.V[1] + A.V[8]*B.V[2] + A.V[12]*B.V[3];
	Result.V[1] := A.V[1]*B.V[0] + A.V[5]*B.V[1] + A.V[9]*B.V[2] + A.V[13]*B.V[3];
	Result.V[2] := A.V[2]*B.V[0] + A.V[6]*B.V[1] + A.V[10]*B.V[2] + A.V[14]*B.V[3];
	Result.V[3] := A.V[3]*B.V[0] + A.V[7]*B.V[1] + A.V[11]*B.V[2] + A.V[15]*B.V[3];

	Result.V[4] := A.V[0]*B.V[4] + A.V[4]*B.V[5] + A.V[8]*B.V[6] + A.V[12]*B.V[7];
	Result.V[5] := A.V[1]*B.V[4] + A.V[5]*B.V[5] + A.V[9]*B.V[6] + A.V[13]*B.V[7];
	Result.V[6] := A.V[2]*B.V[4] + A.V[6]*B.V[5] + A.V[10]*B.V[6] + A.V[14]*B.V[7];
	Result.V[7] := A.V[3]*B.V[4] + A.V[7]*B.V[5] + A.V[11]*B.V[6] + A.V[15]*B.V[7];

	Result.V[8] := A.V[0]*B.V[8] + A.V[4]*B.V[9] + A.V[8]*B.V[10] + A.V[12]*B.V[11];
	Result.V[9] := A.V[1]*B.V[8] + A.V[5]*B.V[9] + A.V[9]*B.V[10] + A.V[13]*B.V[11];
	Result.V[10] := A.V[2]*B.V[8] + A.V[6]*B.V[9] + A.V[10]*B.V[10] + A.V[14]*B.V[11];
	Result.V[11] := A.V[3]*B.V[8] + A.V[7]*B.V[9] + A.V[11]*B.V[10] + A.V[15]*B.V[11];

	Result.V[12] := A.V[0]*B.V[12] + A.V[4]*B.V[13] + A.V[8]*B.V[14] + A.V[12]*B.V[15];
	Result.V[13] := A.V[1]*B.V[12] + A.V[5]*B.V[13] + A.V[9]*B.V[14] + A.V[13]*B.V[15];
	Result.V[14] := A.V[2]*B.V[12] + A.V[6]*B.V[13] + A.V[10]*B.V[14] + A.V[14]*B.V[15];
	Result.V[15] := A.V[3]*B.V[12] + A.V[7]*B.V[13] + A.V[11]*B.V[14] + A.V[15]*B.V[15];
End;

Function MatrixMultiply4x3(Const A,B:Matrix):Matrix;
Begin
	Result.V[0] := A.V[0]*B.V[0] + A.V[4]*B.V[1] + A.V[8]*B.V[2];
	Result.V[1] := A.V[1]*B.V[0] + A.V[5]*B.V[1] + A.V[9]*B.V[2];
	Result.V[2] := A.V[2]*B.V[0] + A.V[6]*B.V[1] + A.V[10]*B.V[2];
	Result.V[3] := 0.0;

	Result.V[4] := A.V[0]*B.V[4] + A.V[4]*B.V[5] + A.V[8]*B.V[6];
	Result.V[5] := A.V[1]*B.V[4] + A.V[5]*B.V[5] + A.V[9]*B.V[6];
	Result.V[6] := A.V[2]*B.V[4] + A.V[6]*B.V[5] + A.V[10]*B.V[6];
	Result.V[7] := 0.0;

	Result.V[8] := A.V[0]*B.V[8] + A.V[4]*B.V[9] + A.V[8]*B.V[10];
	Result.V[9] := A.V[1]*B.V[8] + A.V[5]*B.V[9] + A.V[9]*B.V[10];
	Result.V[10] := A.V[2]*B.V[8] + A.V[6]*B.V[9] + A.V[10]*B.V[10];
	Result.V[11] := 0.0;

	Result.V[12] := A.V[0]*B.V[12] + A.V[4]*B.V[13] + A.V[8]*B.V[14] + A.V[12];
	Result.V[13] := A.V[1]*B.V[12] + A.V[5]*B.V[13] + A.V[9]*B.V[14] + A.V[13];
	Result.V[14] := A.V[2]*B.V[12] + A.V[6]*B.V[13] + A.V[10]*B.V[14] + A.V[14];
	Result.V[15] := 1.0;
End;



Function Vector3D.Get(Index:Integer):Single;
Begin
  Case Index Of
  0:  Result := X;
  1:  Result := Y;
  2:  Result := Z;
  End;
End;

Procedure Vector3D.SetValue(Index:Integer; Value:Single);
Begin
  Case Index Of
  0:  X := Value;
  1:  Y := Value;
  2:  Z := Value;
  End;
End;

Function VectorCreate(Const X,Y,Z:Single):Vector3D; {$IFDEF FPC} Inline;{$ENDIF}
Begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
End;

Function VectorUniform(Const N:Single):Vector3D; {$IFDEF FPC} Inline;{$ENDIF}
Begin
  Result.X := N;
  Result.Y := N;
  Result.Z := N;
End;

Function Vector3D.Equals(Const B:Vector3D):Boolean; {$IFDEF FPC} Inline;{$ENDIF}
Begin
  Result := (Self.X=B.X) And (Self.Y=B.Y) And(Self.Z=B.Z);
End;

Function VectorMax(Const A,B:Vector3D):Vector3D; {$IFDEF FPC} Inline;{$ENDIF}
Begin
  If A.X>B.X Then Result.X:=A.X Else Result.X:=B.X;
  If A.Y>B.Y Then Result.Y:=A.Y Else Result.Y:=B.Y;
  If A.Z>B.Z Then Result.Z:=A.Z Else Result.Z:=B.Z;
End;

Function VectorMin(Const A,B:Vector3D):Vector3D; {$IFDEF FPC} Inline;{$ENDIF}
Begin
  If A.X<B.X Then Result.X:=A.X Else Result.X:=B.X;
  If A.Y<B.Y Then Result.Y:=A.Y Else Result.Y:=B.Y;
  If A.Z<B.Z Then Result.Z:=A.Z Else Result.Z:=B.Z;
End;

Procedure Vector3D.Rotate(Const Axis:Vector3D; Const Angle:Single); {$IFDEF FPC} Inline;{$ENDIF}
Var
  SX,SY,SZ:Single;
  C,S:Single;
Begin
	C := Cos(angle);
	S := Sin(angle);

  SX := X;
  SY := Y;
  SZ := Z;

	X  := (Axis.x*Axis.x*(1-c) + c)	* Sx + (Axis.x*Axis.y*(1-c) - Axis.z*s)	* Sy + (Axis.x*Axis.z*(1-c) + Axis.y*s)	* Sz;
  Y  := (Axis.y*Axis.x*(1-c) + Axis.z*s)	* Sx + (Axis.y*Axis.y*(1-c) + c)	* Sy + (Axis.y*Axis.z*(1-c) - Axis.x*s)	* Sz;
	Z  := (Axis.x*Axis.z*(1-c) - Axis.y*s)	* Sx + (Axis.y*Axis.z*(1-c) + Axis.x*s)	* Sy + (Axis.z*Axis.z*(1-c) + c)	* Sz;
End;

{Function VectorDotRotate(Const Source:Vector3D; Const Matrix:LMatrix):Vector3D;
Begin
  Result.x:=VectorDot(Source,MatrixGetRow(Matrix,0));
  Result.y:=VectorDot(Source,MatrixGetRow(Matrix,1));
  Result.z:=VectorDot(Source,MatrixGetRow(Matrix,2));
End;
}

Procedure Vector3D.Add(Const B:Vector3D); {$IFDEF FPC} Inline; {$ENDIF}
Begin
  X := X + B.X;
  Y := Y + B.Y;
  Z := Z + B.Z;
End;

Procedure Vector3D.Subtract(Const B:Vector3D); {$IFDEF FPC} Inline;{$ENDIF}
Begin
  X := X - B.X;
  Y := Y - B.Y;
  Z := Z - B.Z;
End;

Procedure Vector3D.Scale(Const S:Single); {$IFDEF FPC} Inline;{$ENDIF}
Begin
  X := X * S;
  Y := Y * S;
  Z := Z * S;
End;

Procedure Vector3D.Multiply(Const B:Vector3D); {$IFDEF FPC} Inline;{$ENDIF}
Begin
  X := X * B.X;
  Y := Y * B.Y;
  Z := Z * B.Z;
End;



Function Vector3D.Dot(Const B:Vector3D):Single; {$IFDEF FPC} Inline;{$ENDIF}
Begin
  Result := (X*B.X)+(Y*B.Y)+(Z*B.Z);
End;

Function VectorDot(Const A,B:Vector3D):Single; {$IFDEF FPC} Inline;{$ENDIF}
Begin
  Result := (A.X*B.X)+(A.Y*B.Y)+(A.Z*B.Z);
End;

// R = 2 * ( N dot V ) * V - V
// R =	V - 2 * ( N dot V ) * V
Function VectorReflect(Const Source,Normal:Vector3D):Vector3D; {$IFDEF FPC} Inline;{$ENDIF}
Var
  N:Single;
Begin
  N := VectorDot(Normal,Source) * 2;
  Result := VectorScale(Source, N);
  Result := VectorSubtract(Source,Result);
End;

Function VectorCross(Const A,B:Vector3D):Vector3D; {$IFDEF FPC} Inline;{$ENDIF}
Begin
  With Result Do
  Begin
    X := A.Y*B.Z - A.Z*B.Y;
    Y := A.Z*B.X - A.X*B.Z;
    Z := A.X*B.Y - A.Y*B.X;
  End;
End;

Function VectorSubtract(Const A,B:Vector3D):Vector3D; {$IFDEF FPC} Inline;{$ENDIF}
Begin
  With Result Do
  Begin
    X:=A.X-B.X;
    Y:=A.Y-B.Y;
    Z:=A.Z-B.Z;
  End;
End;

Function VectorScale(Const A:Vector3D; S:Single):Vector3D; {$IFDEF FPC} Inline;{$ENDIF}
Begin
  With Result Do
  Begin
    X := A.X* S;
    Y := A.Y* S;
    Z := A.Z* S;
  End;
End;

Function VectorAdd(Const A,B:Vector3D):Vector3D; {$IFDEF FPC} Inline;{$ENDIF}
Begin
  With Result Do
  Begin
    X:=(A.X+B.X);
    Y:=(A.Y+B.Y);
    Z:=(A.Z+B.Z);
  End;
End;

Function VectorMultiply(Const A,B:Vector3D):Vector3D; {$IFDEF FPC} Inline;{$ENDIF}
Begin
  With Result Do
  Begin
    X:=(A.X*B.X);
    Y:=(A.Y*B.Y);
    Z:=(A.Z*B.Z);
  End;
End;

Function VectorInterpolate(Const A,B:Vector3D; Const S:Single):Vector3D; {$IFDEF FPC} Inline;{$ENDIF}
Begin
  With Result Do
  Begin
    X := (A.X*S)+(B.X*(1-S));
    Y := (A.Y*S)+(B.Y*(1-S));
    Z := (A.Z*S)+(B.Z*(1-S));
  End;
End;

Function VectorBisect(Const A,B:Vector3D):Vector3D; {$IFDEF FPC} Inline;{$ENDIF}
Var
  Len:Single;
Begin
  Result := VectorAdd(A,B);

  Len := Result.Length;
  If (Len<Epsilon) Then
    Result := VectorZero
  Else
    Result := VectorScale(Result, 1.0 / Len);
End;

Function VectorNullify(Const A:Vector3D):Vector3D; {$IFDEF FPC} Inline;{$ENDIF}
Begin
  If Abs(A.X)<Epsilon Then Result.X:=0 Else Result.X:=A.X;
  If Abs(A.Y)<Epsilon Then Result.Y:=0 Else Result.Y:=A.Y;
  If Abs(A.Z)<Epsilon Then Result.Z:=0 Else Result.Z:=A.Z;
End;

{$IFDEF SSE}
{$IFDEF BENCHMARK}
Procedure Vector3D.NormalizeSSE;
{$ELSE}
Procedure Vector3D.Normalize; {$IFDEF FPC} Inline;{$ENDIF}
{$ENDIF}
Asm
  movss xmm0, [eax+8]    // read (z)
  shufps xmm0, xmm0, 0h  // broadcast length
  movlps xmm0, [eax]    // read(x,y)
  movaps xmm2, xmm0      // store a copy of the vector
  mulps xmm0, xmm0      // xmm0 = (sqr(x), sqr(y), sqr(z))
  movaps xmm1, xmm0
  shufps xmm1, xmm1, 55h  // xmm1 = (sqr(y), sqr(y), sqr(y))
  addps xmm1, xmm0        // add both
  shufps xmm0, xmm0, 0AAh  // xmm1 = (sqr(z), sqr(z), sqr(z))
  addps xmm0, xmm1        // sum all
  shufps xmm0, xmm0, 0h  // broadcast length
  xorps xmm1, xmm1
  ucomiss xmm0, xmm1 // check for zero length
  je @@END
  rsqrtps xmm0, xmm0       // get reciprocal of length of vector
  mulps xmm2, xmm0      // normalize

  movlps [eax], xmm2   // store X and Y
  shufps xmm2, xmm2, 0AAh  // xmm1 = (sqr(z), sqr(z), sqr(z))
  movss [eax+8], xmm2   // store Z
@@END:
End;

{$IFDEF BENCHMARK}
Function Vector3D.LengthSSE:Single;
{$ELSE}
Function Vector3D.Length:Single; {$IFDEF FPC} Inline;{$ENDIF}
{$ENDIF}
Asm
  movss xmm0, [eax+8]    // read (z)
  shufps xmm0, xmm0, 0h
  movlps xmm0, [eax]    // read(x,y)

  mulps xmm0, xmm0      // xmm0 = (sqr(x), sqr(y), sqr(z))
  movaps xmm1, xmm0
  shufps xmm1, xmm1, 55h  // xmm1 = (sqr(y), sqr(y), sqr(y))
  addps xmm1, xmm0        // add both
  shufps xmm0, xmm0, 0AAh  // xmm1 = (sqr(z), sqr(z), sqr(z))
  addps xmm0, xmm1        // sum all

  sqrtss xmm0, xmm0       // and finally, sqrt
  movss result, xmm0
End;

{$IFDEF BENCHMARK}
Function Vector3D.DistanceSSE(Const B:Vector3D):Single;
{$ELSE}
Function Vector3D.Distance(Const B:Vector3D):Single; {$IFDEF FPC} Inline;{$ENDIF}
{$ENDIF}
Asm
  movss xmm0, [eax+8]    // read (z)
  shufps xmm0, xmm0, 0h
  movlps xmm0, [eax]    // read(x,y)

  movss xmm1, [edx+8]    // read (b.z)
  shufps xmm1, xmm1, 0h
  movlps xmm1, [edx]    // read(b.x,b.y)
  subps xmm0, xmm1

  mulps xmm0, xmm0      // xmm0 = (sqr(x), sqr(y), sqr(z))
  movaps xmm1, xmm0
  shufps xmm1, xmm1, 55h  // xmm1 = (sqr(y), sqr(y), sqr(y))
  addps xmm1, xmm0        // add both
  shufps xmm0, xmm0, 0AAh  // xmm1 = (sqr(z), sqr(z), sqr(z))
  addps xmm0, xmm1        // sum all

  sqrtss xmm0, xmm0       // and finally, sqrt
  movss result, xmm0
End;
{$ENDIF}

{$IFDEF BENCHMARK} {$UNDEF SSE} {$ENDIF}

{$IFNDEF SSE}

Procedure Vector3D.Normalize; {$IFDEF FPC} Inline;{$ENDIF}
Var
  K:Single;
Begin
  K := Sqr(X) + Sqr(Y) + Sqr(Z);
  {If (K<=SingleOne) Then
    Exit;}

  K := InvSqrt(K);
  X := X * K;
  Y := Y * K;
  Z := Z * K;
End;

Function Vector3D.Length:Single; {$IFDEF FPC} Inline;{$ENDIF}
Begin
  Result := Sqrt(Sqr(X) + Sqr(Y) + Sqr(Z));
End;

Function Vector3D.LengthSquared:Single; {$IFDEF FPC} Inline;{$ENDIF}
Begin
  Result := Sqr(X) + Sqr(Y) + Sqr(Z);
End;

Function Vector3D.Distance(Const B:Vector3D):Single; {$IFDEF FPC} Inline;{$ENDIF}
Begin
  Result := Sqrt(Sqr(Self.X-B.X) + Sqr(Self.Y-B.Y) + Sqr(Self.Z-B.Z));
End;
{$ENDIF}

Function Vector3D.Distance2D(Const B:Vector3D):Single; {$IFDEF FPC} Inline;{$ENDIF}
Begin
  Result := Sqrt(Sqr(Self.X-B.X)+Sqr(Self.Z-B.Z));
End;

Function TriangleNormal(Const V0,V1,V2:Vector3D):Vector3D;  {$IFDEF FPC} Inline;{$ENDIF}
Var
 A,B:Vector3D;
Begin
  A := VectorSubtract(V1,V0);
  B := VectorSubtract(V2,V0);
  Result := VectorCross(A,B);
  Result.Normalize;
End;

Function TriangleHeightNormal(Const V0,V1,V2:Single):Vector3D;
Begin
  Result:=TriangleNormal(VectorCreate(0.0, V0, 0.0),
                         VectorCreate(1.0, V1, 0.0),
                         VectorCreate(0.0, V2, 1.0));
End;

Function GetTriangleHeight(H0,H1,H2:Single; X,Y:Single; Normal:PVector3D=Nil):Single;
Var
  D:Single;
  FloorNormal:Vector3D;
Begin
  FloorNormal := TriangleHeightNormal(H0, H1, H2);

  D := - (FloorNormal.Y * H0);
  Result := - ((FloorNormal.X * X) + (FloorNormal.Z * Y) + D) / FloorNormal.Y;

  If Assigned(Normal) Then
    Normal^ := FloorNormal;
End;


function makeTransform( position:Vector3d; scale:Vector3d; orientation:Quaternion):Matrix;
var
rot3x3 :Matrix3x3;
begin
	    rot3x3:=QuaternionRotationMatrix(orientation);




        // Set up final matrix with scale, rotation and translation
    result.v[0] := scale.x * rot3x3.m[0];
		result.v[4] := scale.y * rot3x3.m[3];
		result.v[8] := scale.z * rot3x3.m[6];
		result.v[12] := position.x;

    result.v[1] := scale.x * rot3x3.m[1];
		result.v[5] := scale.y * rot3x3.m[4];
		result.v[9] := scale.z * rot3x3.m[7];
		result.v[13] := position.y;

    result.v[2] := scale.x * rot3x3.m[2];
		result.v[6] := scale.y * rot3x3.m[5];
		result.v[10] := scale.z * rot3x3.m[8];
		result.v[14] := position.z;

        // No projection term
    result.v[3] := 0;
		result.v[7] := 0;
		result.v[11] := 0;
		result.v[15] := 1;

end;

Function QuaternionCreate(Const X,Y,Z,W:Single):Quaternion;
Begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
  Result.W := W;
End;


{
     * <code>fromAngles</code> builds a Quaternion from the Euler rotation
     * angles (x,y,z) aka (pitch, yaw, rall)). Note that we are applying in order: (y, z, x) aka (yaw, roll, pitch) but
     * we've ordered them in x, y, and z for convenience.
     * @see <a href="http://www.euclideanspace.com/maths/geometry/rotations/conversions/eulerToQuaternion/index.htm">http://www.euclideanspace.com/maths/geometry/rotations/conversions/eulerToQuaternion/index.htm</a>
     *
     * @param xAngle
     *            the Euler pitch of rotation (in radians). (aka Attitude, often rot
     *            around x)
     * @param yAngle
     *            the Euler yaw of rotation (in radians). (aka Heading, often
     *            rot around y)
     * @param zAngle
     *            the Euler roll of rotation (in radians). (aka Bank, often
     *            rot around z)
     */
     }
Function QuaternionCreate(Const V:Vector3D):Quaternion;
var

cosYXcosZ,sinYXsinZ,cosYXsinZ,sinYXcosZ,angle,sinY, sinZ, sinX, cosY, cosZ, cosX:single;
Begin
        angle := v.z * 0.5;
        sinZ := sin(angle);
        cosZ := cos(angle);
        angle := v.y * 0.5;
        sinY := sin(angle);
        cosY := cos(angle);
        angle := v.x * 0.5;
        sinX := sin(angle);
        cosX := cos(angle);

        // variables used to reduce multiplication calls.
         cosYXcosZ := cosY * cosZ;
         sinYXsinZ := sinY * sinZ;
         cosYXsinZ := cosY * sinZ;
         sinYXcosZ := sinY * cosZ;

        result.w := (cosYXcosZ * cosX - sinYXsinZ * sinX);
        result.x := (cosYXcosZ * sinX + sinYXsinZ * cosX);
        result.y := (sinYXcosZ * cosX + cosYXsinZ * sinX);
        result.z := (cosYXsinZ * cosX - sinYXcosZ * sinX);
    //    result.Normalize();

End;

Function Quaternion.Length:Single;
Begin
  Result := Sqrt(Sqr(X) + Sqr(Y) + Sqr(Z) + Sqr(W));
End;

Function Quaternion.Inverse:Quaternion;
var
  inverseNorm,norm:Single;
Begin
 norm := self.w * self.w + self.x * self.x + self.y * self.y + self.z * self.z;
			if( norm > 0.0 )then
			begin
				 inverseNorm := 1.0 / norm;
			  	Result:=QuaternionCreate( self.w * inverseNorm, -self.x * inverseNorm, -self.y * inverseNorm, -self.z * inverseNorm );
			end
			else  result:= QuaternionZero;

End;

Function Quaternion.Equals(Const B:Quaternion):Boolean;
Begin
  Result := (Self.X=B.X) And (Self.Y=B.Y) And(Self.Z=B.Z) And(Self.W=B.W);
End;

Function QuaternionRotation(Const Rotation:Vector3D):Quaternion;
Var
  cos_z_2, cos_y_2, cos_x_2:Single;
  sin_z_2, sin_y_2, sin_x_2:Single;
Begin
  cos_z_2 := Cos(0.5 * Rotation.Z);
  cos_y_2 := Cos(0.5 * Rotation.y);
  cos_x_2 := Cos(0.5 * Rotation.x);

  sin_z_2 := Sin(0.5 * Rotation.z);
  sin_y_2 := Sin(0.5 * Rotation.y);
  sin_x_2 := Sin(0.5 * Rotation.x);

	// and now compute quaternion
	Result.W := cos_z_2*cos_y_2*cos_x_2 + sin_z_2*sin_y_2*sin_x_2;
  Result.X := cos_z_2*cos_y_2*sin_x_2 - sin_z_2*sin_y_2*cos_x_2;
	Result.Y := cos_z_2*sin_y_2*cos_x_2 + sin_z_2*cos_y_2*sin_x_2;
  Result.Z := sin_z_2*cos_y_2*cos_x_2 - cos_z_2*sin_y_2*sin_x_2;

  Result.Normalize;
End;

  function RotationYawPitchRollTo(yaw:Single; pitch:Single; roll:Single):Quaternion;
  var
    halfRoll,halfPitch,halfYaw,sinroll,cosroll,sinPitch,cospitch,sinyaw,cosyaw:Single;
  begin
 halfRoll := roll * 0.5;
         halfPitch := pitch * 0.5;
         halfYaw := yaw * 0.5;

         sinRoll := sin(halfRoll);
         cosRoll := cos(halfRoll);
         sinPitch := sin(halfPitch);
         cosPitch := cos(halfPitch);
         sinYaw := sin(halfYaw);
         cosYaw := cos(halfYaw);

        result.x := (cosYaw * sinPitch * cosRoll) + (sinYaw * cosPitch * sinRoll);
        result.y := (sinYaw * cosPitch * cosRoll) - (cosYaw * sinPitch * sinRoll);
        result.z := (cosYaw * cosPitch * sinRoll) - (sinYaw * sinPitch * cosRoll);
        result.w := (cosYaw * cosPitch * cosRoll) + (sinYaw * sinPitch * sinRoll);
end;

Function QuaternionCreate(Const pMatrix:Matrix):Quaternion;
var
  scale,diagonal:Single;
  q:Quaternion;

begin
    diagonal := pMatrix.v[0] + pMatrix.v[5] + pMatrix.v[10] + 1;
	     scale := 0.0;
	
		 q:= QuaternionCreate(0, 0, 0, 1);


		if(diagonal > 0.00000001)  then
	begin
		// Calculate the scale of the diagonal
		scale := (sqrt(diagonal ) * 2);

		// Calculate the x, y, x and w of the quaternion through the respective equation
		q.x := ( pMatrix.v[9] - pMatrix.v[6] ) / scale;
		q.y := ( pMatrix.v[2] - pMatrix.v[8] ) / scale;
		q.z := ( pMatrix.v[4] - pMatrix.v[1] ) / scale;
		q.w := 0.25 * scale;
	end
	else
    begin
		// If the first element of the diagonal is the greatest value
		if (( pMatrix.v[0] > pMatrix.v[5]) and (pMatrix.v[0] > pMatrix.v[10] )) then
		begin
			// Find the scale according to the first element, and double that value
			scale  := sqrt( 1.0 + pMatrix.v[0] - pMatrix.v[5] - pMatrix.v[10] ) * 2.0;

			// Calculate the x, y, x and w of the quaternion through the respective equation
			q.x := 0.25 * scale;
			q.y := (pMatrix.v[4] + pMatrix.v[1] ) / scale;
			q.z := (pMatrix.v[2] + pMatrix.v[8] ) / scale;
			q.w := (pMatrix.v[9] - pMatrix.v[6] ) / scale;
		end
		// Else if the second element of the diagonal is the greatest value
		else if ( pMatrix.v[5] > pMatrix.v[10] ) then
		begin
			// Find the scale according to the second element, and double that value
			scale  := sqrt( 1.0 + pMatrix.v[5] - pMatrix.v[0] - pMatrix.v[10] ) * 2.0;
			
			// Calculate the x, y, x and w of the quaternion through the respective equation
			q.x := (pMatrix.v[4] + pMatrix.v[1] ) / scale;
			q.y := 0.25 * scale;
		 	q.z := (pMatrix.v[9] + pMatrix.v[6] ) / scale;
			q.w := (pMatrix.v[2] - pMatrix.v[8] ) / scale;
		end
		// Else the third element of the diagonal is the greatest value
		else
		begin
			// Find the scale according to the third element, and double that value
			scale  := sqrt( 1.0 + pMatrix.v[10] - pMatrix.v[0] - pMatrix.v[5] ) * 2.0;

			// Calculate the x, y, x and w of the quaternion through the respective equation
			q.x := (pMatrix.v[2] + pMatrix.v[8] ) / scale;
			q.y := (pMatrix.v[9] + pMatrix.v[6] ) / scale;
			q.z := 0.25 * scale;
			q.w := (pMatrix.v[4] - pMatrix.v[1] ) / scale;
		end;

	end;

      Result:= q;

end;

Function QuaternionRotationMatrix(Const q:Quaternion):Matrix3x3;
var
  xx,yy,zz,xy,zw,zx,yw,yz,xw:Single;
begin
        xx := q.x * q.x;
         yy := q.y * q.y;
         zz := q.z * q.z;
         xy := q.x * q.y;
         zw := q.z * q.w;
         zx := q.z * q.x;
         yw := q.y * q.w;
         yz := q.y * q.z;
         xw := q.x * q.w;

        result.m[0] := 1.0 - (2.0 * (yy + zz));
        result.m[1] := 2.0 * (xy + zw);
        result.m[2] := 2.0 * (zx - yw);

	    	result.m[3] := 2.0 * (xy - zw);
        result.m[4] := 1.0 - (2.0 * (zz + xx));
        result.m[5] := 2.0 * (yz + xw);

	    	result.m[6] := 2.0 * (zx + yw);
        result.m[7] := 2.0 * (yz - xw);
        result.m[8] := 1.0 - (2.0 * (yy + xx));
end;





Function QuaternionToMatrix(Pos:Vector3D;q:Quaternion):Matrix;

Begin
  Q.Normalize;

  Result.V[0]:= 1.0 - 2.0*Q.Y*Q.Y -2.0 *Q.Z*Q.Z;
  Result.V[1]:= 2.0 * Q.X*Q.Y + 2.0 * Q.W*Q.Z;
  Result.V[2]:= 2.0 * Q.X*Q.Z - 2.0 * Q.W*Q.Y;
  Result.V[3] := 0;

  Result.V[4]:= 2.0 * Q.X*Q.Y - 2.0 * Q.W*Q.Z;
  Result.V[5]:= 1.0 - 2.0 * Q.X*Q.X - 2.0 * Q.Z*Q.Z;
  Result.V[6]:= 2.0 * Q.Y*Q.Z + 2.0 * Q.W*Q.X;
  Result.V[7] := 0;

  Result.V[8] := 2.0 * Q.X*Q.Z + 2.0 * Q.W*Q.Y;
  Result.V[9] := 2.0 * Q.Y*Q.Z - 2.0 * Q.W*Q.X;
  Result.V[10] := 1.0 - 2.0 * Q.X*Q.X - 2.0 * Q.Y*Q.Y;
  Result.V[11] := 0;

  Result.V[12] :=pos.x;
  Result.V[13] :=pos.y;
  Result.V[14] :=pos.z;
  Result.V[15] := 1.0;
End;

Function QuaternionMatrix(Const q:Quaternion):Matrix;

Begin
  Q.Normalize;

  Result.V[0]:= 1.0 - 2.0*Q.Y*Q.Y -2.0 *Q.Z*Q.Z;
  Result.V[1]:= 2.0 * Q.X*Q.Y + 2.0 * Q.W*Q.Z;
  Result.V[2]:= 2.0 * Q.X*Q.Z - 2.0 * Q.W*Q.Y;
  Result.V[3] := 0;

  Result.V[4]:= 2.0 * Q.X*Q.Y - 2.0 * Q.W*Q.Z;
  Result.V[5]:= 1.0 - 2.0 * Q.X*Q.X - 2.0 * Q.Z*Q.Z;
  Result.V[6]:= 2.0 * Q.Y*Q.Z + 2.0 * Q.W*Q.X;
  Result.V[7] := 0;

  Result.V[8] := 2.0 * Q.X*Q.Z + 2.0 * Q.W*Q.Y;
  Result.V[9] := 2.0 * Q.Y*Q.Z - 2.0 * Q.W*Q.X;
  Result.V[10] := 1.0 - 2.0 * Q.X*Q.X - 2.0 * Q.Y*Q.Y;
  Result.V[11] := 0;

  Result.V[12] := 0.0;
  Result.V[13] := 0.0;
  Result.V[14] := 0.0;
  Result.V[15] := 1.0;
End;

Function QuaternionMultiply( Ql,Qr:Quaternion):Quaternion;
Begin
  Result.W := qL.W * qR.W - qL.X * qR.X - qL.Y * qR.Y - qL.Z * qR.Z;
  Result.X := qL.W * qR.X + qL.X * qR.W + qL.Y * qR.Z - qL.Z * qR.Y;
  Result.Y := qL.W * qR.Y + qL.Y * qR.W + qL.Z * qR.X - qL.X * qR.Z;
  Result.Z := qL.W * qR.Z + qL.Z * qR.W + qL.X * qR.Y - qL.Y * qR.X;
End;

Procedure Quaternion.Normalize;
Var
  Len:Single;
Begin
  Len := Sqrt(Sqr(X) + Sqr(Y) + Sqr(Z) + Sqr(W));
  If (Len<=0) Then
    Exit;

  Len := 1.0 / Len;
  X := X * Len;
  Y := Y * Len;
  Z := Z * Len;
  W := W * Len;

End;

Function QuaternionSlerp(A,B:Quaternion; Const T:Single):Quaternion;
Var
  Theta, Sine, Beta, Alpha:Single;
  Cosine:Single;
Begin
  Cosine := a.x*b.x + a.y*b.y + a.z*b.z + a.w*b.w;
  Cosine := Abs(Cosine);

  If ((1-cosine)>Epsilon) Then
  Begin
    Theta := ArcCos(cosine);
  	Sine := Sin(theta);

  	Beta := Sin((1-t)*theta) / sine;
  	Alpha := Sin(t*theta) / sine;
  End Else
  Begin
    Beta := (1.0 - T);
    Alpha := T;
  End;

  Result.X := A.X * Beta + B.X * Alpha;
  Result.Y := A.Y * Beta + B.Y * Alpha;
  Result.Z := A.Z * Beta + B.Z * Alpha;
  Result.W := A.W * Beta + B.W * Alpha;
End;

Procedure Quaternion.Add(Const B:Quaternion);
Begin
  X := X + B.X;
  Y := Y + B.Y;
  Z := Z + B.Z;
//  W := W + B.W;
End;

Procedure Quaternion.Subtract(Const B:Quaternion);
Begin
  X := X - B.X;
  Y := Y - B.Y;
  Z := Z - B.Z;
//  W := W - B.W;
End;

Function QuaternionAdd(Const A,B:Quaternion):Quaternion;
Begin
  With Result Do
  Begin
    X := A.X + B.X;
    Y := A.Y + B.Y;
    Z := A.Z + B.Z;
    W := A.W + B.W;
  End;
End;

Function QuaternionScale(Const Q:Quaternion; Const Scale:Single):Quaternion;
Begin
  With Result Do
  Begin
    X := Q.X * Scale;
    Y := Q.Y * Scale;
    Z := Q.Z * Scale;
    W := Q.W * Scale;
  End;
End;

Function QuaternionFromBallPoints(Const arcFrom,arcTo:Vector3D):Quaternion;
Begin
  Result.X := arcFrom.Y * arcTo.Z - arcFrom.Z * arcTo.Y;
  Result.Y := arcFrom.Z * arcTo.X - arcFrom.X * arcTo.Z;
  Result.Z := arcFrom.X * arcTo.Y - arcFrom.Y * arcTo.X;
  Result.W := arcFrom.X * arcTo.X + arcFrom.Y * arcTo.Y + arcFrom.Z * arcTo.Z;
End;

Procedure QuaternionToBallPoints(Var Q:Quaternion; arcFrom,arcTo:Vector3D);
Var
  S:Single;
Begin
  S := Sqrt(Sqr(Q.X) + Sqr(Q.Y));

  If s=0 Then
    arcFrom:=VectorCreate(0.0, 1.0, 0.0)
  Else
    arcFrom:=VectorCreate(-Q.Y/S, Q.X/S, 0.0);

  arcTo.X := (Q.W * arcFrom.X) - (Q.Z * arcFrom.Y);
  arcTo.Y := (Q.W * arcFrom.Y) + (Q.Z * arcFrom.X);
  arcTo.Z := (Q.X * arcFrom.Y) - (Q.Y * arcFrom.X);

  If Q.W<0.0 Then
    arcFrom := VectorCreate(-arcFrom.X, -arcFrom.Y, 0.0);
End;

Function QuaternionConjugate(Const Q:Quaternion):Quaternion;
Begin
  Result.X :=  -Q.X;
  Result.Y := -Q.Y;
  Result.Z := -Q.Z;
  Result.W := Q.W;
End;

Function VectorCreate4D(Const X,Y,Z,W:Single):Quaternion;
Begin
 Result.X := X;
 Result.Y := Y;
 Result.Z := Z;
 Result.W := W;
End;


Procedure Quaternion.Transform(Const M:Matrix);
Var
  QX,QY,QZ,QW:Single;
Begin
  QX := X;
  QY := Y;
  QZ := Z;
  QW := W;

  X := QX*M.V[0] + QY*M.V[4] + QZ*M.V[8]  + QW*M.V[12];
  Y := QX*M.V[1] + QY*M.V[5] + QZ*M.V[9]  + QW*M.V[13];
  Z := QX*M.V[2] + QY*M.V[6] + QZ*M.V[10] + QW*M.V[14];
  W := QX*M.V[3] + QY*M.V[7] + QZ*M.V[11] + QW*M.V[15];
End;


//! converts from a normalized axis - angle pair rotation to a quaternion
Function QuaternionFromAxisAngle(Const Axis:Vector3D; Const Angle:Single):Quaternion;
Var
  S:Single;
Begin
  S := Sin(Angle/2);
  Result.X := Axis.X * S;
  Result.Y := Axis.Y * S;
  Result.Z := Axis.Z * S;
  Result.W := Cos(angle/2);
End;

Function sgn (a : real) : real;
Begin
  if (a < 0) then
    sgn := -1
  else
    sgn :=  1;
End;

Function atan2 (y, x : Extended) : Extended;
Begin
  if x > 0       then  result := arctan (y/x)
  else if x < 0  then  result := arctan (y/x) + pi
  else                 result := pi/2 * sgn (y);
End;

Function QuaternionToEuler(Const Q:Quaternion):Vector3D;
Var
  sqw, sqx, sqy, sqz:Single;
Begin
{  Result.X := Atan2(2 * q.Y * q.W - 2 * q.X * q.Z,
 	                1 - 2* Pow(q.Y, 2) - 2*Pow(q.Z, 2)   );

  Result.Y := Arcsin(2*q.X*q.Y + 2*q.Z*q.W);

  Result.Z := Atan2(2*q.X*q.W-2*q.Y*q.Z,
 	                1 - 2*Pow(q.X, 2) - 2*Pow(q.Z, 2)     );

  If (q.X*q.Y + q.Z*q.W = 0.5) Then
  Begin
    Result.X := (2 * Atan2(q.X,q.W));
 	  Result.Z := 0;
  End Else
  If (q.X*q.Y + q.Z*q.W = -0.5) Then
  Begin
    Result.X := (-2 * Atan2(q.X, q.W));
    Result.Z := 0;
  End;}

	sqx := Sqr(Q.X);
	sqy := Sqr(Q.Y);
	sqz := Sqr(Q.Z);

		{if (homogenous) Then
			euler.x = atan2f(2.f * (v.x*v.y + v.z*s), sqx - sqy - sqz + sqw);
			euler.y = asinf(-2.f * (v.x*v.z - v.y*s));
			euler.z = atan2f(2.f * (v.y*v.z + v.x*s), -sqx - sqy + sqz + sqw);
		End Else
    }

  Result.x := atan2(2 * (Q.z*Q.y + Q.x*Q.W), 1 - 2*(sqx + sqy));
  Result.y := arcsin(-2 * (Q.x*Q.z - Q.y*Q.W));
  Result.z := atan2(2 * (Q.x*Q.y + Q.z*Q.W), 1 - 2*(sqy + sqz));
End;


Function VectorCreate2D(Const X,Y:Single):Vector2D; {$IFDEF FPC} Inline;{$ENDIF}
Begin
  Result.X := X;
  Result.Y := Y;
End;

Function VectorCross2D(Const A,B:Vector2D):Single; {$IFDEF FPC} Inline;{$ENDIF}
Begin
  Result := (A.X * B.Y) - (A.Y * B.X);
End;

Function Vector2D.Equals(Const B:Vector2D):Boolean; {$IFDEF FPC} Inline;{$ENDIF}
Begin
  Result := (Self.X = B.X) And (Self.Y = B.Y);
End;

Function VectorAdd2D(Const A,B:Vector2D):Vector2D;
Begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
End;

Function VectorSubtract2D(Const A,B:Vector2D):Vector2D;
Begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
End;

Procedure Vector2D.Project(Const V:Vector2D);
Var
  thisDotV:Single;
Begin
  thisDotV := Self.Dot(V);
  Self.X := V.X * thisDotV;
  Self.Y := V.Y * thisDotV;
End;

Procedure Vector2D.Rotate(Const Angle:Single; Const Center:Vector2D); {$IFDEF FPC} Inline;{$ENDIF}
Begin
  Self.Subtract(Center);
  Self.Rotate(Angle);
  Self.Add(Center);
End;

Procedure Vector2D.Rotate(Const Angle:Single); {$IFDEF FPC} Inline;{$ENDIF}
Var
  SX,SY:Single;
  Sine,Cosine:Single;
Begin
  SX := Self.X;
  SY := Self.Y;
  Sine := Sin(Angle);
  Cosine := Cos(Angle);
  X := (Sx * Cosine) - (Sy * Sine);
  Y := (Sx * Sine) + (Sy * Cosine);
End;

Procedure Vector2D.Add(Const B:Vector2D); {$IFDEF FPC} Inline;{$ENDIF}
Begin
  X := X + B.X;
  Y := Y + B.Y;
End;

{Procedure Vector2D.AddSSE(Const B:Vector2D);
Asm
  movlps xmm0, [self]   // xmm0 = (x,y)
  movlps xmm1, [edx]   // xmm1 = (b.x,b.y)
  addps xmm0, xmm1
  movlps [self], xmm0    // store result
End;
}

Procedure Vector2D.Subtract(Const B:Vector2D); {$IFDEF FPC} Inline;{$ENDIF}
Begin
  X := X - B.X;
  Y := Y - B.Y;
End;

Procedure Vector2D.Scale(Const S:Single); {$IFDEF FPC} Inline;{$ENDIF}
Begin
  X := X * S;
  Y := Y * S;
End;

Procedure Vector2D.Scale(Const B:Vector2D); {$IFDEF FPC} Inline;{$ENDIF}
Begin
  X := X * B.X;
  Y := Y * B.Y;
End;

{$IFDEF SSE}
{$IFDEF BENCHMARK}
Function Vector2D.LengthSSE:Single;Register;
{$ELSE}
Function Vector2D.Length:Single;Register;
{$ENDIF}
Asm
  movlps xmm0, [eax]   // xmm0 = (x,y)
  mulps xmm0, xmm0      // xmm0 = (sqr(x), sqr(y))
  movaps xmm1, xmm0
  shufps xmm1, xmm1, 85  // xmm1 = (sqr(y), sqr(y))
  addps xmm0, xmm1        // add both

  sqrtss xmm0, xmm0       // and finally, sqrt}
  movss result, xmm0
End;

{$IFDEF BENCHMARK}
Function Vector2D.DistanceSSE(Const B:Vector2D):Single;Register;
{$ELSE}
Function Vector2D.Distance(Const B:Vector2D):Single;Register;
{$ENDIF}
Asm
  movlps xmm0, [eax]   // xmm0 = (x,y)
  movlps xmm1, [edx]   // xmm0 = (b.x,b.y)
  subps xmm0, xmm1
  mulps xmm0, xmm0      // xmm0 = (sqr(x), sqr(y))
  movaps xmm1, xmm0
  shufps xmm1, xmm1, 55h  // xmm1 = (sqr(y), sqr(y))
  addps xmm0, xmm1        // add both
  sqrtss xmm1, xmm0       // and finally, sqrt
  movss result, xmm1
End;
{$ENDIF}

{$IFDEF BENCHMARK} {$UNDEF SSE} {$ENDIF}

{$IFNDEF SSE}
Function Vector2D.Length:Single;
Begin
  Result := Sqrt(Sqr(X)+Sqr(Y));
End;

Function Vector2D.Distance(Const B:Vector2D):Single;
Begin
  Result := Sqrt(Sqr(Self.X-B.X)+Sqr(Self.Y-B.Y));
End;
{$ENDIF}

Procedure Vector2D.Normalize;
Var
  K:Single;
Begin
  K := Length;
  If (K<=1.0) Then
    Exit;
    
  X := X / K;
  Y := Y / K;
End;

Function Vector2D.Dot(B:Vector2D):Single;
Begin
  Result := (Self.X * B.X) + (Self.Y * B.Y);
End;


End.
