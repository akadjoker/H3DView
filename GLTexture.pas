

unit GLTexture;

interface

uses
  Windows, Messages, SysUtils, Classes, JPEG, Graphics, dglOpenGL, GraphicEx;

{$WARN  UNSAFE_TYPE OFF}
{$WARN  UNSAFE_CODE OFF}

Type TColor3b =  Record
  Red   : GLubyte;
  Green : GLubyte;
  Blue  : GLubyte;
end;

// Type TColor4b
//------------------------------------------------------------------------------
Type TColor4b =  Record
  Red   : GLubyte;
  Green : GLubyte;
  Blue  : GLubyte;
  Alpha : GLubyte;
end;
//------------------------------------------------------------------------------
Type TTextureData24     = Array of TColor3b;
Type TTextureData32     = Array of TColor4b;
Type TTextureDataFormat = (dfNone, df24, df32);
Type TPatternRect       = record Left, Top, Right, Bottom: glFloat; end;
Type TGLXGraphicType    = (gtNone, gtBitmap, gtJpeg, gtTarga, gtPng);


//------------------------------------------------------------------------------
Type TGLTexture = class(TPersistent)
  private
    { Private declarations }
    FGraphicType     : TGLXGraphicType;
    FBitmap          : TBitmap;
    FJpeg            : TJPEGImage;
    FTarga           : TTargaGraphic;
    FPng             : TPNGGraphic;
    FDataFormat      : TTextureDataFormat;
    FData24          : TTextureData24;
    FData32          : TTextureData32;
    FTransparent     : Boolean;
    FTransparentColor: TColor;
    FTexture         : glUint;

    function GetHeight: Integer;
    function GetWidth: Integer;

    procedure CreateBlankBitmap(Bitmap: TBitmap);

    procedure SetBitmap(const Value: TBitmap);
    procedure SetJpeg  (const Value: TJPEGImage);
    procedure SetTarga (const Value: TTargaGraphic);
    procedure SetPng   (const Value: TPNGGraphic);
    function GetPixels(X, Y: Integer): TColor;
    procedure SetPixels(X, Y: Integer; const Value: TColor);

    procedure SetTransparent     (const Value: Boolean);
    procedure SetTransparency;
  public
    { Public declarations }
    constructor Create;
    destructor Destroy; override;
    Procedure Assign(Source: TPersistent); override;

    procedure LoadTexture(FileName: String);                                         overload;


    procedure BuildTexture;                       overload;
    procedure BuildTexture (var Texture: glUint); overload;
    procedure UpdateTexture;                      overload;
    procedure UpdateTexture(var Texture: glUint); overload;
    Procedure DeleteTexture;                      overload;
    Procedure DeleteTexture(var Texture: glUint); overload;

    procedure UpdateTexture(Bitmap: TBitmap);                      overload;
    procedure UpdateTexture(Bitmap: TBitmap; var Texture: glUint); overload;

    Procedure Bind;
    Procedure Scale(Factor: Single);


    property GraphicType         : TGLXGraphicType    read FGraphicType      write FGraphicType;
    property Bitmap              : TBitmap            read FBitmap           write SetBitmap;
    property Jpeg                : TJPEGImage         read FJpeg             write SetJpeg;
    property Targa               : TTargaGraphic      read FTarga            write SetTarga;
    property Png                 : TPNGGraphic        read FPng              write SetPng;
    property Transparent         : Boolean            read FTransparent      write SetTransparent;
    property TransparentColor    : TColor             read FTransparentColor write FTransparentColor;
    property Texture             : glUint             read FTexture          write FTexture;
    property Width               : Integer            read GetWidth;
    property Height              : Integer            read GetHeight;

    property DataFormat          : TTextureDataFormat read FDataFormat       write FDataFormat;
    property Data24              : TTextureData24     read FData24           write FData24;
    property Data32              : TTextureData32     read FData32           write FData32;
    property Pixels[X,Y: Integer]: TColor             read GetPixels         write SetPixels;
  end;

//------------------------------------------------------------------------------
Type TRGBQuadArray = Array[WORD] of TRGBQuad;
Type PRGBQuadArray = ^TRGBQuadArray;

Type TRGBTripleArray = Array[WORD] of TRGBTriple;
Type PRGBTripleArray = ^TRGBTripleArray;

//------------------------------------------------------------------------------
implementation

Uses Dialogs;

//------------------------------------------------------------------------------
function RGBA(r, g, b, a: Byte): COLORREF;
begin
  Result := (r or (g shl 8) or (b shl 16) or (a shl 24));
end;

//------------------------------------------------------------------------------
function GetAValue(rgb: DWORD): Byte;
begin
  Result := Byte(rgb shr 24);
end;

// Creates a texture from the data
//------------------------------------------------------------------------------
function CreateTexture(Width, Height, Format : Word; pData : Pointer) : GLuint;

begin
  glGenTextures(1, @result);
  glBindTexture(GL_TEXTURE_2D, result);
//  glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);  {Texture blends with object background}

  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR); { only first two can be used }
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR); { all of the above can be used }

  if Format = GL_RGBA then begin
    gluBuild2DMipmaps(GL_TEXTURE_2D, GL_RGBA, Width, Height, GL_RGBA, GL_UNSIGNED_BYTE, pData)
  end else begin
    gluBuild2DMipmaps(GL_TEXTURE_2D, GL_RGB, Width, Height, GL_RGB, GL_UNSIGNED_BYTE, pData);
  end;

end;


// Converts a RGB bmp to data
//------------------------------------------------------------------------------
Procedure CreateData(Bitmap: TBitmap; var Data: TTextureData24); overload;
var X, Y: Integer;
var Pix : TRGBTriple;
var Line: PRGBTripleArray;
begin
  SetLength(Data, Bitmap.Width * Bitmap.Height);

  For Y:=0 to Bitmap.Height-1 do begin
    Line := Bitmap.ScanLine[Bitmap.Height-Y-1];
    For X:=0 to Bitmap.Width-1 do begin
      Pix:=Line[X];

      Data[X+(Y*Bitmap.Width)].Red  :=Pix.rgbtRed;
      Data[X+(Y*Bitmap.Width)].Green:=Pix.rgbtGreen;
      Data[X+(Y*Bitmap.Width)].Blue :=Pix.rgbtBlue;
    end;
  end;
end;

// Converts a RGBA bmp to data
//------------------------------------------------------------------------------
Procedure CreateData(Bitmap: TBitmap; var Data: TTextureData32); overload;
var X, Y: Integer;
var Pix : TRGBQuad ;
var Line: PRGBQuadArray;
begin
  SetLength(Data, Bitmap.Width * Bitmap.Height);

  For Y:=0 to Bitmap.Height-1 do begin
    Line := Bitmap.ScanLine[Bitmap.Height-Y-1];
    For X:=0 to Bitmap.Width-1 do begin
      Pix:=Line[X];

      Data[X+(Y*Bitmap.Width)].Red  :=Pix.rgbRed;
      Data[X+(Y*Bitmap.Width)].Green:=Pix.rgbGreen;
      Data[X+(Y*Bitmap.Width)].Blue :=Pix.rgbBlue;
      Data[X+(Y*Bitmap.Width)].Alpha:=Pix.rgbReserved;
   end;
  end;
end;

// Scales a 24 bit bitmap
//------------------------------------------------------------------------------
Procedure ScaleBitmap24(Source, Dest: TBitmap; Scale: Single);
var X, Y: Integer;
//var SPix : TRGBTriple ;
var SLine: PRGBTripleArray;
var DLine: PRGBTripleArray;
begin
  // Can't scale to 0
  IF Scale = 0 then Scale:= 1;

  Dest.PixelFormat:=pf24Bit;
  Dest.Width      :=Trunc(Source.Width  * Scale);
  Dest.Height     :=Trunc(Source.Height * Scale);

  For Y:=0 to Dest.Height-1 do begin
    DLine := Dest  .ScanLine[      Y         ];
    SLine := Source.ScanLine[Trunc(Y / Scale)];
    For X:=0 to Dest.Width-1 do begin
      DLine[X] := SLine[ Trunc(X / Scale) ];
   end;
  end;

  Dest  .SaveToFile('Scaled.bmp');
  Source.SaveToFile('Original.bmp');
end;

//------------------------------------------------------------------------------
Procedure ScaleBitmap32(Source, Dest: TBitmap; Scale: Single);
var X, Y: Integer;
//var SPix : TRGBTriple ;
var SLine: PRGBQuadArray;
var DLine: PRGBQuadArray;
begin
  // Can't scale to 0
  IF Scale = 0 then Scale:= 1;

  Dest.PixelFormat:=pf24Bit;
  Dest.Width      :=Trunc(Source.Width  * Scale);
  Dest.Height     :=Trunc(Source.Height * Scale);

  For Y:=0 to Dest.Height-1 do begin
    DLine := Dest  .ScanLine[      Y         ];
    SLine := Source.ScanLine[Trunc(Y / Scale)];
    For X:=0 to Dest.Width-1 do begin
      DLine[X] := SLine[ Trunc(X / Scale) ];
   end;
  end;

  Dest  .SaveToFile('Scaled.bmp');
  Source.SaveToFile('Original.bmp');
end;




//Class TGLTexture
//==============================================================================
constructor TGLTexture.Create;
begin
  FGraphicType     := gtNone;
  FBitmap          := TBitmap.Create;
  FJpeg            := TJPEGImage.Create;
  FTarga           := TTargaGraphic.Create;
  FPNG             := TPNGGraphic.Create;
  FDataFormat      := dfNone;
  FTransparent     := False;
  FTransparentColor:= clBlack;
end;

//------------------------------------------------------------------------------
destructor TGLTexture.Destroy;
begin
  DeleteTexture;

  FBitmap.Free;
  FJpeg  .Free;
  FTarga .Free;
  FPNG   .Free;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TGLTexture.CreateBlankBitmap(Bitmap: TBitmap);
begin
  Bitmap.Width :=128;
  Bitmap.Height:=128;
  with Bitmap.Canvas do begin
    Brush.Color:=clWhite;
    Brush.Style:=bsSolid;
    Pen.Color  :=clWhite;
  end;
end;

//------------------------------------------------------------------------------
procedure TGLTexture.BuildTexture;
begin
  BuildTexture(FTexture);
end;


//------------------------------------------------------------------------------
procedure TGLTexture.BuildTexture(var Texture: glUint);
var Bitmap: TBitmap;
begin
 // DeleteTexture(Texture);

  SetLength(FData32, 0);
  SetLength(FData24, 0);

  Bitmap:=TBitmap.Create;
  try
    Case FGraphicType of
      gtNone  : CreateBlankBitmap(Bitmap);
      gtBitmap: Bitmap.Assign(FBitmap);
      gtJpeg  : Bitmap.Assign(FJpeg);
      gtTarga : Bitmap.Assign(FTarga);
      gtPng   : Bitmap.Assign(FPng);
    end;

    //RGBA
    IF Bitmap.PixelFormat = pf32bit then begin
      CreateData(Bitmap, FData32);
      FDataFormat:=df32;

      Texture:=CreateTexture(Bitmap.Width, Bitmap.Height, GL_RGBA, Addr(Data32[0]));
    end else
    //RGB
    IF Bitmap.PixelFormat = pf24bit then begin
      CreateData(Bitmap, FData24);
      FDataFormat:=df24;

      Texture:=CreateTexture(Bitmap.Width, Bitmap.Height, GL_RGB, Addr(Data24[0]));
    end else
    // Other, convert to rgb
    begin
      Bitmap.PixelFormat:=pf24bit;
      CreateData(Bitmap, FData24);
      FDataFormat:=df24;

      Texture:=CreateTexture(Bitmap.Width, Bitmap.Height, GL_RGB, Addr(Data24[0]));
    end;

  finally
    Bitmap.Free;
  end;
end;

var TransparentColors : Array[Boolean] of Byte = (255, 0);

//------------------------------------------------------------------------------
procedure TGLTexture.SetTransparency;
var Counter: Integer;
var Color3 : TColor3b;
var Color4 : TColor4b;
var Red, Green, Blue: Byte;
begin

  // Get the RGB of the transparentcolor
  Red  :=GetRvalue(FTransparentColor);
  Green:=GetGvalue(FTransparentColor);
  Blue :=GetBvalue(FTransparentColor);

  // If the image is transparent
  IF FTransparent then begin

    // 24 bit image data
    if FDataFormat = df24 then begin
      // Convert to 32 bit image
      SetLength(FData32, Width * Height);

      // Loop trought the image data
      For Counter:=0 to (Width * Height)-1 do begin
        Color3:=FData24[Counter];
        Color4.Red  :=Color3.Red;
        Color4.Green:=Color3.Green;
        Color4.Blue :=Color3.Blue;
        Color4.Alpha:=TransparentColors[(Color4.Red = Red) and (Color4.Green = Green) and (Color4.Blue = Blue)];
        FData32[Counter]:=Color4;
      end;
      FDataFormat:=df32;
      SetLength(FData24, 0);
    end;
    // 32 bit image data
    if FDataFormat = df32 then begin

      // Loop trought the image data
      For Counter:=0 to (Width * Height)-1 do begin
        Color4:=FData32[Counter];
        Color4.Alpha:=TransparentColors[(Color4.Red = Red) and (Color4.Green = Green) and (Color4.Blue = Blue)];
        FData32[Counter]:=Color4;
      end;
    end;
    // Delete the texture
    DeleteTexture;
    // Rebuild the texture
    Texture:=CreateTexture(Width, Height, GL_RGBA, Addr(Data32[0]));
  end else begin
    // To remove transparency, we'll have to rebuild the texture
    BuildTexture;
  end;
end;      

//------------------------------------------------------------------------------
procedure TGLTexture.UpdateTexture;
begin
  UpdateTexture(FTexture);
end;

//------------------------------------------------------------------------------
procedure TGLTexture.UpdateTexture(Bitmap: TBitmap);
begin
  UpdateTexture(Bitmap, FTexture);
end;

//------------------------------------------------------------------------------
procedure TGLTexture.UpdateTexture(var Texture: glUint);
begin
  glBindTexture(GL_TEXTURE_2D, Texture);
  case FDataFormat of
    df24: glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB , Width, Height, 0, GL_RGB , GL_UNSIGNED_BYTE, Addr(FData24[0]));
    df32: glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, Width, Height, 0, GL_RGBA, GL_UNSIGNED_BYTE, Addr(FData32[0]));
  end;
end;


//------------------------------------------------------------------------------
procedure TGLTexture.UpdateTexture(Bitmap: TBitmap; var Texture: glUint);
begin
  glBindTexture(GL_TEXTURE_2D, Texture);

  case FDataFormat of
    df24: CreateData(Bitmap, FData24);
    df32: CreateData(Bitmap, FData32);
  end;

  case FDataFormat of
    df24: glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB , Width, Height, 0, GL_RGB , GL_UNSIGNED_BYTE, Addr(FData24[0]));
    df32: glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, Width, Height, 0, GL_RGBA, GL_UNSIGNED_BYTE, Addr(FData32[0]));
  end;
end;

//------------------------------------------------------------------------------
procedure TGLTexture.Scale(Factor: Single);
var Source, Dest: TBitmap;
begin
  // No div's by zero
  IF Factor = 0 then Exit;

  Source:=TBitmap.Create;
  Dest  :=TBitmap.Create;
  try
    Case FGraphicType of
      gtBitmap: Source.Assign(FBitmap);
      gtJpeg  : Source.Assign(FJpeg);
      gtTarga : Source.Assign(FTarga);
      gtPng   : Source.Assign(FPng);
    end;


    //RGBA
    IF Bitmap.PixelFormat = pf32bit then begin
      ScaleBitmap32(Source, Dest, Factor);
    end else
    //RGB
    IF Bitmap.PixelFormat = pf24bit then begin
      ScaleBitmap24(Source, Dest, Factor);
    end;
    // Other, convert to rgb
    begin
      Bitmap.PixelFormat:=pf24bit;
      ScaleBitmap24(Source, Dest, Factor);
    end;

    Case FGraphicType of
      gtBitmap: FBitmap.Assign(Dest);
      gtJpeg  : FJpeg  .Assign(Dest);
      gtTarga : FTarga .Assign(Dest);
      gtPng   : FPng   .Assign(Dest);
    end;

  finally
    Source.Free;
    Dest  .Free;
  end;
  BuildTexture;
end;



//------------------------------------------------------------------------------
procedure TGLTexture.DeleteTexture;
begin
  IF FTexture <> 0 then glDeleteTextures(1, @FTexture);
end;
//------------------------------------------------------------------------------
procedure TGLTexture.DeleteTexture(var Texture: glUint);
begin
  IF Texture <> 0 then begin
    glDeleteTextures(1, @Texture);
    Texture:=0;
  end;
end;

//------------------------------------------------------------------------------
procedure TGLTexture.LoadTexture(FileName: String);
var FileExt: String;
begin
  FileExt:=Uppercase(ExtractFileExt(FileName));
  // Windows Bitmap
  if (FileExt = '.BMP') and FileExists(FileName) then begin
    FBitmap.LoadFromFile(FileName);
    FGraphicType:=gtBitmap;
  end else
  // JPEG
  if (FileExt = '.JPG') and FileExists(FileName)  then begin
    FJpeg.LoadFromFile(FileName);
    FGraphicType:=gtJpeg;
  end else
  // Truevision Targa
  if (FileExt = '.TGA') and FileExists(FileName)  then begin
    FTarga.LoadFromFile(FileName);
    FGraphicType:=gtTarga;
  end else
  // Portable Network Graphic
  if (FileExt = '.PNG') and FileExists(FileName)  then begin
    FPng.LoadFromFile(FileName);
    FGraphicType:=gtPng;
  end else
  // Unsupported image or file not found
  begin
    FGraphicType:=gtNone;
    raise Exception.Create('Couldn''t load texture:'+#13+FileName);
  end;
//  BuildTexture;
end;



//------------------------------------------------------------------------------
procedure TGLTexture.Assign(Source: TPersistent);
begin
  IF Source is TGLTexture then begin
    FGraphicType     :=TGLTexture(Source).FGraphicType;
    FDataFormat      :=TGLTexture(Source).FDataFormat;
    FData24          :=TGLTexture(Source).FData24;
    FData32          :=TGLTexture(Source).FData32;
    FTransparent     :=TGLTexture(Source).FTransparent;
    FTransparentColor:=TGLTexture(Source).FTransparentColor;
    FTexture         :=TGLTexture(Source).FTexture;
    Case FGraphicType of
      gtBitmap: FBitmap.Assign(TGLTexture(Source).FBitmap);
      gtJpeg  : FJpeg  .Assign(TGLTexture(Source).FJpeg);
      gtTarga : FTarga .Assign(TGLTexture(Source).FTarga);
      gtPng   : FPng   .Assign(TGLTexture(Source).FPng);
    end;
 end else
   inherited;
end;

//------------------------------------------------------------------------------
procedure TGLTexture.SetBitmap(const Value: TBitmap);
begin
  FBitmap.Assign(Value);
end;

//------------------------------------------------------------------------------
procedure TGLTexture.SetJpeg(const Value: TJPEGImage);
begin
  FJpeg.Assign(Value);
end;

//------------------------------------------------------------------------------
procedure TGLTexture.SetTarga(const Value: TTargaGraphic);
begin
  FTarga.Assign(Value);
end;

//------------------------------------------------------------------------------
procedure TGLTexture.SetPng(const Value: TPNGGraphic);
begin
  FPng.Assign(Value);
end;

//------------------------------------------------------------------------------
procedure TGLTexture.Bind;
begin
  glBindTexture(GL_TEXTURE_2D, FTexture);
end;

//------------------------------------------------------------------------------
function TGLTexture.GetHeight: Integer;
begin
  Case FGraphicType of
    gtBitmap: Result:=FBitmap.Height;
    gtJpeg  : Result:=FJpeg  .Height;
    gtTarga : Result:=FTarga .Height;
    gtPng   : Result:=FPng   .Height;
    else      Result:=-1;
  end;
end;

//------------------------------------------------------------------------------
function TGLTexture.GetWidth: Integer;
begin
  Case FGraphicType of
    gtBitmap: Result:=FBitmap.Width;
    gtJpeg  : Result:=FJpeg  .Width;
    gtTarga : Result:=FTarga .Width;
    gtPng   : Result:=FPng   .Width;
    else      Result:=-1;
  end;
end;



//------------------------------------------------------------------------------
function TGLTexture.GetPixels(X, Y: Integer): TColor;
var Color3: TColor3b;
var Color4: TColor4b;
begin
  Result:=clNone;
  if FDataFormat = df24 then begin
    Color3:=FData24[X + Y * Width];
    Result:=RGB(Color3.Red, Color3.Green, Color3.Blue);
  end;
  if FDataFormat = df32 then begin
    Color4:=FData32[X + Y * Width];

    Result:=RGBA(Color4.Red, Color4.Green, Color4.Blue, Color4.Alpha);
  end;
end;

//------------------------------------------------------------------------------
procedure TGLTexture.SetPixels(X, Y: Integer; const Value: TColor);
var Color3: TColor3b;
var Color4: TColor4b;
begin
  if FDataFormat = df24 then begin
    Color3.Red  := GetRValue(Value);
    Color3.Green:= GetGValue(Value);
    Color3.Blue := GetBValue(Value);

    FData24[X + Y * Width]:=Color3;
  end;
  if FDataFormat = df32 then begin
    Color4.Red  := GetRValue(Value);
    Color4.Green:= GetGValue(Value);
    Color4.Blue := GetBValue(Value);
    Color4.Alpha:= GetAValue(Value);

    FData32[X + Y * Width]:=Color4;
  end;
end;

//------------------------------------------------------------------------------
procedure TGLTexture.SetTransparent(const Value: Boolean);
begin
  FTransparent:= Value;
  SetTransparency;
end;


end.
