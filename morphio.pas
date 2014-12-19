
Unit morphio;

Interface
uses SysUtils;

const
  {$IFDEF MSWINDOWS}
  PathSeparator='\';
  {$ELSE}
  PathSeparator='/';
  {$ENDIF}

  {$IFDEF MSWINDOWS}
  CrLf=#13#10;
  {$ELSE}
  CrLf=#10;
  {$ENDIF}

Const
 // Stream access/permission flags
  smRead    = 1;
  smWrite   = 2;
  smDynamic = 4;
  smShared  = 8;
  smAppend  = 16;
  smDefault = 7; //lfRead Or lfWrite or lfDynamic

Type

  FilePointer=File;

  LStreamParseMethod=Function(Var Buffer;Size:Integer):Integer Of Object;
  LStreamStringParseMethod=Procedure(Var S:String) Of Object;
  LStreamParseMode=(spRead,spWrite);
  
  LStreamByteOrder=(sbLittleEndian, sbBigEndian);

  LStream=Class
     Protected
      _Offset:Integer;
      _Pos:Integer;
      _Mode:Integer;
      _Order:LStreamByteOrder;
      _Size:Integer;
      _Name:String;
      b3d_stack:array[0..100]of integer;
       b3d_tos:Integer;

      Procedure _WriteString(Var S:String);
      Function GetEOF:Boolean;Virtual;

     Public
      Parse:LStreamParseMethod;
      ParseString:LStreamStringParseMethod;
      ParseMode:LStreamParseMode;

      Procedure WriteTag(S:String);

      procedure b3dBeginChunk(tag:string);
      procedure b3dEndChunk();

      Constructor Create(StreamMode:Integer=smDefault);
      Destructor Destroy;Reintroduce;Virtual;Abstract;

      Function Read(Var Buffer; Length:Integer):Integer;Virtual;Abstract;
      Function Write(Var Buffer; Length:Integer):Integer;Virtual;Abstract;

      Procedure EndianSwapWord(Var N:Word);
      Procedure EndianSwapLong(Var N:Cardinal);



      Procedure ReadString(Var S:String);Virtual;
      Procedure ReadLine(Var S:String);Virtual;
      Procedure WriteString(S:String);Virtual;
      Procedure WriteLine(S:String='');Virtual;

      Procedure Copy(Dest:LStream);Overload;
      Procedure Copy(Dest:LStream;Offset,Count:Integer);Overload;
      Procedure CopyText(Dest:LStream);

      Procedure SetParseMode(Mode:LStreamParseMode);

      Procedure Seek(NewPosition:Integer);Virtual;
      Procedure Skip(Size:Integer);Virtual;
      Procedure Truncate;Virtual;

      function writeInt(v: integer) : Integer;
      function writefloat(v: single) : Integer;
      function WriteByte( value: integer) : integer;
      function writeShort(v: byte) : integer;
      function writeChar(v: integer) : integer;

      function readint() : Integer;
      function readfloat() : single;
         function ReadChar():string;



      Property Position:Integer Read _Pos Write Seek;
      Property Size:Integer Read _Size;
      Property Mode:Integer Read _Mode;
      Property ByteOrder:LStreamByteOrder Read _Order Write _Order;

      Property EOF:Boolean Read GetEOF;

      Property Name:String Read _Name Write _Name;
     End;

  LFileStream=Class(LStream)
     Protected
        _File:FilePointer;
        _Open:Boolean;

     Public

        Constructor Create(FileName:String; StreamMode:Integer=smDefault);Overload;
        Constructor Open(FileName:String; StreamMode:Integer=smDefault; Offset:Integer=0; MaxSize:Integer=-1);
        Destructor Destroy;Override;
        Destructor Delete;
        Procedure Rename(NewName:String);
        Procedure Truncate;Override;
        Function Read(Var Buffer; Length:Integer):Integer;Override;
        Function Write(Var Buffer; Length:Integer):Integer;Override;
        Procedure Seek(NewPosition:Integer);Override;
     End;

  LMemoryStream=Class(LStream)
     Protected
      _Buffer:Pointer;

     Public
      Constructor Create(BufferSize:Integer; StreamMode:Integer=smDefault);Overload;
      Constructor Create(BufferSize:Integer;Buffer:Pointer; StreamMode:Integer=smDefault);Overload;
      Constructor Open(FileName:String; StreamMode:Integer=smDefault);Overload;
      Destructor Destroy;Override;
      Procedure SetBuffer(BufferSize:Integer;Buffer:Pointer);
      Function Read(Var Buffer; Length:Integer):Integer;Override;
      Function Write(Var Buffer; Length:Integer):Integer;Override;
      function ReadBuffer(var Buffer; Count: Longint): Longint;
      Procedure Truncate;Override;
      Procedure Seek(NewPosition:Integer);Override;

      procedure LoadFromStream(Stream: LStream);

    function ReadInt8(Var value: Byte) : boolean;
		function ReadInt16(Var value: Word) : boolean; overload;
    function ReadInt16(Var value: SmallInt) : boolean; overload;
		function ReadInt32(Var value: Longword) : boolean; overload;
    function ReadInt32(Var value: Longint) : boolean; overload;


      Property Buffer:Pointer Read _Buffer;
     End;


     
Type
  XMLTagType=(xmlBeginTag,xmlEndTag,xmlData);
  XMLStatus=(xmlWriting,xmlReading);
  XMLType=(xmlString, xmlBoolean, xmlInteger, xmlCardinal,
            xmlByte, xmlWord, xmlSingle,
            xmlVector, xmlColor, xmlTime);

  XMLDocument=Class;
  XMLNode=Class;

  XMLDescriptor=Object
    Name:String;
    Address:Pointer;
    ElementCount:Integer;
    XMLType:XMLType;
    Default:String;
    Found:Boolean;

    Procedure Read(Node:XMLNode);
    Procedure Write(Document:XMLDocument; Node:XMLNode);
  End;

  XMLElement = Class
    Protected
      _Descriptors:Array Of XMLDescriptor;
      _DescriptorCount:Integer;
      _Status:XMLStatus;

      Procedure XMLRegisterElement(Name:String; Address:Pointer; XMLType:XMLType; Default:String='');
      Procedure XMLRegisterArrayElement(Name:String; Address:Pointer; XMLType:XMLType; Size:Integer);

      Procedure XMLLoadElements(Source:XMLNode);Overload;
      Procedure XMLSaveElements(Document:XMLDocument; Parent:XMLNode=Nil);

    Public
      Procedure XMLRegisterStructure; Virtual;
      Procedure XMLClearStructure;
      Procedure XMLSynchronize; Virtual;

      Procedure XMLLoad(Node:XMLNode);Overload;

      Procedure XMLLoad(Document:XMLDocument);Overload;
      Procedure XMLSave(Document:XMLDocument);Overload;

      Procedure XMLLoad(Source:LStream);Overload;
      Procedure XMLSave(Dest:LStream);Overload;

      Procedure XMLLoad(FileName:String);Overload;
      Procedure XMLSave(FileName:String);Overload;

      Function XMLGetPropertyCount:Integer;
      Function XMLGetProperty(Index:Integer):XMLDescriptor; Overload;
      Function XMLGetProperty(Name:String):XMLDescriptor; Overload;

      Function XMLNewElement(Name:String):XMLElement;Virtual;
      Function XMLGetElement(Index:Integer):XMLElement;Virtual;
      Function XMLGetElementCount():Integer;Virtual;

      Property XMLStatus:XMLStatus Read _Status;
  End;

  XMLNode = Class
    Protected
      _Name:String;
      _Value:String;
      _Childs:Array Of XMLNode;
      _ChildCount:Integer;
      _Parent:XMLNode;

      Procedure Save(Dest:LStream);
      Function Read(Source:LStream):String;
      Function GetTagType(S:String):XMLTagType;
      Function GetTagName(S:String):String;
      Function GetPath:String;
      Function GetParentCount:Integer;

    Public

      Constructor Create(Name:String; Value:String = ''); Overload;
      Constructor Create(Source:LStream);Overload;

      Destructor Destroy;Reintroduce;

      Function AddTag(Name,Value:String):XMLNode;
      Procedure AddNode(Node:XMLNode);

      Function GetNode(Name:String):XMLNode;
      Function GetChild(Index:Integer):XMLNode;

      Property Name:String Read _Name;
      Property Value:String Read _Value Write _Value;

      Property ChildCount:Integer Read _ChildCount;
  End;

  XMLDocument = Class
    Protected
      _Root:XMLNode;

    Public
      Destructor Destroy;Reintroduce;

      Procedure Load(Source:LStream);Overload;
      Procedure Save(Dest:LStream);Overload;

      Procedure Load(FileName:String);Overload;
      Procedure Save(FileName:String);Overload;

      Procedure AddNode(Node:XMLNode; Parent:XMLNode=Nil);
      Function GetNode(Name:String):XMLNode;

      Function AddString(Name:String; Value:String=''; Parent:XMLNode=Nil):XMLNode;
      Function AddBoolean(Name:String; Value:Boolean; Parent:XMLNode=Nil):XMLNode;
      Function AddInteger(Name:String; Value:Integer; Parent:XMLNode=Nil):XMLNode;
      Function AddCardinal(Name:String; Value:Cardinal; Parent:XMLNode=Nil):XMLNode;
      Function AddSingle(Name:String; Value:Single; Parent:XMLNode=Nil):XMLNode;

      Property Root:XMLNode Read _Root;
  End;

     
{$R-}

Const
// LZSS Parameters
  _N=4096; // Size of string buffer
  _F=60;   // Size of look-ahead buffer
  THRESHOLD=2;
  _NULL=_N; // End of tree's node

// Huffman coding parameters
  N_CHAR=(256-THRESHOLD+_F);
                           // character code (:= 0..N_CHAR-1)
  _T=(N_CHAR * 2 - 1); // Size of table
  _R=(_T - 1); // root position
  MAX_FREQ=$8000; //update when cumulative frequency reaches to this value

Type
  //TCompressedStream private types
  Freqtype=Array[0.._T] Of Word;
  FreqPtr=^FreqType;
  PntrType=Array[0..PRED(_T + N_Char)] Of SmallInt;
  PntrPtr=^PntrType;
  SonType=Array[0..PRED(_T)] Of SmallInt;
  SonPtr=^SonType;


  TextBufType=Array[0.._N + _F - 2] Of Byte;
  TBufPtr=^TextBufType;
  WordRay=Array[0.._N] Of SmallInt;
  WordRayPtr=^WordRay;
  BWordRay=Array[0.._N + 256] Of SmallInt;
  BWordRayPtr=^BWordRay;

  
  LStreamCompressor=Class
     Private
      Code,Len:Word;
      GetBuf:Word;
      GetLen:Byte;
      PutLen:Byte;
      PutBuf:Word;
      TextSize:Longint;
      CodeSize:Longint;
      PrintCount:Longint;
      Match_Position:SmallInt;
      Match_Length:SmallInt;
      Text_Buf:TBufPtr;
      Lson,Dad:WordRayPtr;
      Rson:BWordRayPtr;
      Freq:FreqPtr; // cumulative freq table
      Prnt:PntrPtr;
      Son:SonPtr;  // pointing children nodes (son[], son[] + 1)
      StreamSize:Integer;
      SourceTarget:LStream;
      DestTarget:LStream;
      Procedure InitTree;
      Procedure InsertNode(R:SmallInt);
      Procedure DeleteNode(P:SmallInt);
      Procedure GetBytes(Var Data;Count:Word;Var ReadCount:Word);
      Procedure PutBytes(Var Data;Count:Word;Var WriteCount:Word);
      Procedure Update(C:SmallInt);
      Procedure StartHuff;
      Procedure Putcode(L:SmallInt;C:Word);
      Procedure Reconst;
      Procedure EncodeChar(C:Word);
      Procedure EncodePosition(C:Word);
      Procedure EncodeEnd;
      Function GetBit:SmallInt;
      Function GetByte:SmallInt;
      Function DecodeChar:SmallInt;
      Function DecodePosition:Word;
     Public
      Constructor Create;
      Destructor Destroy;Override;

      Function Compress(Source,Dest:LStream; PadSize:Integer=4):Integer;
      Function Decompress(Source,Dest:LStream; PadSize:Integer=4):Integer;

      Function GetDecompressedSize(Source:LStream; PadSize:Integer=4):Integer;

    End;

type
    LObjectType=Array[1..4]Of Char;

Const
  leafHeader:LObjectType='SDLK';


  resFile:LObjectType=           'FILE'; // User defined file format
  resSDLKPackage:LObjectType=    'PACK'; // LEAF package



  lfcAutoLoad=1;     //Resource should be autoloaded at start
  lfcCompressed=2;   //Resource is compressed

Type
  LPackageVersion=Packed Record
          Case Byte Of
          0:(Value:Word);
          1:(Major,Minor:Byte);
          End;

  PResourceInfo=^LResourceInfo;
  LResourceInfo=Record
      Tag:LObjectType; // Resource type tag
      Name:String;     // Resource name
      FileName:String; // Resource filename
      Offset:Longint;  // Offset of the resource
      Size:Longint;    // Size of the resource (bytes)
      Flags:Byte;      // Flags-Not used yet
     End;

  LResourceTable=Array Of LResourceInfo;

  LPackage=Class
     Private
      _Offset:Cardinal;      // Position of the package within the file
      _DataOffset:Cardinal;  // Header size, files data starts here
      _TableOffset:Cardinal; // Table position in the file
      _Version:LPackageVersion;

      _Size:Longint;     // Size of the package, may differ from the file size
      _FileName:String;  // File containing the package
      _Name:String;      // Package name
      _Table:LResourceTable; // List of all resources within the file
      _TableSize:Integer;    // Number of resources in the table

      _Loader:LMemoryStream;

      // Writes the package header to the file
      Procedure RebuildHeader(Dest:LStream);
      // Rebuild internal file table
      Procedure RebuildFileTable(Dest:LStream);


      Function GetNewOffset(Size:Integer):Integer;

     Public
      Path:String;  // File override path


       Constructor Create();overload;

      Constructor Create(FileName,PackageName:String); overload;// Creates a new empty package

      function   OpenAsset(FileName:String):boolean;
      Constructor OpenMemory(str:pointer;size:integer);

      destructor Destroy();

      procedure  ListFiles();

      function GetFile(name:string;var buffer:pointer;var buffersize:integer):boolean;overload;
      function GetFile(name:string;Var Dest:LStream):boolean;overload;

      Function FindResource(ResourceName:String):PResourceInfo;
      Function GetResource(ResourceName:String):PResourceInfo;

      // Loads a resource into a stream
      // Note: If resource file is found in search path the is loaded from there
      // This can be used for patches/mods
      Procedure LoadResource(Resource:PResourceInfo;Var Dest:LStream);

      // Loads a resource into a stream
      // Unlike resource, this always gets the resource from the package
      Procedure ExtractResource(Resource:PResourceInfo;Var Dest:LStream);

      // Get file stream containing the resource
      Function OpenResource(Resource:PResourceInfo):LFileStream;

      //Adds a resource to the package
      Function AddResource(ResourceName:String;ResourceType:LObjectType;Resource:LStream;ResourceFlags:Byte=0):PResourceInfo;
      Function AddFile(FileName:String):PResourceInfo;
      Function Add(FileName:String):boolean;


      Procedure DeleteResource(Resource:PResourceInfo);





      // Package name
      Property Name:String Read _Name;
      Property FileName:String Read _FileName;
      Property ItemCount:Integer Read _TableSize;
      Property Size:Integer Read _Size;
      Property Version:LPackageVersion Read _Version;
    End;


    
Function IntMax(Const A,B:Integer):Integer;
Function IntMin(Const A,B:Integer):Integer;
Function LongMax(Const A,B:Cardinal):Cardinal;
Function LongMin(Const A,B:Cardinal):Cardinal;

Function GetNextLine(Var S:String):String;
Function GetNextWord(Var S:String; Separator:Char=' '):String;
Function GetNextArg(Var S:String):String;
Function GetNextToken(Var S:String; Separator:Char=','; Op:Char='(';Ed:Char=')'):String;
Function FindCloseBracket(Const S:String; Op:Char='(';Ed:Char=')'):Integer;

Function TrimLeft(Const S:String):String;
Function TrimRight(Const S:String):String;

Function UpStr(Const S:String):String;
Function LowStr(Const S:String):String;
Function CapStr(Const S:String):String;

Function StrLPad(S:String;N:Integer; Token:Char='0'):String;
Function StrRPad(S:String;N:Integer; Token:Char='0'):String;
Function StrClean(Const S:String):String;

Function IntToString(Const N:Integer):String;
Function LongToString(Const N:Cardinal):String;Overload;
Function FloatToString(Const N:Single):String;
Function BoolToString(Const N:Boolean):String;Overload;
Function TicksToString(Const N:Cardinal):String;Overload;
Function MemoryToString(Const N:Cardinal):String;

Function HexStr(Const Value:Byte):String;Overload;
Function HexStr(Const Value:Word):String;Overload;
Function HexStr(Const Value:Cardinal):String;Overload;

Function StringToBase64(Buf:String):String;
Function Base64ToString(B64:String):String;

Procedure ReplaceText(Const Token,Value:String; Var S:String);
Procedure ReplaceAllText(Const Token,Value:String; Var S:String);

Function PosRev(Const SubStr,Str:String):Integer;

Function UnicodeChar(Code:Word):String;

Function StringToInt(Const S:String):Integer;
Function StringToLong(Const S:String):Cardinal;
Function StringToBool(Const S:String):Boolean;
Function StringToFloat(Const S:String):Single;

Function GetFilePath(FileName:String):String;
Function GetLastFilePath(FileName:String):String;
Function GetFirstFilePath(FileName:String):String;
Function GetFileName(FileName:String;Const RemoveExt:Boolean):String;
Function GetFileExt(FileName:String):String;


//Function GetCRC32(Source:LStream):Cardinal;overload;
//Function GetCRC32(Source:LStream; Start,Length:Integer):Cardinal;overload;


function  FileExists(Const FileName:String):Boolean;


Function OpenFileStream(FileName:String; StreamMode:Integer=smDefault):LStream;


Implementation

Function OpenFileStream(FileName:String; StreamMode:Integer=smDefault):LStream;
Begin
  Result:=LFileStream.Open(FileName, StreamMode);
End;

Function FileExists(Const FileName:String): Boolean;
Begin
  Result:=(FileAge(FileName)<>-1);
End;


Procedure RaiseError(Const Desc:String);
Begin
writeln(Desc);
End;


Procedure FileSeek(Var F:FilePointer; Offset:Integer);
Begin
  System.Seek(F, Offset);
End;

Procedure FileTruncate(Var F:FilePointer);
Begin
  System.Truncate(F);
End;

Procedure FileRename(Var F:FilePointer; Name:String);
Begin
  System.Rename(F, Name);
End;

Function DefaultFileExists(Const FileName:String): Boolean;
Begin
  Result:=(FileAge(FileName)<>-1);
End;


Var
  SystemEndian:LStreamByteOrder;


Function ByteSwap32(Const A:Cardinal):Cardinal;
Var
  B1,B2,B3,B4:Byte;
Begin
  B1:=A And 255;
  B2:=(A Shr 8) And 255;
  B3:=(A Shr 16)And 255;
  B4:=(A Shr 24)And 255;

  Result:=(Cardinal(B1)Shl 24) + (Cardinal(B2) Shl 16) + (Cardinal(B3) Shl 8) + B4;
End;

Function ByteSwap16(A:Word):Word;
Var
  B1,B2:Byte;
Begin
  B1:=A And 255;
  B2:=(A Shr 8) And 255;

  Result:=(B1 Shl 8)+B2;
End;

//{$ENDIF}

{*****************
  TStream Object
 *****************}
Constructor LStream.Create(StreamMode:Integer=smDefault);
Begin
  _Name:='';
  _Mode:=StreamMode;
  _Order:=SystemEndian;
  _Pos:=0;
  _Offset:=0;
  b3d_tos:=0;
End;

procedure LStream.b3dBeginChunk(tag:string);
begin
    Inc(b3d_tos);
    WriteTag(tag);
    writeInt(0);
    b3d_stack[b3d_tos]:=Position;

end;
procedure LStream.b3dEndChunk();
var
  n:Integer;
begin
   n:=Position;
   Seek(b3d_stack[b3d_tos]-4);
   writeInt(n-b3d_stack[b3d_tos]);
   Seek(n);
   b3d_tos:=b3d_tos-1;

end;

Procedure LStream.EndianSwapWord(Var N:Word);
Begin
  If ByteOrder=SystemEndian Then
    Exit;
  N:=ByteSwap16(N);
End;

Procedure LStream.EndianSwapLong(Var N:Cardinal);
Begin
  If ByteOrder=SystemEndian Then
    Exit;
  N:=ByteSwap32(N);
End;

Procedure LStream.SetParseMode(Mode:LStreamParseMode);
Begin
  ParseMode:=Mode;
  Case Mode Of
  spRead: Begin
            Parse:=Read;
            ParseString:=ReadString;
          End;
  spWrite:Begin
            Parse:=Write;
            ParseString:=_WriteString;
          End;
  End;
End;
Function IntMax(Const A,B:Integer):Integer;
Begin
  If A>B Then Result:=A Else Result:=B;
End;

Function IntMin(Const A,B:Integer):Integer;
Begin
  If A<B Then Result:=A Else Result:=B;
End;

Procedure LStream.Copy(Dest:LStream);
Var
 Count,BytesRead:Integer;
 Buffer:Pointer;
 BufferSize:Integer;
 BlockSize:Integer;
 A,B:Integer;
Begin
  If (Self.ByteOrder<>Dest.ByteOrder) Then
   writeln('IO','Destination byte order differs from source.');

  Seek(0);
  Count:=Self.Size;
  If (Dest.Size-Dest.Position<Count)And(Dest.Mode And smDynamic=0) Then
    Count:=Dest.Size-Dest.Position;

  BufferSize:=65534;
  If Count<BufferSize Then
    BufferSize:=Count;
  GetMem(Buffer,BufferSize);

  BytesRead:=0;
  While BytesRead<Count Do
  Begin
    A:=Self.Size-Self.Position;
    B:=Dest.Size-Dest.Position;
    If Dest.Mode And smDynamic<>0 Then
      B:=A;

    BlockSize:=IntMin(IntMin(BufferSize,Count-BytesRead), IntMin(A,B));
    Read(Buffer^,BlockSize);

    Dest.Write(Buffer^,BlockSize);
    Inc(BytesRead,BlockSize);
  End;

  FreeMem(Buffer,BufferSize);
End;

Procedure LStream.Copy(Dest:LStream;Offset,Count:Integer);
Var
  BytesRead:Integer;
  Buffer:Pointer;
  BufferSize:Integer;
  BlockSize:Integer;
  A,B:Integer;
Begin
  If (Self.ByteOrder<>Dest.ByteOrder) Then
   writeln('IO','Destination byte order differs from source.');

  Seek(Offset);
  If (Dest.Size-Dest.Position<Count)And(Dest.Mode And smDynamic=0) Then
    Count:=Dest.Size-Dest.Position;

  BufferSize:=65534;
  If Count<BufferSize Then
    BufferSize:=Count;
  GetMem(Buffer,BufferSize);

  BytesRead:=0;
  While BytesRead<Count Do
  Begin
    A:=Self.Size-Self.Position;

    If A=0 Then
    Begin
  writeln('Buffer too small.');
      Exit;
    End;

    B:=Dest.Size-Dest.Position;
    If Dest.Mode And smDynamic<>0 Then
      B:=A;

    BlockSize:=IntMin(IntMin(BufferSize,Count-BytesRead), IntMin(A,B));
    Read(Buffer^,BlockSize);

    Dest.Write(Buffer^,BlockSize);
    Inc(BytesRead,BlockSize);
  End;
  FreeMem(Buffer,BufferSize);
End;

Procedure LStream.CopyText(Dest:LStream);
Var
  C:Char;
  S:String;
Begin
  S:='';
  While Self.Position<Self.Size Do
  Begin
    Read(C,1);
    If (C=#10) Then
      Dest.WriteString(S)
    Else
    S:=S+C;
  End;
End;

Procedure LStream.Seek(NewPosition:Integer);
Begin
  _Pos:=NewPosition;
End;

Procedure LStream.Skip(Size:Integer);
Begin
  If Size=0 Then
    Exit;

  Seek(_Pos+Size);
End;

Procedure LStream.Truncate;
Begin
  writeln('IO','Method not supported in this stream.');
End;

Procedure LStream.ReadString(Var S:String);
Var
  Len:Word;
  N:Byte;
Begin



  Read(N,1);
  If N=255 Then
    Read(Len,2)
  Else
    Len:=N;

  SetLength(S,Len);
  If Len>0 Then
    Read(S[1],Len);
    
End;

function LStream.WriteByte( value: integer) : integer;
begin
   write(value, 1);
   Result:=1;
end;
function LStream.writeShort( v: byte) : integer;
begin
  WriteByte( (v shr 8) and $ff);
  WriteByte( (v shr 0) and $ff);
  Result:=2;
end;

function LStream.writeChar( v: integer) : integer;
begin
  WriteByte( (v shr 8) and $ff);
  WriteByte( (v shr 0) and $ff);
  Result:=4;
end;  {
function LStream.writeInt( v: integer) : Integer;
begin
  WriteByte( (v shr 24) and $ff);
  WriteByte( (v shr 16) and $ff);
  WriteByte( (v shr 8) and $ff);
  WriteByte( (v shr 0) and $ff);
  Result:=8;
end;
      }

function LStream.ReadChar():string;
var
  i:Integer;
begin
  i:=readint();
  SetLength(Result,i);

  read(PChar(result)^,i);
end;

Procedure LStream.WriteString(S:String);
Var
  N:Byte;

   i:Integer;
   len: cardinal;
   oString: UTF8String;
begin
   {
    len:=Length(s);
    Writeln(len);
    WriteInt(len);
    for i:=1 to  len do
    write(s[i], 1);
      }
  i:=Length(s);
  writeInt(i);
  Write(PChar(s)^,i);
  

  {
  Len:=Length(S);
  If Len<255 Then
    N:=Len
  Else
    N:=255;
  Write(N,1);

  If Len>=255 Then
    Write(Len,2);

  If Len>0 Then
    Write(S[1],Len);}
End;

Procedure LStream.WriteTag(S:String);
Var
   len: cardinal;

begin

   Len:=Length(S);
   Write(PChar(s)^,len);
End;

Procedure LStream._WriteString(Var S:String);
Begin
  WriteString(S);
End;

Procedure LStream.WriteLine(S:String);
Begin
  S:=S+#13#10;
  Write(S[1],Length(S));
End;

function LStream.writeInt(v: integer) : Integer;
begin
   write(v,4);
end;
function LStream.readint() : Integer;
begin
  read(result,4);
end;
function LStream.readfloat() : single;
begin
  read(result,4);
end;
function LStream.writefloat(v: single) : Integer;
begin
   write(v,4);
end;
Procedure LStream.ReadLine(Var S:String);
Var
  C:Char;
Begin
  S:='';
  C:=#0;
  While (C<>#10)And(Position<Size) Do
  Begin
    Read(C,1);
    If (C<>#10)Or(C<>#13) Then
    S:=S+C;
  End;
  S:=TrimRight(S);
End;

Function LStream.GetEOF:Boolean;
Begin
  Result:=Position>=Size;
End;

{**************************
   TFileStream Object
 **************************}

Constructor LFileStream.Create(FileName:String; StreamMode:Integer=smDefault);
Begin
  Inherited Create(StreamMode);



//  writeln('IO','Opening '+FileName);
  If StreamMode=0 Then
    writeln('Invalid file mode.['+FileName+']')
  Else
  Begin
    _Name:=FileName;
    FileMode:=2;

    AssignFile(_File,_Name);
    Rewrite(_File,1);

    _Size:=0;
    _Open:=True;
  End;
End;

Constructor LFileStream.Open(FileName:String; StreamMode:Integer=smDefault; Offset:Integer=0; MaxSize:Integer=-1);
Begin
  Inherited Create(StreamMode);



  _Open:=False;
  _Name:=FileName;



  If StreamMode=0 Then
  Begin
    writeln('Invalid file mode. ['+FileName+']');
    Exit;
  End Else
  Begin
    _Offset:=Offset;

    AssignFile(_File,_Name);
    Reset(_File,1);
    _Size:=FileSize(_File);

    If (MaxSize>0) And (MaxSize<_Size) Then
      _Size:=MaxSize;

    If _Offset>0 Then
      FileSeek(_File,_Offset);

    _Open:=True;
  End;
End;

Destructor LFileStream.Destroy;
Begin
  If Not _Open Then
    Exit;
  CloseFile(_File);
End;

Destructor LFileStream.Delete;
Begin
  If Not _Open Then
    Exit;

  CloseFile(_File);
  Erase(_File);
End;

Procedure LFileStream.Truncate;
Begin
  If Not _Open Then
    Exit;

  FileTruncate(_File);
  _Size:=_Pos;
End;

Procedure LFileStream.Rename(NewName:String);
Begin
  If Not _Open Then
    Exit;

  _Name:=NewName;

  CloseFile(_File);
  Erase(_File);
  FileRename(_File,_Name);
  AssignFile(_File,_Name);
  Reset(_File,1);

  Seek(Position);
End;

Function LFileStream.Read(Var Buffer; Length:Integer):Integer;
Begin
  If (Length=0)Or(Not _Open) Then
  Begin
    Result:=0;
    Exit;
  End;

  If (_Mode And smRead=0)Or(_Pos>=_Size) Then
  Begin
    Result:=0;
    writeln('Cannot read from file. ['+_Name+']');
    Exit;
  End;

  If (_Pos+Length>_Size)Then
    Length:=_Size-_Pos;

  BlockRead(_File,Buffer, Length);
  Inc(_Pos, Length);
  Result:=Length;

  Case Length Of
  2:EndianSwapWord(Word(Buffer));
  4:EndianSwapLong(Cardinal(Buffer));
   End;
End;

Function LFileStream.Write(Var Buffer; Length:Integer):Integer;
Begin
  Result:=0;
  If (Not _Open) Then
    Exit;

  If (_Mode And smWrite=0)Then
  Begin
    writeln('File is write protected.['+_Name+']');
    Exit;
  End;

  If (_Pos>=_Size)And(_Mode And smDynamic=0) Then
  Begin
    writeln('Cannot write to file.['+_Name+']');
    Exit;
  End;

  // Swap the bytes to proper order
  If (ByteOrder<>SystemEndian) Then
  Begin
    Case Length Of
    2:EndianSwapWord(Word(Buffer));
    4:EndianSwapLong(Cardinal(Buffer));
    {Else
      If (Length>4) Then
        RaiseError(ltWarning,'IO','FileStream.Write','Possible data corruption. Endian doesn''''t match.');}
    End;
  End;

  BlockWrite(_File,Buffer,Length);
  Inc(_Pos, Length);
  If _Pos>_Size Then
    _Size:=_Pos;

  // Restore the bytes order
  If (ByteOrder<>SystemEndian) Then
  Begin
    Case Length Of
    2:EndianSwapWord(Word(Buffer));
    4:EndianSwapLong(Cardinal(Buffer));
    End;
  End;

  Result:=Length;
End;

Procedure LFileStream.Seek(NewPosition:Integer);
Begin
  If _Pos>_Size Then
  Begin
    writeln('Cannot seek in file.['+_Name+']');
    Exit;
  End;
  _Pos:=NewPosition;

  FileSeek(_File, _Pos+_Offset);
End;

{***************************
  TMemoryStream object
 ***************************}

Constructor LMemoryStream.Create(BufferSize:Integer; StreamMode:Integer=smDefault);
Begin
  //RaiseError('IO','MemoryStream.Create','Allocating '+MemStr(Size));

  If (StreamMode And smDynamic<>0) Then
    StreamMode:=StreamMode Xor smDynamic;

  If (StreamMode And smShared<>0) Then
    StreamMode:=StreamMode Xor smShared;

  Inherited Create(StreamMode);
  _Size:=BufferSize;

  GetMem(_Buffer,_Size);
  _Pos:=0;
End;

Constructor LMemoryStream.Create(BufferSize:Integer; Buffer:Pointer; StreamMode:Integer=smDefault);
Begin
  If (StreamMode And smShared=0) Then
    StreamMode:=StreamMode Or smShared;

  Inherited Create(StreamMode);
  _Size:=BufferSize;
  _Buffer:=Buffer;
  _Pos:=0;
End;

Constructor LMemoryStream.Open(FileName:String; StreamMode:Integer=smDefault);
Var
  F:FilePointer;
Begin
  Inherited Create(StreamMode);

  AssignFile(F,FileName);
  Reset(F,1);
  _Size:=FileSize(F);
  Create(_Size, StreamMode);

  BlockRead(F,_Buffer^,_Size);
  CloseFile(F);
End;

Destructor LMemoryStream.Destroy;
Begin
  If _Mode And smShared=0 Then
  Begin
    //RaiseError('IO','MemoryStream.Destroy','Releasing '+MemStr(Size));
    FreeMem(_Buffer,_Size);
  End;

  _Buffer:=Nil;
End;

{
function LMemoryStream.ReadString(Var text: String) : boolean;
var
  readedOk: boolean;
  lenChar: Word;
  character: Char;
begin
	lenChar := SizeOf(Char);
	character := Char(1);
  text := '';

	readedOk := (ReadBuffer(character, lenChar)>0);
  while((readedOk) and (character <> Char(0))) do
  begin
		text := text + character;
    readedOk := (ReadBuffer(character, lenChar)>0);
	end;

  result := readedOk;
end;
}

function LMemoryStream.ReadInt8(Var value: Byte) : boolean;
begin
  result := (ReadBuffer(value, SizeOf(Byte))>0);
end;

function LMemoryStream.ReadInt16(Var value: Word) : boolean;
begin
  result := (ReadBuffer(value, SizeOf(Word))>0);
end;

function LMemoryStream.ReadInt16(Var value: SmallInt) : boolean;
begin
  result := (ReadBuffer(value, SizeOf(SmallInt))>0);
end;

function LMemoryStream.ReadInt32(Var value: Longword) : boolean;
begin
  result := (ReadBuffer(value, SizeOf(Longword))>0);
end;

function LMemoryStream.ReadInt32(Var value: Longint) : boolean;
begin
  result := (ReadBuffer(value, SizeOf(Longint))>0);
end;

function LMemoryStream.ReadBuffer(var Buffer; Count: Longint): Longint;
begin
  if (_Pos >= 0) and (Count >= 0) then
  begin
    Result := _Size - _Pos;
    if Result > 0 then
    begin
      if Result > Count then Result := Count;
      Move(Pointer(Longint(_Buffer) + _Pos)^, Buffer, Result);
      Inc(_Pos, Result);
      Exit;
    end;
  end;
  Result := 0;
end;

Function LMemoryStream.Read(Var Buffer; Length:Integer):Integer;
Var
 P:Pointer;
Begin
  If (Length=0) Then
  Begin
    Result:=0;
    Exit;
  End;

  If Not Assigned(_Buffer) Then
  Begin
    writeln('Buffer not assigned.');
    Result:=0;
    Exit;
  End;

  If (_Pos>=_Size) Then
  Begin
    writeln('Cannot read from memory.');
    Result:=0;
    Exit;
  End;

  If (_Pos+Length>_Size)Then  Length:=_Size-_Pos;

  P:=Pointer(Integer(_Buffer)+_Pos);
  Case Length Of
    1:  Begin
          Byte(Buffer):=Byte(P^);
        End;
    2:  Begin
          Word(Buffer):=Word(P^);
          EndianSwapWord(Word(Buffer));
        End;
    4:  Begin
          Cardinal(Buffer):=Cardinal(P^);
          EndianSwapLong(Cardinal(Buffer));
        End;
    Else
        Begin
          Move(P^,Buffer,Length);
        End;
  End;

  Inc(_Pos,Length);
  Result:=Length;
End;


procedure LMemoryStream.LoadFromStream(Stream: LStream);
var
  Count: Longint;
begin
  Stream.Position := 0;
  Count := Stream.Size;
  _Size:=Count;
  if Count <> 0 then Stream.Read(buffer^, Count);
end;


Function LMemoryStream.Write(Var Buffer; Length:Integer):Integer;
Var
 P:Pointer;
Begin
  Result:=0;

  If Not Assigned(_Buffer) Then
  Begin
    writeln('Buffer not assigned.');
    Exit;
  End;

  If (_Pos>=_Size) Then
  Begin
    writeln('Cannot write to memory.');
    Exit;
  End;

  If (_Pos+Length>_Size)Then
    Length:=_Size-_Pos;

  P:=Pointer(Integer(_Buffer)+_Pos);
  Case Length Of
    1:  Begin
          Byte(P^):=Byte(Buffer);
        End;
    2:  Begin
          Word(P^):=Word(Buffer);
          EndianSwapWord(Word(P^));
        End;
    4:  Begin
          Cardinal(P^):=Cardinal(Buffer);
          EndianSwapLong(Cardinal(P^));
        End;
    Else Begin
          Move(Buffer,P^,Length);
          {If (Size>4) Then
            RaiseError(ltWarning,'IO','MemoryStream.Write','Possible data corruption. Endian doesn''''t match.');}
        End;
  End;
  Inc(_Pos, Length);
  Result:=Length;
End;


Procedure LMemoryStream.Seek(NewPosition:Integer);
Begin
  If Position>_Size Then
  Begin
    writeln('Cannot seek in memory.');
    Exit;
  End;

  _Pos:=NewPosition;
End;

Procedure LMemoryStream.Truncate;
Var
 Ptr:Pointer;
Begin
  GetMem(Ptr,_Pos);
  Move(_Buffer^,Ptr^,_Pos);
  FreeMem(_Buffer,_Size);
  _Size:=_Pos;
  _Buffer:=Ptr;
End;

Procedure LMemoryStream.SetBuffer(BufferSize:Integer;Buffer:Pointer);
Begin
  _Size:=BufferSize;
  _Buffer:=Buffer;
  _Pos:=0;
End;

Function DefaultOpenFileStream(FileName:String; StreamMode:Integer=smDefault):LStream;
Begin
  Result:=LFileStream.Open(FileName, StreamMode);
End;

Procedure DetectSystemEndian;
Var
  I:Cardinal;
  P:PByte;
Begin
  I:=1;
  P:=@I;
  If (P^=1) Then  // Lowest address contains the least significant byte
    SystemEndian:=sbLittleEndian
  Else
    SystemEndian:=sbBigEndian;
End;

Const
// Tables FOR encoding/decoding upper 6 bits of sliding dictionary pointer
// Encoder table
  P_Len:Array[0..63] Of Byte =
  ($03, $04, $04, $04, $05, $05, $05, $05,
    $05, $05, $05, $05, $06, $06, $06, $06,
    $06, $06, $06, $06, $06, $06, $06, $06,
    $07, $07, $07, $07, $07, $07, $07, $07,
    $07, $07, $07, $07, $07, $07, $07, $07,
    $07, $07, $07, $07, $07, $07, $07, $07,
    $08, $08, $08, $08, $08, $08, $08, $08,
    $08, $08, $08, $08, $08, $08, $08, $08);

  P_Code:Array[0..63] Of Byte =
  ($00, $20, $30, $40, $50, $58, $60, $68,
    $70, $78, $80, $88, $90, $94, $98, $9C,
    $A0, $A4, $A8, $AC, $B0, $B4, $B8, $BC,
    $C0, $C2, $C4, $C6, $C8, $CA, $CC, $CE,
    $D0, $D2, $D4, $D6, $D8, $DA, $DC, $DE,
    $E0, $E2, $E4, $E6, $E8, $EA, $EC, $EE,
    $F0, $F1, $F2, $F3, $F4, $F5, $F6, $F7,
    $F8, $F9, $FA, $FB, $FC, $FD, $FE, $FF);

// decoder table
  D_Code:Array[0..255] Of Byte =
  ($00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $01, $01, $01, $01, $01, $01, $01, $01,
    $01, $01, $01, $01, $01, $01, $01, $01,
    $02, $02, $02, $02, $02, $02, $02, $02,
    $02, $02, $02, $02, $02, $02, $02, $02,
    $03, $03, $03, $03, $03, $03, $03, $03,
    $03, $03, $03, $03, $03, $03, $03, $03,
    $04, $04, $04, $04, $04, $04, $04, $04,
    $05, $05, $05, $05, $05, $05, $05, $05,
    $06, $06, $06, $06, $06, $06, $06, $06,
    $07, $07, $07, $07, $07, $07, $07, $07,
    $08, $08, $08, $08, $08, $08, $08, $08,
    $09, $09, $09, $09, $09, $09, $09, $09,
    $0A, $0A, $0A, $0A, $0A, $0A, $0A, $0A,
    $0B, $0B, $0B, $0B, $0B, $0B, $0B, $0B,
    $0C, $0C, $0C, $0C, $0D, $0D, $0D, $0D,
    $0E, $0E, $0E, $0E, $0F, $0F, $0F, $0F,
    $10, $10, $10, $10, $11, $11, $11, $11,
    $12, $12, $12, $12, $13, $13, $13, $13,
    $14, $14, $14, $14, $15, $15, $15, $15,
    $16, $16, $16, $16, $17, $17, $17, $17,
    $18, $18, $19, $19, $1A, $1A, $1B, $1B,
    $1C, $1C, $1D, $1D, $1E, $1E, $1F, $1F,
    $20, $20, $21, $21, $22, $22, $23, $23,
    $24, $24, $25, $25, $26, $26, $27, $27,
    $28, $28, $29, $29, $2A, $2A, $2B, $2B,
    $2C, $2C, $2D, $2D, $2E, $2E, $2F, $2F,
    $30, $31, $32, $33, $34, $35, $36, $37,
    $38, $39, $3A, $3B, $3C, $3D, $3E, $3F);

  D_Len:Array[0..255] Of Byte =
  ($03, $03, $03, $03, $03, $03, $03, $03,
    $03, $03, $03, $03, $03, $03, $03, $03,
    $03, $03, $03, $03, $03, $03, $03, $03,
    $03, $03, $03, $03, $03, $03, $03, $03,
    $04, $04, $04, $04, $04, $04, $04, $04,
    $04, $04, $04, $04, $04, $04, $04, $04,
    $04, $04, $04, $04, $04, $04, $04, $04,
    $04, $04, $04, $04, $04, $04, $04, $04,
    $04, $04, $04, $04, $04, $04, $04, $04,
    $04, $04, $04, $04, $04, $04, $04, $04,
    $05, $05, $05, $05, $05, $05, $05, $05,
    $05, $05, $05, $05, $05, $05, $05, $05,
    $05, $05, $05, $05, $05, $05, $05, $05,
    $05, $05, $05, $05, $05, $05, $05, $05,
    $05, $05, $05, $05, $05, $05, $05, $05,
    $05, $05, $05, $05, $05, $05, $05, $05,
    $05, $05, $05, $05, $05, $05, $05, $05,
    $05, $05, $05, $05, $05, $05, $05, $05,
    $06, $06, $06, $06, $06, $06, $06, $06,
    $06, $06, $06, $06, $06, $06, $06, $06,
    $06, $06, $06, $06, $06, $06, $06, $06,
    $06, $06, $06, $06, $06, $06, $06, $06,
    $06, $06, $06, $06, $06, $06, $06, $06,
    $06, $06, $06, $06, $06, $06, $06, $06,
    $07, $07, $07, $07, $07, $07, $07, $07,
    $07, $07, $07, $07, $07, $07, $07, $07,
    $07, $07, $07, $07, $07, $07, $07, $07,
    $07, $07, $07, $07, $07, $07, $07, $07,
    $07, $07, $07, $07, $07, $07, $07, $07,
    $07, $07, $07, $07, $07, $07, $07, $07,
    $08, $08, $08, $08, $08, $08, $08, $08,
    $08, $08, $08, $08, $08, $08, $08, $08);

Constructor LStreamCompressor.Create;
Begin
  GetBuf:=0;
  GetLen:=0;
  PutLen:=0;
  PutBuf:=0;
  TextSize:=0;
  CodeSize:=0;
  PrintCount:=0;
  Match_Position:=0;
  Match_Length:=0;

  New(LSon);
  New(Dad);
  New(RSon);
  New(Text_Buf);
  New(Freq);
  New(Prnt);
  New(Son);
End;

Destructor LStreamCompressor.Destroy;
Begin
  Dispose(Son);
  Dispose(Prnt);
  Dispose(Freq);
  Dispose(Text_buf);
  Dispose(RSon);
  Dispose(Dad);
  Dispose(LSon);
End;

Procedure LStreamCompressor.GetBytes;
Begin
 If SourceTarget.Position+Count>SourceTarget.Size Then
  Count:=SourceTarget.Size-SourceTarget.Position;

 SourceTarget.Read(Data,Count);
 ReadCount:=Count;
End;

Procedure LStreamCompressor.PutBytes;
Begin
 DestTarget.Write(Data,Count);
 WriteCount:=Count;
End;

Procedure LStreamCompressor.InitTree;
Var
  I:SmallInt;
Begin
 For I:=_N + 1 To _N + 256 Do
  RSon^[i]:=_NULL; // root
 For I:=0 To _N Do
  Dad^[i]:=_NULL; // node
End;

Procedure LStreamCompressor.InsertNode;
Var
 Tmp,i,p,Cmp:SmallInt;
 Key:TBufPtr;
 C:Word;
Begin
 Cmp:=1;
 Key:=@Text_Buf^[r];
 P:=Succ(_N)+Key^[0];
 RSon^[r]:=_NULL;
 LSon^[r]:=_NULL;
 Match_Length:=0;
 While Match_Length<_F Do
  Begin
    If (Cmp>=0) Then
     Begin
      If (RSon^[p]<>_NULL)Then
       Begin
        p:=rson^[p];
       End Else
       Begin
        RSon^[p] := r;
        Dad^[r] := p;
        Exit;
       End;
     End Else
     Begin
      If (lson^[p]<>_NULL)Then
       Begin
       p:=lson^[p];
       End Else
       Begin
        lson^[p] := r;
        dad^[r] := p;
        Exit;
       End;
     End;
    i:=0;
    Cmp:=0;
    While (i < _F)And (Cmp = 0)Do
     Begin
      Inc(i);
      Cmp:=Key^[i] - text_buf^[p + i];
     End;
     If (i > THRESHOLD) Then
      Begin
       tmp:=Pred((r - p) And Pred(_N));
       If (i>Match_length)Then
        Begin
         Match_Position:=tmp;
         Match_Length:=i;
        End;
       If (Match_Length<_F)And(i=Match_length)Then
        Begin
         c:=Tmp;
         If (c<Match_Position)Then Match_Position:=C;
        End;
    End; { if i > threshold }
  End; { WHILE match_length < F }
  Dad^[r]:=Dad^[p];
  LSon^[r]:=LSon^[p];
  RSon^[r]:=RSon^[p];
  Dad^[LSon^[p]]:=r;
  Dad^[RSon^[p]]:=r;
  If(RSon^[dad^[p]] = p)Then
   Begin
    RSon^[dad^[p]]:=r;
   End Else
   Begin
    LSon^[dad^[p]]:=r;
   End;
  Dad^[p]:=_NULL; // remove p
End;

Procedure LStreamCompressor.DeleteNode;
Var
  Q:SmallInt;
Begin
 If (Dad^[p]=_NULL)Then Exit; // unregistered
 If (RSon^[p]=_NULL) Then
  Begin
   q:=LSon^[p];
  End Else
  Begin
   If (LSon^[p]=_NULL)Then
    Begin
     q:=RSon^[p];
    End Else
    Begin
     q:=LSon^[p];
     If (RSon^[q]<>_NULL)Then
      Begin
       Repeat
        q:=RSon^[q];
       Until (RSon^[q]=_NULL);
       RSon^[dad^[q]]:=LSon^[q];
       Dad^[lson^[q]] := dad^[q];
       LSon^[q] := lson^[p];
       Dad^[lson^[p]] := q;
      End;
     RSon^[q]:=RSon^[p];
     Dad^[rson^[p]]:=q;
   End;
 End;
 Dad^[q]:=Dad^[p];
 If (RSon^[Dad^[p]]=p)Then
  RSon^[dad^[p]] := q
 Else
  LSon^[dad^[p]] := q;
 Dad^[p]:=_NULL;
End;

Function LStreamCompressor.GetBit;
Var
 i:Byte;
 i2:SmallInt;
 Wresult:Word;
Begin
 While (GetLen<=8) Do
 Begin
  GetBytes(i, 1, Wresult);
  If WResult=1 Then
   i2:=i
  Else
   i2:=0;
  GetBuf:=Getbuf Or (i2 Shl (8 - getlen));
  Inc(GetLen,8);
  End;
  i2:=GetBuf;
  GetBuf:=GetBuf shl 1;
  Dec(GetLen);
  GetBit:=SmallInt((i2 < 0));
End;

Function LStreamCompressor.GetByte;
Var
  j:Byte;
  i,Wresult:Word;
Begin
 While (Getlen <= 8)Do
  Begin
   GetBytes(j, 1, Wresult);
   If Wresult = 1 then
    i:=j
   Else
    i:=0;
  Getbuf := getbuf or (i shl (8 - getlen));
  Inc(GetLen, 8);
  End;
  i:=GetBuf;
  Getbuf:=GetBuf Shl 8;
  Dec(getlen,8);
  GetByte:=SmallInt(i Shr 8);
end;

Procedure LStreamCompressor.Update;
Var
  i,j,k,l:SmallInt;
Begin
 If (Freq^[_R]=MAX_FREQ) Then Reconst;
 c:=Prnt^[c + _T];
 Repeat
  Inc(freq^[c]);
  K:=Freq^[c];
 // swap nodes to keep the tree freq-ordered
  L:=Succ(C);
  If (k>Freq^[l])Then
  Begin
   While (k>Freq^[l])Do Inc(l);
   Dec(l);
   Freq^[c]:=Freq^[l];
   Freq^[l]:=k;
   i:=Son^[c];
   Prnt^[i]:=l;
   If (i < _T) Then Prnt^[Succ(i)] := l;
   J:=Son^[l];
   Son^[l]:=i;
   Prnt^[j]:=c;
   If (j < _T)Then Prnt^[Succ(j)] := c;
   Son^[c]:=j;
   C:=l;
  End;
  c:=Prnt^[c];
 Until (c=0); // REPEAT it until reaching the root
End;

Procedure LStreamCompressor.StartHuff;
Var
 I,J:SmallInt;
Begin
 For I:=0 To Pred(N_CHAR)Do
 Begin
  Freq^[i]:=1;
  Son^[i]:=i + _T;
  Prnt^[i + _T]:=i;
 End;
 I:=0;
 J:=N_CHAR;
 While (j <= _R)Do
 Begin
  Freq^[j]:=Freq^[i] + Freq^[i + 1];
  Son^[j]:=i;
  Prnt^[i]:=j;
  Prnt^[i + 1]:=j;
  Inc(I,2);
  Inc(J);
 End;
 Freq^[_T]:=$FFFF;
 Prnt^[_R]:=0;
End;

Procedure LStreamCompressor.PutCode;
Var
  Temp:Byte;
  Got:Word;
Begin
 PutBuf:=PutBuf Or(C Shr PutLen);
 Inc(PutLen,L);
 If (PutLen>=8)Then
  Begin
   Temp:=PutBuf Shr 8;
   PutBytes(Temp, 1, Got);
   Dec(putlen, 8);
   If (PutLen >= 8)Then
    Begin
     Temp := Lo(PutBuf);
     PutBytes(Temp, 1, Got);
     Inc(codesize, 2);
     Dec(putlen, 8);
     PutBuf:=C Shl (L-PutLen);
    End Else
    Begin
     PutBuf:=PutBuf Shl 8;
     Inc(CodeSize);
    End;
  End;
End;

Procedure LStreamCompressor.Reconst;
Var
 I,J,K,Tmp:SmallInt;
 F,L:Word;
Begin
// halven cumulative freq FOR leaf nodes
 j:=0;
 For i:=0 To Pred(_T)Do
 Begin
  If (Son^[i]>=_T)Then
  Begin
   Freq^[j] := Succ(Freq^[i]) Div 2; {@@ Bug Fix MOD -> DIV @@}
   Son^[j] := Son^[i];
   Inc(j);
  End;
 End;
 // make a tree : first, connect children nodes
 i:=0;
 j:=N_CHAR;
 While (j < _T)Do
 Begin
  k:=Succ(i);
  f := freq^[i] + freq^[k];
  Freq^[j] := f;
  k:=Pred(j);
  While f<Freq^[k] Do Dec(K);
  Inc(k);
  l := (j - k) shl 1;
  tmp := SUCC(k);
  Move(freq^[k], freq^[tmp], l);
  Freq^[k] := f;
  Move(son^[k], son^[tmp], l);
  Son^[k] := i;
  Inc(i, 2);
  Inc(j);
 End;
// connect parent nodes
 For i:=0 To Pred(_T)Do
 Begin
  k:=Son^[i];
  If (k >= _T)Then
  Begin
   Prnt^[k] := i;
  End Else
  Begin
   Prnt^[k] := i;
   Prnt^[SUCC(k)] := i;
  End;
 End;
End;

Procedure LStreamCompressor.EncodeChar;
Var
 i:Word;
 j,k:SmallInt;
Begin
 i:=0;
 j:=0;
 k:=Prnt^[c + _T];
 // search connections from leaf node to the root
 Repeat
  i:=i Shr 1;
 {
 IF node's address is odd, output 1
 ELSE output 0
 }
  If Boolean(k And 1)Then Inc(i,$8000);
  Inc(j);
  k:=Prnt^[k];
 Until (k=_R);
 Putcode(j, i);
 Code:=i;
 Len:=j;
 Update(c);
End;

Procedure LStreamCompressor.EncodePosition;
Var
 i,j:Word;
Begin
 // output upper 6 bits with encoding
 i:=c Shr 6;
 j:=p_code[i];
 PutCode(p_len[i],j Shl 8);
 // output lower 6 bits directly
 PutCode(6, (c And $3F) Shl 10);
End;

Procedure LStreamCompressor.EncodeEnd;
Var
 Temp:Byte;
 Got:Word;
Begin
 If Boolean(PutLen) Then
 Begin
  Temp:=Lo(PutBuf Shr 8);
  PutBytes(Temp,1,Got);
  Inc(CodeSize);
 End;
End;

Function LStreamCompressor.DecodeChar;
Var
 C:Word;
Begin
 c:=Son^[_R];
    {
     * start searching tree from the root to leaves.
     * choose node #(son[]) IF input bit = 0
     * ELSE choose #(son[]+1) (input bit = 1)
    }
 While (c < _T) Do
 Begin
  c:=c + GetBit;
  c:=Son^[c];
 End;
 c:=c - _T;
 Update(c);
 DecodeChar:=SmallInt(c);
End;

Function LStreamCompressor.DecodePosition;
Var
 I,J,C:Word;
Begin
// decode upper 6 bits from given table
 i:=GetByte;
 c:=Word(d_code[i] shl 6);
 j:=d_len[i];
// input lower 6 bits directly
 Dec(j, 2);
 While j <> 0 Do
 Begin
  i:=(i Shl 1) + GetBit;
  Dec(J);
 End;
 DecodePosition:=c Or i And $3F;
End;

Function LStreamCompressor.Compress(Source,Dest:LStream; PadSize:Integer=4):Integer;
Var
  Ct:Byte;
  i,L,R,S,Last_Match_Length:SmallInt;
  Got:Word;
  SourceSize:Longint;
Begin
  SourceTarget:=Source;
  DestTarget:=Dest;
  Source.Seek(0);

  TextSize:=0; // rewind and rescan
  StartHuff;
  InitTree;
  s:=0;
  r:=_N - _F;
  FillChar(Text_buf^[0], r, ' ');
  L:=0;
  Got:=1;
  While(L < _F)And(Got <> 0)Do
  Begin
    GetBytes(ct, 1, Got);
    If Got <> 0 Then
    Begin
      Text_buf^[r+L] := ct;
      Inc(l);
    End;
  End;

  Textsize := len;
  For i:=1 To _F Do
    InsertNode(r - i);

  InsertNode(r);
  Repeat
    If (Match_Length > L)Then
      Match_Length:=L;

    If (Match_Length<=THRESHOLD)Then
    Begin
      Match_Length:=1;
      EncodeChar(text_buf^[r]);
    End Else
    Begin
      EncodeChar(255 - THRESHOLD + match_length);
      EncodePosition(match_position);
    End;

    Last_Match_Length:=Match_length;
    i:=0;
    Got:=1;
    While (i < last_match_length)And (Got <> 0) Do
    Begin
      GetBytes(ct, 1, Got);
      If Got <> 0 Then
      Begin
        DeleteNode(s);
        text_buf^[s] := ct;

        If (s < Pred(_F))Then
          text_buf^[s + _N]:=ct;

      s:=Succ(s) And Pred(_N);
      r:=Succ(r) And Pred(_N);
      InsertNode(r);
      Inc(i);
    End;
  End;
  Inc(textsize, i);
  While (i<Last_Match_Length)Do
  Begin
   Inc(i);
   DeleteNode(s);
   s:=Succ(s) And Pred(_N);
   r:=Succ(r) And Pred(_N);
   Dec(l);
   If Boolean(Len)Then InsertNode(r);
  End;
  Until (L<= 0);
  EncodeEnd;

  StreamSize:=TextSize;
  SourceSize:=Source.Size;
  Case PadSize Of
  1:  Begin
        Ct:=Byte(SourceSize);
        Dest.Write(Ct,1);
      End;
  2:  Begin
        Got:=Word(SourceSize);
        Dest.Write(Got,2);
      End;
  4:  Dest.Write(SourceSize,SizeOf(SourceSize));
  Else
    writeln('Invalid padsize.');
  End;

  Result:=TextSize;
End;

Function LStreamCompressor.GetDecompressedSize(Source:LStream; PadSize:Integer=4):Integer;
Var
  N:Byte;
  W:Word;
Begin
  Source.Seek(Source.Size-SizeOf(StreamSize));
  Case PadSize Of
  1:  Begin
        Source.Read(N,1);
        Result:=N;
      End;
  2:  Begin
        Source.Read(W,2);
        Result:=W;
      End;
  4:  Source.Read(Result,SizeOf(Result));
  Else
    writeln('Invalid padsize.');
  End;
  Source.Seek(0);
End;

Function LStreamCompressor.Decompress(Source,Dest:LStream; PadSize:Integer=4):Integer;
Var
 c,i,j,k,r:SmallInt;
 c2:Byte;
 count:Longint;
 Put:Word;
Begin
  StreamSize:=GetDecompressedSize(Source, PadSize);
  Result:=StreamSize;

  SourceTarget:=Source;
  DestTarget:=Dest;

  StartHuff;
  r:=_N - _F;
  FillChar(text_buf^[0], r, ' ');
  Count := 0;
  While Count<StreamSize Do
  Begin
    c:=DecodeChar;
    If (c < 256)Then
    Begin
      c2:=Lo(c);
      PutBytes(c2, 1, Put);
      text_buf^[r] := c;
      Inc(r);
      r:=r And Pred(_N);
      Inc(count);
    End Else
    Begin {c >= 256 }
      i:=(r - Succ(DecodePosition)) And Pred(_N);
      j:=c - 255 + THRESHOLD;
      For K:=0 To Pred(j) Do
      Begin
        c:=text_buf^[(i + k) And Pred(_N)];
        c2:=Lo(c);
        PutBytes(c2, 1, Put);
        text_buf^[r] := c;
        Inc(r);
        r:=r And Pred(_N);
        Inc(count);
      End;
    End;
  End;
End;


//LPackage class
Procedure LPackage.RebuildHeader;
Var
  Tag:LObjectType;
Begin
  Tag:=leafHeader;
  Dest.Seek(_Offset);
  Dest.Write(Tag,SizeOf(Tag));
  Dest.WriteString(_Name);     //Write the name of the package
  Dest.Write(_TableSize,SizeOf(_TableSize)); //Write tablesize
  Dest.Write(_TableOffset,SizeOf(_TableOffset)); //Write table offset
End;

Procedure LPackage.RebuildFileTable(Dest:LStream);
Var
 I:Integer;
Begin
//  SortFileTable;

  Dest.Seek(_TableOffset);
  For I:=0 To Pred(_TableSize)Do
  With _Table[I] Do
  Begin
    Dest.Write(Tag,SizeOf(Tag)); //Write resource type tag
    Dest.WriteString(FileName);  //Write resource name
    Dest.Write(Offset,SizeOf(Offset)); //Write offset of the resource
    Dest.Write(Size,SizeOf(Size));   //Write size of the resource
    Dest.Write(Flags,SizeOf(Flags)); //Write flags
  End;

  _Version.Major:=2;
  _Version.Minor:=0;
  Dest.Write(_Version,SizeOf(_Version));
End;

destructor  LPackage.Destroy();
begin
      SetLength(_Table,0);
     if assigned(_Loader) then _Loader.Destroy;
end;

Constructor LPackage.Create(FileName,PackageName:String);
Var
  Dest:LFileStream;
Begin

    PackageName:='PACKAGE';



  _FileName:=FileName;
  _Name:=PackageName;
  _TableSize:=0;
  _Offset:=0;
  _TableOffset:=SizeOf(LObjectType)+Succ(Length(_Name))+SizeOf(_TableSize)+SizeOf(_TableOffset);
  Dest:=LFileStream.Create(FileName);
  RebuildHeader(Dest);
  _Size:=Dest.Position;
  _DataOffset:=Dest.Position;
  Dest.Destroy;
End;

Constructor LPackage.Create();
Begin

End;



function LPackage.OpenAsset(FileName:String):boolean;
Var
  Tag:LObjectType;
  I,J:Integer;
  S:String;
  
Begin
  _FileName:=FileName;

  If Not FileExists(_FileName) Then
  Begin
    RaiseError('File not found. ['+_FileName+']');
    result:=false;
    Exit;
  End;

  _Loader:=LMemoryStream.Open(_FileName,smRead);
  _Size:=_Loader.Size;
  _Loader.Read(Tag,SizeOf(Tag));
  If Tag<>leafHeader Then
  Begin
    RaiseError('Invalid header. ['+_FileName+']');
        result:=false;
    Exit;
  End;
  _Loader.ReadString(S); //Read package name
  _Name:=S;
  _Loader.Read(_TableSize,SizeOf(_TableSize)); //Read filetable info
  _Loader.Read(_TableOffset,SizeOf(_TableOffset));
  _DataOffset:=_Loader.Position;

  _Loader.Seek(_TableOffset);
  SetLength(_Table,_TableSize);
  For I:=0 To Pred(_TableSize)Do
  With _Table[I] Do
  Begin
    _Loader.Read(Tag,SizeOf(Tag)); //Read resource type tag
    _Loader.ReadString(S); //Read resource name
    FileName:=S;
    writeln(s);
    Name:=UpStr(GetFileName(S,True));
    For J:=0 To Pred(I) Do
    If _Table[J].Name=Name Then
    Begin
      RaiseError('Duplicated resource ['+Name+'] in package '+Self._Name+'.');
          result:=false;
      Exit;
    End;
    _Loader.Read(Offset,SizeOf(Offset));       // Read offset of the resource
    _Loader.Read(Size,SizeOf(Size));           // Size of the resource
    _Loader.Read(Flags,SizeOf(Flags));         // Read flags


  End;

  If _Loader.Position<_Loader.Size Then
    _Loader.Read(_Version,SizeOf(_Version))
  Else
  Begin
    _Version.Major:=1;
    _Version.Minor:=0;
  End;

      result:=true;

End;

function LPackage.GetFile(name:string;Var Dest:LStream):boolean;
Var
 Resource:PResourceInfo;
Begin

Resource:=GetResource(name);
if assigned(Resource) then
begin
// Source:=LFileStream.Open(_FileName,smDefault,_Offset,_Size);
_Loader.Copy(Dest,Resource.Offset,Resource.Size);
// Source.Destroy;
end else
begin

 Assert(Assigned(Resource),'ExtractResource(): Null resource');
result:=false;
end;

End;
function LPackage.GetFile(name:string;var buffer:pointer;var buffersize:integer):boolean;
Var
 Resource:PResourceInfo;
 Dest:LStream;
Begin



Resource:=GetResource(name);
if assigned(Resource) then
begin
   getmem(buffer,size);
   buffersize:=Resource.Size;
  _Loader.Seek(Resource.Offset);
  _Loader.Read(buffer^,buffersize);
   result:=true;
end else
begin
Assert(Assigned(Resource),'ExtractResource(): Null '+name);
result:=false;
end;

End;





Constructor LPackage.OpenMemory(str:pointer;size:integer);
Var


  Tag:LObjectType;
  I,J:Integer;
  S:String;


Begin
  _FileName:='memory';


//  Source:=LFileStream.Open(_FileName,smDefault Or smAppend);
  _Loader:=LMemoryStream.Create(size,str,smRead);
// _Loader:=LMemoryStream.Open('pack.sdlk',smRead);

  _Size:=_Loader.Size;
  _Loader.Read(Tag,SizeOf(Tag));


  If Tag<>leafHeader Then
  Begin
    RaiseError('Invalid header. ['+_FileName+']');
    Exit;
  End;

  _Loader.ReadString(S); //Read package name

  _Name:=S;

  _Loader.Read(_TableSize,SizeOf(_TableSize)); //Read filetable info
  _Loader.Read(_TableOffset,SizeOf(_TableOffset));
  _DataOffset:=_Loader.Position;

  _Loader.Seek(_TableOffset);
  SetLength(_Table,_TableSize);

  For I:=0 To Pred(_TableSize)Do
  With _Table[I] Do
  Begin
    _Loader.Read(Tag,SizeOf(Tag)); //Read resource type tag
    _Loader.ReadString(S); //Read resource name
    FileName:=S;
    writeln(s);
    Name:=UpStr(GetFileName(S,True));
    For J:=0 To Pred(I) Do
    If _Table[J].Name=Name Then
    Begin
      RaiseError('Duplicated resource ['+Name+'] in package '+Self._Name+'.');
      Exit;
    End;
    _Loader.Read(Offset,SizeOf(Offset));       // Read offset of the resource
    _Loader.Read(Size,SizeOf(Size));           // Size of the resource
    _Loader.Read(Flags,SizeOf(Flags));         // Read flags


  End;

  If _Loader.Position<_Loader.Size Then
    _Loader.Read(_Version,SizeOf(_Version))
  Else
  Begin
    _Version.Major:=1;
    _Version.Minor:=0;
  End;


End;


procedure  LPackage.ListFiles()  ;
Var
 I:Integer;
 res:PResourceInfo;
Begin
  Res:=Nil;
  For I:=0 To Pred(_TableSize) Do
  begin

      res:=@(_Table[I]);
       writeln(i);
      writeln(res.Name);
      writeln(res.FileName);
      writeln(res.Offset);
      writeln(res.size);

  end;

  If Not Assigned(res)Then
    RaiseError('Package'+ 'Resource not found.');

end;

//Searches for a resource within the file table
//If not found returns nil
Function LPackage.FindResource(ResourceName:String):PResourceInfo;
Var
 I:Integer;
Begin
  ResourceName:=UpStr(GetFileName(ResourceName,True));
  Result:=Nil;
  For I:=0 To Pred(_TableSize) Do
   If _Table[I].Name=ResourceName Then
    Begin
      Result:=@(_Table[I]);
      Break;
    End;

 // If Not Assigned(Result)Then
 //   RaiseError('Package'+ 'Resource not found.['+ResourceName+']');
End;
Function LPackage.GetResource(ResourceName:String):PResourceInfo;
Var
 I:Integer;
Begin
  ResourceName:=UpStr(GetFileName(ResourceName,True));
  Result:=Nil;
  For I:=0 To Pred(_TableSize) Do
   If _Table[I].Name=ResourceName Then
    Begin
      Result:=@(_Table[I]);
      writeln('load resource .');
                        writeln(_Table[I].Name);
                        writeln(_Table[I].FileName);

      Break;
    End;

  If Not Assigned(Result)Then
    RaiseError('Package'+ 'Resource not found.['+ResourceName+']');
End;

//Loads a resource from the package into a stream
Procedure LPackage.LoadResource(Resource:PResourceInfo;Var Dest:LStream);
Var
 Source:LFileStream;
Begin
  Assert(Assigned(Resource),'Package.LoadResource(): Null resource');



  If FileExists(Path+Resource.FileName) Then
  Begin
    Source:=LFileStream.Open(Path+Resource.FileName);
    If Dest.Size<Source.Size Then
    Begin
      Dest.Destroy;
      Dest:=LMemoryStream.Create(Source.Size);
    End;
    Source.Copy(Dest);
  End Else
  Begin
    Source:=LFileStream.Open(_FileName,smDefault,_Offset,_Size);
    Source.Copy(Dest,Resource.Offset,Resource.Size);
  End;

  Source.Destroy;
End;

//Loads a resource from the package into a stream
Procedure LPackage.ExtractResource(Resource:PResourceInfo;Var Dest:LStream);
Var
 Source:LFileStream;
Begin
 Assert(Assigned(Resource),'ExtractResource(): Null resource');
 Source:=LFileStream.Open(_FileName,smDefault,_Offset,_Size);
 Source.Copy(Dest,Resource.Offset,Resource.Size);
 Source.Destroy;
End;

// Get file stream containing the resource
Function LPackage.OpenResource(Resource:PResourceInfo):LFileStream;
Begin
 Assert(Assigned(Resource),'Package.LoadResource(): Null resource');

 Result:=LFileStream.Open(_FileName,smDefault,Resource.Offset,Resource.Size);
 Result.Name:=Result.Name+':'+Resource.FileName;
End;

Function LPackage.Add(FileName:String):boolean;
Var
  Stream:LStream;
  Tag:LObjectType;
  res:PResourceInfo;
Begin
   Result:=false;

  If Not FileExists(FileName) Then
  Begin
    RaiseError('File not found. ['+FileName+']');
    Result:=false;
    Exit;
  End;

  Stream:=LFileStream.Open(FileName, smDefault Or smAppend);
  Tag:=resFile;

  res:=AddResource(GetFileName(FileName,False),Tag,Stream);
  Stream.Destroy;
End;

Function LPackage.AddFile(FileName:String):PResourceInfo;
Var
  Stream:LStream;
  Tag:LObjectType;
Begin
   Result:=Nil;

  If Not FileExists(FileName) Then
  Begin
    RaiseError('File not found. ['+FileName+']');
    Result:=Nil;
    Exit;
  End;

  Stream:=LFileStream.Open(FileName, smDefault Or smAppend);
  Tag:=resFile;

  Result:=AddResource(GetFileName(FileName,False),Tag,Stream);
  Stream.Destroy;
End;

// Adds a resource to the package
Function LPackage.AddResource(ResourceName:String;ResourceType:LObjectType;Resource:LStream;ResourceFlags:Byte):PResourceInfo;
Var
  Dest:LFileStream;
  Info:PResourceInfo;
Begin


  Info:=FindResource(ResourceName);
  If Assigned(Info) Then   DeleteResource(Info);



  SetLength(_Table,Succ(_TableSize));
  With _Table[_TableSize] Do
  Begin
    Result:=@(_Table[_TableSize]);
    FileName:=ResourceName;
    Name:=UpStr(GetFileName(ResourceName,True));
    FileName:=ResourceName;
    Tag:=ResourceType;
    Size:=Resource.Size-Resource.Position;
    Offset:=GetNewOffset(Size);
    Flags:=ResourceFlags;
    //Start copying the resource into the package file
    Dest:=LFileStream.Open(_FileName, smDefault Or smAppend);
    Dest.Seek(Offset);
    Resource.Copy(Dest, Resource.Position, Size);
    Inc(_TableSize);
    Inc(_Size,SizeOf(LObjectType)+Succ(Length(Name))+SizeOf(Longint)+SizeOf(Longint)+SizeOf(Byte));
    Inc(_Size,Size);
  End;
  _TableOffset:=Dest.Position;

  // Filetable was overwritten by new resource, so rebuild it
  RebuildHeader(Dest);
  RebuildFileTable(Dest);
  Dest.Destroy;
End;

//Removes a resource from the package
Procedure LPackage.DeleteResource(Resource:PResourceInfo);
Var
  I:Integer;
  Stream:LStream;
Begin
  Assert(Assigned(Resource),'DeleteResource(): Null resource.');



  I:=0;
  While I<_TableSize Do
  If _Table[I].Name=Resource.Name Then
  Begin
    _Table[I]:=_Table[Pred(_TableSize)];
    Dec(_TableSize);
    Break;
  End Else
    Inc(I);

  Stream:=LFileStream.Open(FileName, smDefault Or smAppend);
  RebuildFileTable(Stream);
  Stream.Destroy;
End;




Function LPackage.GetNewOffset(Size: Integer): Integer;
Var
  DataStart,I:Integer;
Begin
  If _TableSize=0 Then
  Begin
    Result:=_DataOffset;
    Exit;
  End;

  // Check for fragmented holes to fill
  For I:=0 To Pred(_TableSize) Do
  Begin
    If I=0 Then
      DataStart:=_DataOffset
    Else
      DataStart:=_Table[Pred(I)].Offset+_Table[Pred(I)].Size;
    If (_Table[I].Offset-DataStart>=Size) Then
    Begin
      Result:=DataStart;
      Exit;
    End;
  End;

  //Otherwise calculate new offset from last file in table
  Result:=_Table[Pred(_TableSize)].Offset+_Table[Pred(_TableSize)].Size
End;



         
// LXMLNode

Constructor XMLNode.Create(Name:String; Value:String='');
Begin
  Self._Name:=Name;
  Self._Value:=Value;
End;

Constructor XMLNode.Create(Source:LStream);
Var
  S, S2:String;
  Tag, Value:String;
  I:Integer;
  ShortTag:Boolean;
  Node:XMLNode;
Begin
  _Value := '';

  Repeat
    If (Source.EOF) Then
      Exit;
    S := Read(Source);

  Until (Length(S)>=2) And (S[2]<>'?');

  If GetTagType(S)<>xmlBeginTag Then
  Begin
    RaiseError ('Invalid XML sintax!');
    Exit;
  End;

  ShortTag := S[Pred(Length(S))] = '/';
  S := GetTagName(S);

  I := Pos(' ', S);
  If (I>0) Then
  Begin
    S2 := Copy(S, Succ(I), MaxInt);
    S := Copy(S, 1, Pred(I));
    Self._Name := S;

    S := S2;
    If (S[Length(S)]='/') Then
      SetLength(S, Pred(Length(S)));
    While (S<>'') Do
    Begin
      S2 := GetNextWord(S, ' ');

      I := Pos('=', S2);
      Tag := Copy(S2, 1, Pred(I));
      Value := Copy(S2, I+1, MaxInt);
      Value := TrimRight(TrimLeft(Value));
      Value := Copy(Value, 2, Length(Value)-2);

      Node := XMLNode.Create(Tag, Value);
      AddNode(Node);
    End;
  End Else
    Self._Name := S;

  If (ShortTag) Then
    Exit;

  Repeat
    S := Read(Source);
    Case GetTagType(S) Of
      xmlBeginTag:  Begin
                      Source.Skip(-Length(S));
                      Node := XMLNode.Create(Source);
                      AddNode(Node);
                    End;
      xmlEndTag:    Break;
      xmlData:      _Value:=S;
    End;
  Until False;
End;

Destructor XMLNode.Destroy;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_ChildCount) Do
    _Childs[I].Destroy;
  SetLength(_Childs,0);
  _ChildCount:=0;
End;

Function XMLNode.GetTagType(S:String):XMLTagType;
Begin
  If (S='')Or(S[1]<>'<')Or(S[Length(S)]<>'>') Then
    Result:=xmlData
  Else
  If (S[2]='/') Then
    Result:=xmlEndTag
  Else
    Result:=xmlBeginTag;
End;

Function XMLNode.GetTagName(S:String):String;
Begin
  Result:=Copy(S,2,Length(S)-2);
End;

Function XMLNode.GetParentCount:Integer;
Var
  Node:XMLNode;
Begin
  Node:=Self;
  Result:=-1;
  While Assigned(Node) Do
  Begin
    Inc(Result);
    Node:=Node._Parent;
  End;
End;

Function XMLNode.GetPath:String;
Var
  Node:XMLNode;
Begin
  Node:=Self;
  Result:='';
  While Assigned(Node) Do
  Begin
    If Result<>'' Then
      Result:='.'+Result;
    Result:=Node._Name+Result;
    Node:=Node._Parent;
  End;
End;

Function XMLNode.GetChild(Index:Integer):XMLNode;
Begin
  If (Index<0) Or (Index>=_ChildCount) Then
    Result := Nil
  Else
    Result := _Childs[Index];
End;

Function XMLNode.GetNode(Name:String):XMLNode;
Var
  I:Integer;
Begin
  Name:=UpStr(Name);
  Result:=Nil;
  For I:=0 To Pred(_ChildCount) Do
    If UpStr(_Childs[I].Name)=Name Then
    Begin
      Result:=_Childs[I];
      Exit;
    End;
End;

Function XMLNode.Read(Source:LStream):String;
Const
  BufferSize = 1024;
Var
  S:String;
  C:Char;
Begin
  S := '';
  Repeat
    If (Source.EOF) Then
      Break;

    Source.Read(C, 1);
    If (C='<') And (S<>'') Then
    Begin
      Source.Skip(-1);
      Break;
    End;
    S := S+C;
  Until (C='>');
  Result := TrimLeft(S);
End;

Procedure XMLNode.Save(Dest:LStream);
Var
  Tabs:String;
  I:Integer;
Begin
  Tabs:='';
  For I:=1 To GetParentCount Do
    Tabs:=Tabs+#9;

  If Value<>'' Then
  Begin
    Dest.WriteLine(Tabs+'<'+Name+'>'+Value+'</'+Name+'>');
  End Else
  Begin
    Dest.WriteLine(Tabs+'<'+Name+'>');
    For I:=0 To Pred(_ChildCount) Do
      _Childs[I].Save(Dest);
    Dest.WriteLine(Tabs+'</'+Name+'>');
  End;
End;

Procedure XMLNode.AddNode(Node:XMLNode);
Begin
  Node._Parent:=Self;
  Inc(_ChildCount);
  SetLength(_Childs,_ChildCount);
  _Childs[Pred(_ChildCount)]:=Node;
End;

Function XMLNode.AddTag(Name, Value: String): XMLNode;
Var
  Node:XMLNode;
Begin
  Node := XMLNode.Create(Name,Value);
  AddNode(Node);
  Result:=Node;
End;

// LXMLDocument
Destructor XMLDocument.Destroy;
Begin
  If Assigned(_Root) Then
    _Root.Destroy;
End;

Function XMLDocument.GetNode(Name:String):XMLNode;
Begin
  Result:=Nil;
  If Assigned(_Root) Then
    Result:=_Root.GetNode(Name);
End;

Procedure XMLDocument.AddNode(Node:XMLNode; Parent:XMLNode=Nil);
Begin
  If Assigned(Parent) Then
    Parent.AddNode(Node)
  Else
  Begin
    If Not Assigned(_Root) Then
      _Root:=Node
    Else
      _Root.AddNode(Node);
  End;
End;

Function XMLDocument.AddString(Name:String; Value:String=''; Parent:XMLNode=Nil):XMLNode;
Begin
  If Assigned(Parent) Then
    Result:=Parent.AddTag(Name,Value)
  Else
  Begin
    If Not Assigned(_Root) Then
    Begin
      _Root := XMLNode.Create(Name,Value);
      Result := _Root;
    End Else
      Result := _Root.AddTag(Name,Value);
  End;
End;

Function XMLDocument.AddBoolean(Name:String; Value:Boolean; Parent:XMLNode=Nil):XMLNode;
Begin
  Result := AddString(Name, BoolToString(Value), Parent);
End;

Function XMLDocument.AddInteger(Name:String; Value:Integer; Parent:XMLNode=Nil):XMLNode;
Begin
  Result := AddString(Name, IntToString(Value), Parent);
End;

Function XMLDocument.AddCardinal(Name:String; Value:Cardinal; Parent:XMLNode=Nil):XMLNode;
Begin
  Result := AddString(Name, LongToString(Value), Parent);
End;

Function XMLDocument.AddSingle(Name:String; Value:Single; Parent:XMLNode=Nil):XMLNode;
Begin
  Result := AddString(Name, FloatToString(Value), Parent);
End;

Procedure XMLDocument.Load(Source:LStream);
Begin
  _Root := XMLNode.Create(Source);
End;

Procedure XMLDocument.Save(Dest:LStream);
Begin
  _Root.Save(Dest);
End;

Procedure XMLDocument.Load(FileName:String);
Var
  Source:LFileStream;
Begin
  Source := LFileStream.Open(FileName);
  Load(Source);
  Source.Destroy;
End;

Procedure XMLDocument.Save(FileName:String);
Var
  Dest:LFileStream;
Begin
  Dest := LFileStream.Create(FileName);
  Save(Dest);
  Dest.Destroy;
End;



// XMLElement
Procedure XMLElement.XMLRegisterStructure;
Begin
End;

Procedure XMLElement.XMLSynchronize;
Begin
End;

Function XMLElement.XMLGetPropertyCount:Integer;
Begin
  If (Self._DescriptorCount=0) Then
    XMLRegisterStructure;
  Result := _DescriptorCount;
End;

Function XMLElement.XMLGetProperty(Index:Integer):XMLDescriptor;
Begin
  Result := _Descriptors[Index];
End;

Function XMLElement.XMLGetProperty(Name:String):XMLDescriptor;
Var
  S:String;
  I:Integer;
Begin
  Name := UpStr(Name);

  I := Pos('.', Name);
  If (I>0) Then
  Begin
    S := Copy(Name, Succ(I), MaxInt);
    Name := Copy(Name, 1, Pred(I));
    Result := XMLGetProperty(Name);
    If (S='X') Then
    Begin
      Inc(PByte(Result.Address), 0);
      Result.XMLType := xmlSingle;
    End Else
    If (S='Y') Then
    Begin
      Inc(PByte(Result.Address), 4);
      Result.XMLType := xmlSingle;
    End Else
    If (S='Z') Then
    Begin
      Inc(PByte(Result.Address), 8);
      Result.XMLType := xmlSingle;
    End Else
    If (S='RED') Or (S='R') Then
    Begin
      Inc(PByte(Result.Address), 0);
      Result.XMLType := xmlByte;
    End Else
    If (S='GREEN') Or (S='G') Then
    Begin
      Inc(PByte(Result.Address), 1);
      Result.XMLType := xmlByte;
    End Else
    If (S='BLUE') Or (S='B') Then
    Begin
      Inc(PByte(Result.Address), 2);
      Result.XMLType := xmlByte;
    End Else
    If (S='ALPHA') Or (S='A') Then
    Begin
      Inc(PByte(Result.Address), 3);
      Result.XMLType := xmlByte;
    End Else
      RaiseError('XML: Unknow type component ['+S+']');
    Exit;
  End;

  For I:=0 To Pred(_DescriptorCount) Do
  If (UpStr(_Descriptors[I].Name) = Name) Then
    Result := _Descriptors[I];
End;

Function XMLElement.XMLGetElement(Index:Integer):XMLElement;
Begin
  Result := Nil;
End;

Function XMLElement.XMLGetElementCount():Integer;
Begin
  Result := 0;
End;

Function XMLElement.XMLNewElement(Name:String):XMLElement;
Begin
  Result := Nil;
End;

Procedure XMLElement.XMLRegisterElement(Name:String; Address:Pointer;
                                        XMLType:XMLType; Default:String='');
Begin
  Inc(_DescriptorCount);
  SetLength(_Descriptors,_DescriptorCount);
  _Descriptors[Pred(_DescriptorCount)].Name:=Name;
  _Descriptors[Pred(_DescriptorCount)].Address:=Address;
  _Descriptors[Pred(_DescriptorCount)].XMLType:=XMLType;
  _Descriptors[Pred(_DescriptorCount)].ElementCount:=1;
  _Descriptors[Pred(_DescriptorCount)].Default:=Default;
  _Descriptors[Pred(_DescriptorCount)].Found:=False;
End;

Procedure XMLElement.XMLRegisterArrayElement(Name:String; Address:Pointer;
                                             XMLType:XMLType; Size:Integer);
Begin
  Inc(_DescriptorCount);
  SetLength(_Descriptors,_DescriptorCount);
  _Descriptors[Pred(_DescriptorCount)].Name:=Name;
  _Descriptors[Pred(_DescriptorCount)].Address:=Address;
  _Descriptors[Pred(_DescriptorCount)].XMLType:=XMLType;
  _Descriptors[Pred(_DescriptorCount)].ElementCount:=Size;
  _Descriptors[Pred(_DescriptorCount)].Default:='';
  _Descriptors[Pred(_DescriptorCount)].Found:=False;
End;

Procedure XMLElement.XMLClearStructure;
Begin
  _DescriptorCount := 0;
  SetLength(_Descriptors, 0);
End;

Procedure XMLDescriptor.Read(Node:XMLNode);
Var
  S,S2:String;
  K:Integer;
Begin
  Found:=True;
  S:=Node.Value;

    For K:=1 To ElementCount Do
    Begin
      If ElementCount=1 Then
      Begin
        S2:=S;
        S:='';
      End Else
        S2:=GetNextWord(S,',');
      If (S2='') Then
      Begin
         RaiseError('Number of array elements differs from declaration! ['+Node.GetPath+']');
        Exit;
      End;

      Case XMLType Of
        xmlString:  Begin
                    PString(Address)^:=S2;
                      Inc(PString(Address));
                    End;

        xmlBoolean: Begin
                      PBoolean(Address)^:=StringToBool(S2);
                      Inc(PBoolean(Address));
                    End;

        xmlInteger: Begin
                      PInteger(Address)^:=StringToInt(S2);
                      Inc(PInteger(Address));
                    End;

        xmlCardinal:  Begin
                        PCardinal(Address)^:=StringToLong(S2);
                        Inc(PCardinal(Address));
                      End;

        xmlByte:  Begin
                    PByte(Address)^:=StringToInt(S2);
                    Inc(PByte(Address));
                  End;

        xmlWord:  Begin
                    PWord(Address)^:=StringToLong(S2);
                    Inc(PWord(Address));
                  End;

        xmlSingle:  Begin
                      PSingle(Address)^ := StringToFloat(S2);
                      Inc(PSingle(Address));
                    End;
        Else
          RaiseError('XML'+ 'Invalid XML type '+ IntToString(Cardinal(XMLType)));
      End;
    End;

    If (S<>'') Then
      RaiseError('XML'+ 'Extra array elements discarded! ['+Node.GetPath+']');

End;

Procedure XMLDescriptor.Write(Document:XMLDocument; Node:XMLNode);
Var
  S:String;
  J:Integer;
Begin
  S:='';

  Begin
    For J:=1 To ElementCount Do
    Begin
      If J>1 Then
        S:=S+',';
      Case XMLType Of
        xmlString:  Begin
                      S:=S+PString(Address)^;
                      Inc(PString(Address));
                    End;

        xmlBoolean: Begin
                      S:=S+BoolToString(PBoolean(Address)^);
                      Inc(PBoolean(Address));
                    End;

        xmlInteger: Begin
                      S:=S+IntToString(PInteger(Address)^);
                      Inc(PInteger(Address));
                    End;

        xmlCardinal:  Begin
                        S:=S+LongToString(PCardinal(Address)^);
                        Inc(PCardinal(Address));
                      End;

        xmlByte:    Begin
                      S:=S+IntToString(PByte(Address)^);
                      Inc(PByte(Address));
                    End;

        xmlWord:    Begin
                      S:=S+LongToString(PWord(Address)^);
                      Inc(PWord(Address));
                    End;

        xmlSingle:  Begin
                      S := S + FloatToString(PSingle(Address)^);
                      Inc(PSingle(Address));
                    End;
        Else
           RaiseError( 'Invalid XML type '+ IntToString(Cardinal(XMLType)));
      End;
    End;

    If (S<>Default) Then
      Node.AddTag(Name, S);
  End;
End;

Procedure XMLElement.XMLLoadElements(Source:XMLNode);
Var
  Found:Boolean;
  I,J:Integer;
  Node:XMLNode;
  Element:XMLElement;
Begin
  Self._Status:=xmlReading;
  XMLClearStructure;
  XMLRegisterStructure;

  For I:=0 To Pred(Source._ChildCount) Do
  Begin
    Node := Source._Childs[I];
    Found := False;

    For J:=0 To Pred(_DescriptorCount) Do
    If (UpStr(_Descriptors[J].Name)=UpStr(Node.Name)) Then
    Begin
      _Descriptors[J].Read(Node);
      Found := True;
      Break;
    End;

    If Not Found Then
    Begin
      Element := XMLNewElement(Node.Name);
      If Not Assigned(Element) Then
      Begin
         RaiseError( 'Could not create XML element! ['+Node.Name+']');
        Exit;
      End;

      Element.XMLLoadElements(Node);
      Found := True;
      Break;
    End;
  End;

  For J:=0 To Pred(_DescriptorCount) Do
  If (Not _Descriptors[J].Found) And (_Descriptors[J].Default<>'') Then
  Begin
    Node := XMLNode.Create(_Descriptors[J].Name, _Descriptors[J].Default);
    _Descriptors[J].Read(Node);
    Node.Destroy;
  End;

  XMLSynchronize;
End;

Procedure XMLElement.XMLSaveElements(Document:XMLDocument; Parent:XMLNode=Nil);
Var
  I,J, Count:Integer;
  Node:XMLNode;
  Element:XMLElement;
Begin
  Self._Status:=xmlWriting;
  XMLClearStructure;
  XMLRegisterStructure;

  Node := XMLNode.Create(Self.ClassName);

  For I:=0 To Pred(_DescriptorCount) Do
    _Descriptors[I].Write(Document, Node);

  Count := XMLGetElementCount();
  For J:=0 To Pred(Count) Do
  Begin
    Element := XMLGetElement(J);
    If Not Assigned(Element) Then
    Begin
       RaiseError('XML element not avaliable! ['+IntToString(J)+']');
      Exit;
    End;

    Element.XMLSaveElements(Document, Node);
  End;

  Document.AddNode(Node, Parent);
End;

Procedure XMLElement.XMLLoad(Node:XMLNode);
Begin
  XMLLoadElements(Node);
End;

Procedure XMLElement.XMLLoad(Document:XMLDocument);
Begin
  XMLLoadElements(Document.Root);
End;

Procedure XMLElement.XMLSave(Document: XMLDocument);
Begin
  XMLSaveElements(Document);
End;

Procedure XMLElement.XMLLoad(Source:LStream);
Var
  Document:XMLDocument;
Begin
  Document := XMLDocument.Create;
  Document.Load(Source);
  XMLLoad(Document);
  Document.Destroy;
End;

Procedure XMLElement.XMLSave(Dest:LStream);
Var
  Document: XMLDocument;
Begin
  Document := XMLDocument.Create;
  XMLSave(Document);
  Document.Save(Dest);
  Document.Destroy;
End;

Procedure XMLElement.XMLLoad(FileName:String);
Var
  Source:LStream;
Begin
  Source := LFileStream.Open(FileName);
  XMLLoad(Source);
  Source.Destroy;
End;

Procedure XMLElement.XMLSave(FileName: String);
Var
  Dest:LStream;
Begin
  Dest := LFileStream.Create(FileName);
  XMLSave(Dest);
  Dest.Destroy;
End;

          

Function GetNextLine(Var S:String):String;
Var
  I:Integer;
Begin
  If S='' Then
  Begin
    Result:='';
    Exit;
  End;

  I:=Pos(#13#10,S);
  If I<=0 Then
  Begin
    Result:=S;
    S:='';
  End Else
  Begin
    Result:=Copy(S,1,Pred(I));
    S:=Copy(S,I+2,MaxInt);
  End;
End;

Function GetNextWord(Var S:String; Separator:Char=' '):String;
Var
  I:Integer;
Begin
  S:=TrimLeft(S);
  If S='' Then
  Begin
    Result:='';
    Exit;
  End;

  I:=Pos(Separator,S);
  If I<=0 Then
  Begin
    Result:=S;
    S:='';
  End Else
  Begin
    Result:=Copy(S,1,Pred(I));
    S:=Copy(S,Succ(I),MaxInt);
  End;
  S:=TrimLeft(S);
End;

Function GetNextToken(Var S:String; Separator:Char=','; Op:Char='(';Ed:Char=')'):String;
Var
 I,J,K:Integer;
Begin
  S:=TrimLeft(S);
  If S='' Then
  Begin
    Result:='';
    Exit;
  End;
  If S[1]=Op Then
  Begin
    J:=FindCloseBracket(S, Op, Ed);
    If J=0 Then
    Begin
     Result:=S;
     S:='';
    End Else
    Begin
     Result:=Copy(S,1,J);
     S:=Copy(S,J+1,Length(S)-J);
    End;
   End Else
   Begin
    I:=Pos(Separator,S);
    J:=Pos(Op,S);
    K:=Pos(Ed,S);
    If (J<I)And(J>0)Or(I=0)Then I:=J;
    If (K<I)And(K>0)Or(I=0)Then I:=K;
    If I=0 Then
    Begin
     Result:=S;
     S:='';
    End Else
    Begin
     If I=1 Then K:=1 Else K:=I-1;
     Result:=Copy(S,1,K);
     S:=Copy(S,I,Length(S)-I+1);
    End;
   End;
  S:=TrimLeft(S);
End;

Function GetNextArg(Var S:String):String;
Var
  I:Integer;
Begin
  I:=Pos(',',S);
  If I=0 Then
  Begin
    Result:=S;
    S:='';
  End Else
  Begin
    Result:=Copy(S,1,Pred(I));
    S:=Copy(S,Succ(I),Length(S));
  End;
End;

Function GetFileExt(FileName:String):String;
Var
  I:Integer;
Begin
  Result:='';
  Repeat
    I:=Pos('.',FileName);
    If I<>0 Then
    Begin
      Result:=Copy(FileName,I+1,Length(FileName)-I);
      FileName:=Copy(FileName, Succ(I), MaxInt);
    End;
  Until I<=0;
End;

Function FindCloseBracket(Const S:String; Op:Char='(';Ed:Char=')'):Integer;
Var
 K,I:Integer;
Begin
  K:=0;
  Result:=0;
  For I:=1 To Length(S) Do
  If S[I]=Op Then
    Inc(K)
  Else
  If S[I]=Ed Then
  Begin
    If K=1 Then
    Begin
      Result:=I;
      Break;
    End Else
      Dec(K);
  End;
End;

Function StrClean(Const S:String):String;
Var
  I,N:Integer;
Begin
  N:=Length(S);
  For I:=1 To N Do
    If (S[I]<' ') Then
    Begin
      N:=Pred(I);
      Break;
    End;
  Result:=Copy(S,1,N);
End;

Function TrimLeft(Const S:String):String;
Var
  I,L:Integer;
Begin
  L:=Length(S);
  I:=1;
  While (I <= L) And (S[I]<=' ') Do
    Inc(I);
  Result:=Copy(S,I,Maxint);
End;

Function TrimRight(Const S:String):String;
Var
  I:Integer;
Begin
  I:=Length(S);
  While (I>0) And (S[I]<=' ') Do
    Dec(I);

  Result:=Copy(S,1,I);
End;

//Converts a string to upcase
Function UpStr(Const S:String):String;
Var
  I:Integer;
  C:Char;
Begin
  Result:='';
  For I:=1 To Length(S) Do
  Begin
    C:=S[I];
    If (C>='a')And(C<='z') Then
      Dec(C,32);
    Result:=Result+C;
  End;
End;

//Converts a string to lowercase
Function LowStr(Const S:String):String;
Var
  I:Integer;
  C:Char;
Begin
  Result:='';
  For I:=1 To Length(S) Do
  Begin
    C:=S[I];
    If (C>='A')And(C<='Z') Then
      Inc(C,32);
    Result:=Result+C;
  End;
End;

Function CapStr(Const S:String):String;
Begin
  Result:=LowStr(S);
  If Result<>'' Then
    Result[1]:=UpCase(Result[1]);
End;


Function StringToInt(Const S:String):Integer;
Var
 K:Integer;
Begin
  Val(S,Result,K);
End;

Function StringToLong(Const S:String):Cardinal;
Var
  K:Integer;
Begin
  Val(S,Result,K);
End;

Function StringToBool(Const S:String):Boolean;
Begin

//  Result:=(S=iTrue);
End;

//Converts a string to an Single
Function StringToFloat(Const S:String):Single;
Var
  I:Integer;
  X:Single;
Begin
  System.Val(S,X,I);
  Result := X;
End;


Function StrLPad(S:String;N:Integer; Token:Char='0'):String;
Begin
  While Length(S)<N Do
    S:=Token+S;
  Result:=S;
End;

Function StrRPad(S:String;N:Integer; Token:Char='0'):String;
Begin
  While Length(S)<N Do
    S:=S+Token;
  Result:=S;
End;

Function IntToString(Const N:Integer):String;
Var
  S:String;
Begin
  Str(N,S);
  Result:=S;
End;

Function BoolToString(Const N:Boolean):String;
Begin
  If N Then
    Result:='True'
  Else
    Result:='False';
End;


Function LongToString(Const N:Cardinal):String;
Var
  S:String;
Begin
  Str(N,S);
  Result:=S;
End;

Function FloatToString(Const N:Single):String;
Var
  P,X:Single;
  A,B:Integer;
Begin
  P:=N;
  A:=Trunc(P);
  X:=Abs(Frac(P));
  Repeat
    X:=X*10;
  Until Frac(X)=0;
  B:=Trunc(X);

  Result:=IntToString(A)+'.'+IntToString(B);
  If (StringToFloat(Result)<>N) Then
    Str(P,Result);
End;

Function TicksToString(Const N:Cardinal):String;
Var
  Ext:String;
  X:Single;
  Int,Rem:Integer;
Begin
  If (N>=60000)Then
  Begin
    X:=N/60000;
    Int:=Trunc(X);
    Rem:=Trunc(Frac(X)*10);
    Ext:='m';
  End Else
  If (N>=1000)Then
  Begin
    X:=N/1000;
    Int:=Trunc(X);
    Rem:=Trunc(Frac(X)*10);
    Ext:='s';
  End Else
  Begin
    Int:=N;
    Rem:=0;
    Ext:='ms';
  End;

  Result:=IntToString(Int);
  If Rem>0 Then
    Result:=Result+'.'+IntToString(Rem);
  Result:=Result+' '+Ext;
End;

Function MemoryToString(Const N:Cardinal):String;
Var
  Ext:Char;
  X:Single;
  Int,Rem:Integer;
Begin
  If (N>=1 Shl 30)Then
  Begin
    X:=N/(1 Shl 30);
    Int:=Trunc(X);
    Rem:=Trunc(Frac(X)*10);
    Ext:='G';
  End Else
  If (N>=1 Shl 20)Then
  Begin
    X:=N/(1 Shl 20);
    Int:=Trunc(X);
    Rem:=Trunc(Frac(X)*10);
    Ext:='M';
  End Else
  If (N>=1 Shl 10)Then
  Begin
    X:=N/(1 Shl 10);
    Int:=Trunc(X);
    Rem:=Trunc(Frac(X)*10);
    Ext:='K';
  End Else
  Begin
    Int:=N;
    Rem:=0;
    Ext:=#0;
  End;

  Result:=IntToString(Int);
  If Rem>0 Then
  Result:=Result+'.'+IntToString(Rem);
  Result:=Result+' ';
  If Ext<>#0 Then
    Result:=Result+Ext;
  Result:=Result+'b';
End;

Function GetVersionID(Const Major,Minor,Build:Word):Word;
Begin
 Result:=(Major*1000)+(Minor*100)+Build;
End;

Procedure ReplaceText(Const Token,Value:String; Var S:String);
Var
 I:Integer;
 S2:String;
Begin
  If (Token = Value) Then
    Exit;
  I := Pos(Upstr(Token),Upstr(S));
  While (I>0) Do
  Begin
    S2:=Copy(S,I+Length(Token),Length(S)-I);
    S:=Copy(S,1,Pred(I));
    S:=S+Value+S2;
    I:=Pos(Upstr(Token),Upstr(S));
  End;
End;
   
Procedure ReplaceAllText(Const Token,Value:String; Var S:String);
Begin
  If (Token = Value) Then
    Exit;

  While Pos(Token, S)>0 Do
    ReplaceText(Token, Value, S);
End;

Function HexStr(Const Value:Byte):String;
Const
  Hex:Array[0..15] Of AnsiChar='0123456789ABCDEF';
Var
  txt:String[2];
Begin
  txt[0] := Char(2);
  txt[1] := Hex[(value SHR  4) AND $0F];
  txt[2] := Hex[value AND $0F];
  Result:= txt;
End;

Function HexStr(Const Value:Word):String;
Const
  Hex:Array[0..15] Of AnsiChar='0123456789ABCDEF';
Var
  txt:String[4];
Begin
  txt[0]:= Char(4);
  txt[1]:= Hex[(value SHR 12) AND $0F];
  txt[2]:= Hex[(value SHR  8) AND $0F];
  txt[3]:= Hex[(value SHR  4) AND $0F];
  txt[4]:= Hex[value AND $0F];
  Result:= txt;
End;

Function HexStr(Const Value:Cardinal):String;
Const
  Hex : Array[0..15] of AnsiChar = '0123456789ABCDEF';
Var
  txt : String[8];
Begin
  txt[0]:= Char(8);
  txt[1]:= Hex[(value SHR 28) AND $0F];
  txt[2]:= Hex[(value SHR 24) AND $0F];
  txt[3]:= Hex[(value SHR 20) AND $0F];
  txt[4]:= Hex[(value SHR 16) AND $0F];
  txt[5]:= Hex[(value SHR 12) AND $0F];
  txt[6]:= Hex[(value SHR  8) AND $0F];
  txt[7]:= Hex[(value SHR  4) AND $0F];
  txt[8]:= Hex[value AND $0F];
  Result:= txt;
End;

Const
  Base64Code= 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'+
              'abcdefghijklmnopqrstuvwxyz'+
              '0123456789+/';
  Pad = '=';

Function StringToBase64(Buf:String):String;
Var
  I:Integer;
  x1,x2,x3,x4:Byte;
  PadCount:Integer;
Begin
  PadCount:=0;
 // we need at least 3 input bytes...
  While Length(Buf)<3 Do
  Begin
    Buf := Buf + #0;
    Inc( PadCount );
  End;
 // ...and all input must be an even multiple of 3
  While ( length( Buf ) mod 3 ) <> 0 do
  Begin
    Buf:=Buf+#0; // if not, zero padding is added
    Inc(PadCount);
  End;

  Result:= '';
  I:=1;

 // process 3-byte blocks or 24 bits
  While (I<=Length(Buf)-2) Do
  Begin
    // each 3 input bytes are transformed into 4 index values
    // in the range of  0..63, by taking 6 bits each step

    // 6 high bytes of first char
    x1 := ( Ord( Buf[i] ) shr 2 ) and $3F;

    // 2 low bytes of first char + 4 high bytes of second char
    x2 := ( ( Ord( Buf[i] ) shl 4 ) and $3F )
      or Ord( Buf[i + 1] ) shr 4;

    // 4 low bytes of second char + 2 high bytes of third char
    x3 := ( ( Ord( Buf[i + 1] ) shl 2 ) and $3F )
      or Ord( Buf[i + 2] ) shr 6;

    // 6 low bytes of third char
    x4 := Ord( Buf[i + 2] ) and $3F;

    // the index values point into the code array
    Result:=Result + Base64Code[x1 + 1] + Base64Code[x2 + 1]
                   + Base64Code[x3 + 1] + Base64Code[x4 + 1];
    Inc(i,3);
  End;

 // if needed, finish by forcing padding chars ('=')
 // at end of string
  If PadCount>0 Then
    For i := Length( Result ) downto 1 do
    Begin
      Result[i]:=Pad;
      Dec(PadCount);
      If PadCount=0 Then
        Break;
    End;
End;

// helper : given a char, returns the index in code table
Function Char2IDx(c:Char):Byte;
Var
  I:Integer;
Begin
  For I:=1 To Length(Base64Code) Do
  If Base64Code[i]=c Then
  Begin
    Result:=Pred(I);
    Exit;
  End;
  Result:=Ord(Pad);
End;

Function Base64ToString(B64:String):String;
Var
  I,PadCount:Integer;
  Block:String[3];
  N:Integer;
  A,B,C,D:Byte;
Begin
	// input _must_ be at least 4 chars long,
	// or multiple of 4 chars
	If (Length(B64)<4) Or (Length(B64) Mod 4<>0) Then
	Begin
		Result:='';		
		Exit;
	End;

  PadCount:=0;
  I:=Length(B64);
  // count padding chars, if any
  While (B64[i]=Pad) And (i>0) Do
  Begin
    Inc(PadCount);
    Dec(I);
  End;

  Result:='';
  i:=1;
  SetLength(Block,3);
  While i<=Length(B64)-3 Do

  Begin
    A:=Char2Idx(B64[I+0]);
    B:=Char2IDx(B64[I+1]);
    C:=Char2IDx(B64[I+2]);
    D:=Char2IDx(B64[I+3]);

    // reverse process of above
    N:=(A Shl 2) Or (B Shr 4);
    Result:=Result+Chr(N);

    N:=(B Shl 4) Or (C Shr 2);
    Result:=Result+Chr(N);

    N:=(C Shl 6 ) Or D;
    Result:=Result+Chr(N);
    Inc(i,4);
  End;

  // delete padding, if any
  While PadCount>0 Do
  Begin
    Delete(Result, Length(Result), 1);
    Dec(PadCount);
  End;
End;

Function UnicodeChar(Code:Word):String;
Var
  A,B:Byte;
Begin
  A := Byte(Code And $FF);
  B := Byte((Code Shr 8) And $FF);
  Result := #255 + Chr(B) + Chr(A);
End;

//Returns the position of a substring inside of a string
//Search starts from the end of the string.
Function PosRev(Const SubStr,Str:String):Integer;
Var
 I,K,N:Integer;
Begin
 K:=0;
 N:=Length(SubStr);
 For I:=Length(Str) DownTo 1 Do
 Begin
  If Str[I]=SubStr[N] Then
  Begin
   If N=1 Then
   Begin
    K:=I;
    Break;
   End Else
    Dec(N);
  End Else
   N:=Length(SubStr);
 End;
 Result:=K;
End;

Procedure SetFlag(Var N:Byte; Const Flag:Byte; Const Value:Boolean);
Begin
  If (((N And Flag)<>0)<>Value)Then N:=N Xor Flag;
End;


Procedure FillByte(Var Dest; Count:Integer; Value:Byte);
Var
  P:PByte;
Begin
  P:=@Dest;
  While (Count>0) Do
  Begin
    P^:=Value;
    Inc(P);
    Dec(Count);
  End;
End;

Procedure FillWord(Var Dest; Count:Integer; Value:Word);
Var
  P:PWord;
Begin
  P:=@Dest;
  While (Count>0) Do
  Begin
    P^:=Value;
    Inc(P);
    Dec(Count);
  End;
End;

Procedure FillLong(Var Dest; Count:Integer; Value:Cardinal);
Var
  P:PCardinal;
Begin
  P:=@Dest;
  While (Count>0) Do
  Begin
    P^:=Value;
    Inc(P);
    Dec(Count);
  End;
End;

Function MatchRegEx(S, Expression:String):Boolean;
Var
  I, J:Integer;
Begin
  I := 1;
  J := 1;
  Result := False;
  While (I<=Length(S)) And (J<=Length(Expression)) Do
  Begin
    If (Expression[J] = '*') Then
    Begin
      While (Expression[J] = '*') Do
      Begin
        If (J>=Length(Expression)) Then
        Begin
          Result := True;
          Exit;
        End;
        Inc(J);
      End;

      While (S[I]<>Expression[J]) Do
      Begin
        Inc(I);
        If (I>Length(S)) Then
          Exit;
      End;

      Inc(I);
      Inc(J);
    End Else
    If (S[I]=Expression[J]) Then
    Begin
      Inc(I);
      Inc(J);
    End Else
      Exit;
  End;

  Result := True;
End;





Function LongMin(Const A,B:Cardinal):Cardinal; {$IFDEF FPC} Inline; {$ENDIF}
Begin
  If A<B Then Result:=A Else Result:=B;
End;

Function LongMax(Const A,B:Cardinal):Cardinal; {$IFDEF FPC} Inline; {$ENDIF}
Begin
  If A>B Then Result:=A Else Result:=B;
End;




Function GetFilePath(FileName:String):String;
Var
  I:Integer;
  S:String;
Begin
  I:=Pos(PathSeparator,FileName);
  S:='';
  While I<>0 Do
  Begin
    S:=S+Copy(FileName,1,I);
    FileName:=Copy(FileName,I+1,Length(FileName)-I);
    I:=Pos(PathSeparator,FileName);
  End;
  Result:=S;
End;

Function GetLastFilePath(FileName:String):String;
Var
  I:Integer;
Begin
  Result:=GetFilePath(FileName);
  Result:=Copy(Result, 1, Pred(Length(Result)));
  I:=Pos('\', Result);
  While (I>0) Do
  Begin
    Result:=Copy(Result, Succ(I), MaxInt);
    I:=Pos('\', Result);
  End;
End;

Function GetFirstFilePath(FileName:String):String;
Var
  I:Integer;
  S:String;
Begin
  S:=GetFilePath(FileName);
  S:=Copy(S, 1, Pred(Length(S)));
  I:=Pos('\', S);
  Result:='';
  While (I>0) Do
  Begin
    Result:=Result+Copy(S, 1, I);
    S:=Copy(S, Succ(I), MaxInt);
    I:=Pos('\', S);
  End;
End;

Function GetFileName(FileName:String;Const RemoveExt:Boolean):String;
Var
  I:Integer;
Begin
  I:=Pos(':',FileName);
  If I>0 Then
    FileName:=Copy(FileName,Succ(I),MaxInt);

  I:=Pos(PathSeparator,FileName);
  While I<>0 Do
  Begin
    FileName:=Copy(FileName,I+1,Length(FileName)-I);
    I:=PosRev(PathSeparator,FileName);
  End;
  Result:=FileName;
  If RemoveExt Then
  Begin
    I:=PosRev('.',FileName);
    If I<>0 Then Result:=Copy(FileName,1,I-1);
  End;
End;

Function GetCurrentDir:String;
Begin
  Result:='';
  GetDir(0,Result);
End;

{
Const
  Polynomial=$EDB88320;

Var
  CRC_Table:Array[0..255]Of Cardinal;

Procedure GenerateCRC_Table;
Var
  I:Integer;
  Count:Integer;
  CRC:Cardinal;
Begin
  For I:=0 To 255 Do
  Begin
    CRC:=I;
    For Count:=8 DownTo 1 Do
    Begin
      If (CRC And 1)>0 Then
        CRC:=(CRC Shr 1) Xor Polynomial
      Else
        CRC:=CRC Shr 1;
   End;
    CRC_Table[I]:=CRC;
  End;
End;


Function GetCRC32(Source:LStream; Start,Length:Integer):Cardinal;
Const
  BufferSize=1024*32;
Var
  I:Integer;
  OP,Pos,Size:Integer;
  CRC:Cardinal;
  Buffer:Array[0..Pred(BufferSize)]Of Byte;
  BlockSize:Word;
Begin
  CRC:=$FFFFFFFF;
  OP:=Source.Position;
  Source.Seek(Start);
  Size:=Length;
  Pos:=0;
  While (Pos<Size)Do
  Begin
    BlockSize:=IntMin(BufferSize,Source.Size-Source.Position);
    Source.Read(Buffer[0],BlockSize);
    For I:=0 To Pred(BlockSize)Do
      CRC:=(CRC Shr 8) Xor CRC_Table[(CRC Xor Buffer[I])And $FF];
    Inc(Pos,BlockSize);
  End;
  CRC:=CRC Xor $FFFFFFFF;
  Source.Seek(OP);
  Result:=CRC;
End;


Function GetCRC32(Source:LStream):Cardinal;
Begin
  Result:=GetCRC32(Source, 0, Source.Size);
End;


Begin
 GenerateCRC_Table;
 }


End.

