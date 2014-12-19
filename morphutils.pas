unit morphutils;

interface
uses Classes,opengl,  SysUtils,morphmath;

   const
   cMaxArray = (MaxInt shr 4);


type



   TIntegerVector = array[0..cMaxArray] of integer;
   TFloatVector = array[0..cMaxArray] of Single;
   TSingleArray = array of Single;
   PFloatVector = ^TFloatVector;
   PFloatArray = PFloatVector;
   PSingleArray = PFloatArray;
   TSingleArrayList = array [0..MaxInt shr 4] of Single;
   PSingleArrayList = ^TSingleArrayList;
   TIntegerArray = array [0..MaxInt shr 3] of Integer;
   PIntegerArray = ^TIntegerArray;
   PInteger = ^Integer;
   TSmallIntArray = array [0..MaxInt shr 2] of SmallInt;
   PSmallIntArray = ^TSmallIntArray;
   PSmallInt = ^SmallInt;
   TShortIntArray = array [0..MaxInt shr 2] of ShortInt;
   PShortIntArray = ^TShortIntArray;
   PShortInt = ^ShortInt;



	TSingleList = class
		private
      FItemSize : Integer;
   		FCount : Integer;
			FCapacity : Integer;
			FGrowthDelta : Integer;
      FBufferItem : PByteArray;
    	FList : PSingleArrayList;

	protected
      procedure SetCount(val : Integer);
			function  Get(Index: Integer) : Single;
			procedure Put(Index: Integer; const item : Single);
			procedure SetCapacity(NewCapacity: Integer);

		public
			constructor Create;
      procedure Delete(index: Integer);
			function  Add(const item : Single) : Integer;
			procedure Push(const val : Single);
			function  Pop : Single;
      function push_back(v:single):integer;
      procedure Flush;
      procedure Clear;
			procedure Insert(Index : Integer; const item : Single);
			property Items[Index: Integer] : Single read Get write Put; default;
			property List : PSingleArrayList read FList;
     	property Count : Integer read FCount write SetCount;
 			property Capacity : Integer read FCapacity write SetCapacity;
 			property GrowthDelta : Integer read FGrowthDelta write FGrowthDelta;
	end;



	TIntegerList = class
		private
      FItemSize : Integer;
   		FCount : Integer;
			FCapacity : Integer;
			FGrowthDelta : Integer;
      FBufferItem : PByteArray;
    	FList : PIntegerArray;

	protected
      procedure SetCount(val : Integer);
			function  Get(Index: Integer) : Integer;
			procedure Put(Index: Integer; const item : Integer);
			procedure SetCapacity(NewCapacity: Integer);

		public
			constructor Create;
      procedure Delete(index: Integer);
			function  Add(const item : Integer) : Integer;
			procedure Push(const val : Integer);
			function  Pop : Integer;
      procedure Flush;
      procedure Clear;
      function push_back(v:integer):integer;
			procedure Insert(Index : Integer; const item : Integer);
			property Items[Index: Integer] : Integer read Get write Put; default;
			property List : PIntegerArray read FList;
     	property Count : Integer read FCount write SetCount;
 			property Capacity : Integer read FCapacity write SetCapacity;
 			property GrowthDelta : Integer read FGrowthDelta write FGrowthDelta;
	end;



implementation



// ------------------
// ------------------ TSingleList ------------------
// ------------------

// Create
//
constructor TSingleList.Create;
begin
  FItemSize:=SizeOf(Single);
  FGrowthDelta:=16;
	inherited Create;

end;


procedure TSingleList.SetCount(val : Integer);
begin
   Assert(val>=0);
   if val>FCapacity then       SetCapacity(val);
 //  if (val>FCount)  then
   //   FillChar(FList[FItemSize*FCount], (val-FCount)*FItemSize, 0);


   //if (val>FCount) and (bloSetCountResetsMemory in FOptions) then
   //   FillChar(FBaseList[FItemSize*FCount], (val-FCount)*FItemSize, 0);
   FCount:=val;
end;


// Add
//
function TSingleList.Add(const item : Single): Integer;
begin
	Result:=FCount;
	if Result=FCapacity then  SetCapacity(FCapacity+FGrowthDelta);
	FList[Result]:=Item;
  Inc(FCount);
end;

function TSingleList.push_back(v:single):integer;
begin
result:=add(v);
end;


// Get
//
function TSingleList.Get(Index : Integer) : Single;
begin
{$IFOPT R+}
   Assert(Cardinal(Index)<Cardinal(FCount));
{$endif}
	Result := FList[Index];
end;

// Insert
//
procedure TSingleList.Insert(Index : Integer; const Item : Single);
begin
{$IFOPT R+}
   Assert(Cardinal(Index)<Cardinal(FCount));
{$ENDIF}
	if FCount=FCapacity then SetCapacity(FCapacity+FGrowthDelta);
	if Index<FCount then
		System.Move(FList[Index], FList[Index + 1],
						(FCount-Index)*SizeOf(Single));
	FList[Index]:=Item;
	Inc(FCount);
end;

procedure TSingleList.Flush;
begin
	if Assigned(Self) then begin
      SetCount(0);
	end;
end;

// Clear
//
procedure TSingleList.Clear;
begin
	if Assigned(Self) then begin
		SetCount(0);
		SetCapacity(0);
	end;
end;


// Put
//
procedure TSingleList.Put(Index : Integer; const Item : Single);
begin
{$IFOPT R+}
   Assert(Cardinal(Index)<Cardinal(FCount));
{$ENDIF}
	FList[Index] := Item;
end;

// SetCapacity
//
procedure TSingleList.SetCapacity(NewCapacity : Integer);
begin
   inherited;
  // FList:=PSingleArrayList(NewCapacity);
		ReallocMem(FList, newCapacity*FItemSize);
		FCapacity:=newCapacity;

end;

// Push
//
procedure TSingleList.Push(const val : Single);
begin
	Add(val);
end;

procedure TSingleList.Delete(index: Integer);
begin
{$IFOPT R+}
   Assert(Cardinal(index)<Cardinal(FCount));
{$ENDIF}
	Dec(FCount);
	if index<FCount then
		System.Move(FList[(index+1)*FItemSize],
                  FList[index*FItemSize],
                  (FCount-index)*FItemSize);
end;

// Pop
//
function TSingleList.Pop : Single;
begin
	if FCount>0 then begin
		Result:=Get(FCount-1);
		Delete(FCount-1);
	end else Result:=0;
end;









// ------------------
// ------------------ TIntegerList ------------------
// ------------------

// Create
//
constructor TIntegerList.Create;
begin
  FItemSize:=SizeOf(Single);
  FGrowthDelta:=16;
	inherited Create;

end;


procedure TIntegerList.SetCount(val : Integer);
begin
   Assert(val>=0);
   if val>FCapacity then       SetCapacity(val);
 //  if (val>FCount)  then
   //   FillChar(FList[FItemSize*FCount], (val-FCount)*FItemSize, 0);


   //if (val>FCount) and (bloSetCountResetsMemory in FOptions) then
   //   FillChar(FBaseList[FItemSize*FCount], (val-FCount)*FItemSize, 0);
   FCount:=val;
end;


// Add
//
function TIntegerList.Add(const item : integer): Integer;
begin
	Result:=FCount;
	if Result=FCapacity then  SetCapacity(FCapacity+FGrowthDelta);
	FList[Result]:=Item;
  Inc(FCount);
end;

// Get
//
function TIntegerList.Get(Index : Integer) : integer;
begin
{$IFOPT R+}
   Assert(Cardinal(Index)<Cardinal(FCount));
{$endif}
	Result := FList[Index];
end;

// Insert
//
procedure TIntegerList.Insert(Index : Integer; const Item : integer);
begin
{$IFOPT R+}
   Assert(Cardinal(Index)<Cardinal(FCount));
{$ENDIF}
	if FCount=FCapacity then SetCapacity(FCapacity+FGrowthDelta);
	if Index<FCount then
		System.Move(FList[Index], FList[Index + 1],
						(FCount-Index)*SizeOf(Single));
	FList[Index]:=Item;
	Inc(FCount);
end;

// Put
//
procedure TIntegerList.Put(Index : Integer; const Item : integer);
begin
{$IFOPT R+}
   Assert(Cardinal(Index)<Cardinal(FCount));
{$ENDIF}
	FList[Index] := Item;
end;

// SetCapacity
//
procedure TIntegerList.SetCapacity(NewCapacity : Integer);
begin
   inherited;
  // FList:=PSingleArrayList(NewCapacity);
		ReallocMem(FList, newCapacity*FItemSize);
		FCapacity:=newCapacity;

end;

// Push
//
procedure TIntegerList.Push(const val : integer);
begin
	Add(val);
end;

procedure TIntegerList.Delete(index: Integer);
begin
{$IFOPT R+}
   Assert(Cardinal(index)<Cardinal(FCount));
{$ENDIF}
	Dec(FCount);
	if index<FCount then
		System.Move(FList[(index+1)*FItemSize],
                  FList[index*FItemSize],
                  (FCount-index)*FItemSize);
end;

procedure TIntegerList.Flush;
begin
	if Assigned(Self) then begin
      SetCount(0);
	end;
end;

// Clear
//
procedure TIntegerList.Clear;
begin
	if Assigned(Self) then begin
		SetCount(0);
		SetCapacity(0);
	end;
end;

function TIntegerList.push_back(v:integer):integer;
begin
result:=add(v);
end;


// Pop
//
function TIntegerList.Pop : integer;
begin
	if FCount>0 then
  begin
		Result:=Get(FCount-1);
		Delete(FCount-1);
	end else Result:=0;
end;








end.
