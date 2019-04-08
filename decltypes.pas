unit DeclTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JClassReader;

type
  TAccessType = (acPublic, acProtected, acPrivate);
  TDeclTypes = class;
  TInterfaceDeclType = class;

  { TBaseDeclType }

  TBaseDeclType = class
  protected
    FParent: TDeclTypes;
    FDepends: TFPList; // list of TBaseDeclType
    FAccessType: TAccessType;
    FOrigName: string;
    FName: string;
    FState: (stNone, stNeedPredeclare, stDone);
    printOrder: Integer;
    FRealDependances: TFPList;
    killed: Boolean; // true when was killed to resolve circular dependance (for some inner class)
    procedure AddDependance(dep: TBaseDeclType);
  public
    constructor Create(Parent: TDeclTypes; Name, AOrigName: string);
    destructor Destroy; override;
    procedure Print(DestStream: TStream; indent: Integer;
      MaxAccessType: TAccessType); virtual; abstract;
    property OrigName: string read FOrigName;
    function GetName(forWhat: TBaseDeclType): string; virtual;
  end;

  { TBuiltInDeclType }

  TBuiltInDeclType = class(TBaseDeclType)
  public
    constructor Create(Parent: TDeclTypes; Name, AOrigName: string);
    procedure Print(DestStream: TStream; indent: Integer;
      MaxAccessType: TAccessType); override;
  end;

  { TArrayDeclType }

  TArrayDeclType = class(TBaseDeclType)
  protected
    FElemType: TBaseDeclType;
    FDeclaredIn: TInterfaceDeclType;
  public
    constructor Create(Parent: TDeclTypes; Name: string;
      ElemType: TBaseDeclType);
    function GetName(forWhat: TBaseDeclType): string; override;
    procedure Print(DestStream: TStream; indent: Integer;
      MaxAccessType: TAccessType); override;
  end;

  TMethodDecl = record
    Access: TAccessType;
    IsFinal: Boolean;
    IsStatic: Boolean;
    IsAbstract: Boolean;
    IsVarArgs: Boolean;
    MethodName: string;
    ResultType: TBaseDeclType;
    Params: array of record
      ParamName: string;
      ParamType: TBaseDeclType;
    end;
    Exceptions: array of string;
  end;

  TFieldDecl = record
    Access: TAccessType;
    IsFinal: Boolean;
    IsStatic: Boolean;
    FieldName: string;
    FieldType: TBaseDeclType;
    ConstVal: string;
  end;

  { TInterfaceDeclType }

  TInterfaceDeclType = class(TBaseDeclType)
  protected
    IsAbstract: Boolean;
    IsFinal: Boolean;
    FInterfaces: TFPList; // list of TInterfaceDeclType
    FMethods: array of TMethodDecl;
    FJavaClass: TJavaClassFile;
    FFields: array of TFieldDecl;
    FInnerClasses: TFPList; // inner(nested) classes
    FLocalName: string; // <> '' => inner
    FParentClass: TInterfaceDeclType;
    FArrayTypes: TFPList;
    sec_printed, sec_clear: Boolean;
    sec_type_printed: TAccessType;
    function AncestorHasField(FieldName: string): Boolean; virtual;
    procedure CompleteToFullHeader(var s: string); virtual;
    procedure EnsureSectionPrinted(DestStream: TStream; indent: Integer;
      Sec: TAccessType); virtual;
    function Find(AOrigName: string; Strict: Boolean): TBaseDeclType;
    function Header(Full: Boolean): string; virtual;
    function HasField(FieldName: string): Boolean;
    function Inheritance: string; virtual;
    function MethodModifiers(m: TMethodDecl): string; virtual;
    procedure FixInnerName;
    procedure Parse; virtual;
    procedure ParseFields;
    procedure PrintHeader(DestStream: TStream; indent: Integer);
    procedure PrintBody(DestStream: TStream; indent: Integer; MaxAccessType: TAccessType); virtual;
    procedure PrintInnerClasses(DestStream: TStream; indent: Integer; MaxAccessType: TAccessType);
    procedure PrintConstants(DestStream: TStream; indent: Integer; MaxAccessType: TAccessType);
    procedure PrintMethods(DestStream: TStream; indent: Integer; MaxAccessType: TAccessType); virtual;
  public
    constructor Create(Parent: TDeclTypes; Name: string;
      JavaClass: TJavaClassFile);
    destructor Destroy; override;
    function GetName(forWhat: TBaseDeclType): string; override;
    function IsInner: Boolean;
    function HasCommonAncestorWith(cl: TBaseDeclType): Boolean;
    procedure PrintPredecl(fout: TStream; Indent: Integer); virtual;
    procedure Print(DestStream: TStream; indent: Integer; MaxAccessType: TAccessType); override;
  end;

  { TClassDeclType }

  TClassDeclType = class(TInterfaceDeclType)
  protected
    FSuperClass: TInterfaceDeclType;
    function AncestorHasField(FieldName: string): Boolean; override;
    procedure CompleteToFullHeader(var s: string); override;
    function Header(Full: Boolean): string; override;
    procedure Parse; override;
    function Inheritance: string; override;
    procedure EnsureSectionPrinted(DestStream: TStream; indent: Integer;
      Sec: TAccessType); override;
    function MethodModifiers(m: TMethodDecl): string; override;
    procedure PrintBody(DestStream: TStream; indent: Integer; MaxAccessType: TAccessType); override;
    procedure PrintFields(DestStream: TStream; indent: Integer; MaxAccessType: TAccessType);
  end;

  TClassNotFoundEvent = procedure (Sender: TObject; JavaClassName: string;
    var Failed: Boolean) of object;

  { TDeclTypes }

  TDeclTypes = class
  private
    FItems, FFakeTypes, FPrintOrder, FNeedPredecl, FInheritanceOrder: TFPList;
    FJavaTypes: TStringList;
    FOnClassNotFound: TClassNotFoundEvent;
    FStat: record
      classes,
      interfaces,
      arrays: Integer;
    end;
    procedure ClearItems;
    function GetItems(Index: Integer): TBaseDeclType;
    procedure InitPredefined;
    function JavaClassPascalName(JavaName: string): string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Count: Integer;
    procedure Add(JavaClass: TJavaClassFile);
    procedure AddFake(OrigName: string); // for "-x" option
    function AddArray(ElementType: TBaseDeclType): TBaseDeclType;
    function Find(OrigName: string; Strict: Boolean): TBaseDeclType;
    procedure PrintAllPredeclarations(fout: TStream);
    procedure PrintNeededPredeclarations(fout: TStream);
    procedure PrintArrays(fout: TStream);
    procedure PrintDeclarations(fout: TStream; MaxAccessType: TAccessType);
    procedure PrintDeclarationsInheritanceOrder(fout: TStream; MaxAccessType: TAccessType);
    property Items[Index: Integer]: TBaseDeclType read GetItems; default;
    property OnClassNotFound: TClassNotFoundEvent read FOnClassNotFound write FOnClassNotFound;
  end;

implementation

uses strutils;

var
  FPCReserved: TStringList;

function FixName(name: string): string;
begin
  if FPCReserved.IndexOf(name) >= 0 then
    Result := '&' + name
  else
  if name = 'create' then
    Result := '_create'
  else
  if Pos('$', name) > 0 then
    Result := StringReplace(name, '$', '_', [rfReplaceAll])
  else
    Result := name;
end;

function ReadType(var desc: string): string;
var
  c: Char;
  i: Integer;
begin
  if desc = '' then Exit('');
  i := 1;
  c := desc[1];
  case c of
  'B': Result := 'byte';   // signed byte
  'C': Result := 'char';   // Unicode character code point in the Basic Multilingual Plane, encoded with UTF-16
  'D': Result := 'double'; // double-precision floating-point value
  'F': Result := 'float';  // single-precision floating-point value
  'I': Result := 'int';    // integer
  'J': Result := 'long';   // long integer
  'S': Result := 'short';  // signed short
  'Z': Result := 'boolean';// true or false
  'V': Result := 'void';
  'L':
    begin
      i := Pos(';', desc);
      Result := Copy(desc, 2, i - 2);
    end;
  '[':
    begin
      Delete(desc, 1, 1);
      Result := ReadType(desc) + '[]';
    end;
  end;
  if c <> '[' then
    Delete(desc, 1, i);
end;

function ResolveBaseType(t: TBaseDeclType): TBaseDeclType;
begin
  if t is TArrayDeclType then
    Result := ResolveBaseType(TBaseDeclType(t.FDepends[0]))
  else
    Result := t;
end;

function DepCount(Item1, Item2: Pointer): Integer;
begin
  Result := TBaseDeclType(Item1).FRealDependances.Count
          - TBaseDeclType(Item2).FRealDependances.Count
end;

function SortByPrintOrder(Item1, Item2: Pointer): Integer;
begin
  Result := TBaseDeclType(Item1).printOrder - TBaseDeclType(Item2).printOrder
end;

{ TBuiltInDeclType }

constructor TBuiltInDeclType.Create(Parent: TDeclTypes; Name, AOrigName: string);
begin
  inherited;
  FState := stDone;
  FAccessType := acPublic;
end;

procedure TBuiltInDeclType.Print(DestStream: TStream; indent: Integer;
  MaxAccessType: TAccessType);
begin
  // DO NOTHING
end;

{ TDeclTypes }

procedure TDeclTypes.ClearItems;

  procedure ClearList(l: TFPList);
  var
    i: Integer;
  begin
    for i := 0 to l.Count - 1 do
      TObject(l[i]).Free;
    l.Clear;
  end;

begin
  ClearList(FItems);
  ClearList(FFakeTypes);
end;

function TDeclTypes.GetItems(Index: Integer): TBaseDeclType;
begin
  Result := TBaseDeclType(FItems[Index])
end;

procedure TDeclTypes.InitPredefined;

  procedure AddPredef(AName, OrigName: string);
  var
    typ: TBuiltInDeclType;
  begin
    typ := TBuiltInDeclType.Create(Self, AName, OrigName);
    FItems.Add(typ);
    FJavaTypes.AddObject(OrigName, typ);
  end;

begin
  AddPredef('jbyte',    'byte');
  AddPredef('jchar',    'char');
  AddPredef('jdouble',  'double');
  AddPredef('jfloat',   'float');
  AddPredef('jint',     'int');
  AddPredef('jlong',    'long');
  AddPredef('jshort',   'short');
  AddPredef('jboolean', 'boolean');
  AddPredef('',         'void');
  AddPredef('Arr1jbyte',    'byte[]');
  AddPredef('Arr1jchar',    'char[]');
  AddPredef('Arr1jdouble',  'double[]');
  AddPredef('Arr1jfloat',   'float[]');
  AddPredef('Arr1jint',     'int[]');
  AddPredef('Arr1jlong',    'long[]');
  AddPredef('Arr1jshort',   'short[]');
  AddPredef('Arr1jboolean', 'boolean[]');
  AddPredef('Arr2jbyte',    'byte[][]');
  AddPredef('Arr2jchar',    'char[][]');
  AddPredef('Arr2jdouble',  'double[][]');
  AddPredef('Arr2jfloat',   'float[][]');
  AddPredef('Arr2jint',     'int[][]');
  AddPredef('Arr2jlong',    'long[][]');
  AddPredef('Arr2jshort',   'short[][]');
  AddPredef('Arr2jboolean', 'boolean[][]');
  AddPredef('Arr3jchar',    'char[][][]');
end;

function TDeclTypes.JavaClassPascalName(JavaName: string): string;
begin
  Result := '';
  if Pos('$', JavaName) > 0 then
  begin
    Result := 'Inner' + Copy(JavaName, RPos('$', JavaName) + 1, MaxInt);
    //Result := JavaClassPascalName(Copy(JavaName, 1, Pos('$', JavaName) - 1));
    //Delete(JavaName, 1, Pos('$', JavaName));
    //Result := Result + '_' + JavaClassPascalName(JavaName);
  end else begin
    while Pos('/', JavaName) > 0 do
    begin
      Result := Result + upcase(JavaName[1]);
      Delete(JavaName, 1, Pos('/', JavaName));
    end;
    Result := Result + JavaName;
  end;
end;

constructor TDeclTypes.Create;
begin
  FItems := TFPList.Create;
  FFakeTypes := TFPList.Create;
  FJavaTypes := TStringList.Create;
  FJavaTypes.CaseSensitive := True;
  FJavaTypes.Sorted := True;
  FJavaTypes.Duplicates := dupError;
  FPrintOrder := TFPList.Create;
  FNeedPredecl := TFPList.Create;
  FInheritanceOrder := TFPList.Create;
  InitPredefined;
end;

destructor TDeclTypes.Destroy;
begin
  FInheritanceOrder.Free;
  FPrintOrder.Free;
  FNeedPredecl.Free;
  ClearItems;
  FItems.Free;
  FFakeTypes.Free;
  FJavaTypes.Free;
  inherited Destroy;
end;

procedure TDeclTypes.Clear;
begin
  FStat.classes := 0;
  FStat.interfaces := 0;
  FStat.arrays := 0;
  FPrintOrder.Clear;
  FNeedPredecl.Clear;
  FJavaTypes.Clear;
  ClearItems;
  InitPredefined;
end;

function TDeclTypes.Count: Integer;
begin
  Result := FItems.Count;
end;

procedure TDeclTypes.Add(JavaClass: TJavaClassFile);
var
  t: TInterfaceDeclType;
begin
  if JavaClass.IsInterface then
  begin
    t := TInterfaceDeclType.Create(Self, JavaClassPascalName(JavaClass.GetClassName), JavaClass);
    Inc(FStat.interfaces);
  end else begin
    t := TClassDeclType.Create(Self, JavaClassPascalName(JavaClass.GetClassName), JavaClass);
    Inc(FStat.classes);
  end;
  FItems.Add(t);
  FJavaTypes.AddObject(t.FOrigName, t);
  t.Parse;
  t.FState := stDone;
  FPrintOrder.Add(t);
end;

procedure TDeclTypes.AddFake(OrigName: string);
var
  t: TBaseDeclType;
begin
  t := TBaseDeclType.Create(Self, JavaClassPascalName(OrigName), OrigName);
  t.FState := stDone;
  FFakeTypes.Add(t);
  FJavaTypes.AddObject(OrigName, t);
end;

function TDeclTypes.AddArray(ElementType: TBaseDeclType): TBaseDeclType;
var
  s: string;
  t: TBaseDeclType;
begin
  s := ElementType.FName;
  if (Copy(s, 1, 3) = 'Arr') and (Length(s) > 3) and (s[4] in ['1'..'8']) then
  begin
    Inc(s[4]);
//    Delete(s, 1, 3);
  end else
    s := 'Arr1' + s;
  Result := TArrayDeclType.Create(Self, s, ElementType);
  Inc(FStat.arrays);
  FItems.Add(Result);
  FJavaTypes.AddObject(Result.OrigName, Result);
  t := ElementType;
  while t is TArrayDeclType do
    t := TArrayDeclType(t).FElemType;
  if t is TInterfaceDeclType then
  begin
    TInterfaceDeclType(t).FArrayTypes.Add(Result);
    if TInterfaceDeclType(t).IsInner then
      TArrayDeclType(Result).FDeclaredIn := TInterfaceDeclType(t).FParentClass;
  end;
  FPrintOrder.Add(Result);
end;

function TDeclTypes.Find(OrigName: string; Strict: Boolean): TBaseDeclType;
var
  i: Integer;
  s: string;
  Failed: Boolean;
begin
  i := Pos('$', OrigName);
  if i > 0 then
  begin
    s := Copy(OrigName, 1, i - 1);
    Find(s, True)
  end;

  i := FJavaTypes.IndexOf(OrigName);
  if i < 0 then
  begin
    Result := nil;
    if RightStr(OrigName, 2) = '[]' then
    begin
      Result := Find(Copy(OrigName, 1, Length(OrigName) - 2), Strict);
      i := FJavaTypes.IndexOf(OrigName);
      if i >= 0 then
        Result := TBaseDeclType(FJavaTypes.Objects[i])
      else
      if Assigned(Result) then
        Result := AddArray(Result);
    end;
    if Strict and (Result = nil) and Assigned(FOnClassNotFound) then
    begin
      Failed := True;
      FOnClassNotFound(Self, OrigName, Failed);
      if Failed then
        raise Exception.Create('class not found: ' + OrigName)
      else
        Result := Find(OrigName, False)
    end;
  end else begin
    Result := TBaseDeclType(FJavaTypes.Objects[i]);
    if Result.FState = stNone then
    begin
      if Pos('$', OrigName) > 0 then
        OrigName := OrigName;
      Result.FState := stNeedPredeclare;
      FNeedPredecl.Add(Result);
    end;
  end;
end;

procedure TDeclTypes.PrintAllPredeclarations(fout: TStream);
var
  EmptyLine: Boolean;
  i: Integer;
begin
  EmptyLine := False;
  for i := 0 to FItems.Count - 1 do
  begin
    if TBaseDeclType(FItems[i]).killed then Continue;
    if TObject(FItems[i]) is TInterfaceDeclType then
      with TInterfaceDeclType(FItems[i]) do
        if not IsInner then
        begin
          PrintPredecl(fout, 2);
          EmptyLine := True;
        end;
  end;
  if EmptyLine then
    fout.Write(sLineBreak, Length(sLineBreak))
end;

procedure TDeclTypes.PrintNeededPredeclarations(fout: TStream);
var
  i: Integer;
  EmptyLine: Boolean;
begin
  EmptyLine := False;
  for i := 0 to FNeedPredecl.Count - 1 do
  begin
    if TObject(FNeedPredecl[i]) is TInterfaceDeclType then
      with TInterfaceDeclType(FNeedPredecl[i]) do
        if not IsInner then
        begin
          PrintPredecl(fout, 2);
          EmptyLine := True;
        end;
  end;
  for i := FNeedPredecl.Count - 1 downto 0 do
    if (TObject(FNeedPredecl[i]) is TInterfaceDeclType)
    and not TInterfaceDeclType(FNeedPredecl[i]).IsInner then
      FNeedPredecl.Delete(i);
  if EmptyLine then
    fout.Write(sLineBreak, Length(sLineBreak))
end;

procedure TDeclTypes.PrintArrays(fout: TStream);
var
  i, j: Integer;
  EmptyLine: Boolean;
begin
  EmptyLine := False;
  for i := 0 to FItems.Count - 1 do
    if TBaseDeclType(FItems[i]).killed then
      Continue
    else
    if TObject(FItems[i]) is TInterfaceDeclType then
      with TInterfaceDeclType(FItems[i]) do
        if not IsInner then
          for j := 0 to FArrayTypes.Count - 1 do
          begin
            TArrayDeclType(FArrayTypes[j]).Print(fout, 2, acPrivate);
            EmptyLine := True;
          end;
  if EmptyLine then
    fout.Write(sLineBreak, Length(sLineBreak))
end;

procedure TDeclTypes.PrintDeclarations(fout: TStream;
  MaxAccessType: TAccessType);
var
  t: TBaseDeclType;
  s: string;
  i: Integer;
begin
  for i := 0 to FPrintOrder.Count - 1 do
  begin
    t := TBaseDeclType(FPrintOrder[i]);
    if t is TArrayDeclType then Continue;
    if Pos('$', t.OrigName) > 0 then
      Continue; // inner classes will be printed by owners
    t.Print(fout, 2, MaxAccessType);
  end;
end;

procedure TDeclTypes.PrintDeclarationsInheritanceOrder(fout: TStream;
  MaxAccessType: TAccessType);

  function FindNestedClassToKill(cl: TInterfaceDeclType; st: TFPList): TInterfaceDeclType;
  var
    i: Integer;
    ct: TInterfaceDeclType;
  begin
    if st.IndexOf(cl) > 0 then
    begin
      Result := nil;
      for i := 0 to st.Count - 1 do
        with TInterfaceDeclType(st[i]) do
          if IsInner and (FParentClass.FRealDependances.Count = 1) then
          begin
            Result := TInterfaceDeclType(st[i]);
            Break;
          end;
      Exit;
    end;
    st.Insert(0, cl);
    for i := 0 to cl.FInnerClasses.Count - 1 do
    begin
      ct := TInterfaceDeclType(cl.FInnerClasses[i]);
      if ct.killed or (ct.printOrder >= 0) then Continue;
      Result := FindNestedClassToKill(ct, st);
      if Result <> nil then Exit;
    end;
    if (cl is TClassDeclType)
    and Assigned(TClassDeclType(cl).FSuperClass)
    and (TClassDeclType(cl).FSuperClass.printOrder < 0) then
    begin
      Result := FindNestedClassToKill(TClassDeclType(cl).FSuperClass, st);
      if Result <> nil then Exit;
    end;
    for i := 0 to cl.FInterfaces.Count - 1 do
    begin
      ct := TInterfaceDeclType(cl.FInterfaces[i]);
      if ct.printOrder >= 0 then Continue;
      Result := FindNestedClassToKill(ct, st);
      if Result <> nil then Exit;
    end;
  end;

var
  t, t1: TBaseDeclType;
  ti: TInterfaceDeclType;
  s: string;
  i, j: Integer;
  depCnt: TFPList;
  st: TFPList;
begin
  //WriteLn; Writeln;
  FPrintOrder.Clear;
  depCnt := TFPList.Create;
  for i := 0 to FItems.Count - 1 do
  begin
    t := TBaseDeclType(FItems[i]);
    if t is TInterfaceDeclType then
    begin
      if t.OrigName = 'java/lang/ThreadGroup' then
        writeln;
      t.printOrder := -1;
      t.killed := False;
      t.FRealDependances.Clear;

      if (t is TClassDeclType)
      and Assigned(TClassDeclType(t).FSuperClass) then
        with TClassDeclType(t) do
          if not IsInner and FSuperClass.IsInner
          and (FSuperClass.FParentClass <> t) then
            t.FRealDependances.Add(FSuperClass.FParentClass)
          else
          if IsInner and (FSuperClass <> FParentClass) or not IsInner then
            t.FRealDependances.Add(FSuperClass);
      with TInterfaceDeclType(t) do
      begin
        for j := 0 to FInterfaces.Count - 1 do
          if not IsInner and TInterfaceDeclType(FInterfaces[j]).IsInner
          and (TInterfaceDeclType(FInterfaces[j]).FParentClass <> t) then
            t.FRealDependances.Add(TInterfaceDeclType(FInterfaces[j]).FParentClass)
          else
            t.FRealDependances.Add(FInterfaces[j]);
        for j := 0 to FInnerClasses.Count - 1 do
          t.FRealDependances.Add(FInnerClasses[j]);
      end;
      if not TInterfaceDeclType(t).IsInner then
      for j := 0 to t.FDepends.Count - 1 do
      begin
        t1 := ResolveBaseType(TBaseDeclType(t.FDepends[j]));
        if (t1 is TInterfaceDeclType)
        and TInterfaceDeclType(t1).IsInner then
        begin
          t1 := TInterfaceDeclType(t1).FParentClass;
          if (t1 <> t) and (t.FRealDependances.IndexOf(t1) < 0) then
            t.FRealDependances.Add(t1);
        end;
      end;
      if not t.killed then
        depCnt.Add(t)
      else
        for j := 0 to depCnt.Count - 1 do
          TBaseDeclType(depCnt[j]).FRealDependances.Remove(t);
    end;
  end;
  depCnt.Sort(@DepCount);
  while depCnt.Count > 0 do
  begin
    ti := TInterfaceDeclType(depCnt[0]);
    depCnt.Delete(0);

    Write(ti.FRealDependances.Count, #9, ti.OrigName);
    if (ti is TClassDeclType)
    and (TClassDeclType(ti).FSuperClass <> nil) then
      Write(#9, '(',TClassDeclType(ti).FSuperClass.OrigName,
      '[',TClassDeclType(ti).FSuperClass.printOrder,']',')');
    for i := 0 to ti.FInterfaces.Count - 1 do
      Write(#9, TBaseDeclType(ti.FInterfaces[i]).OrigName,
      '[',TBaseDeclType(ti.FInterfaces[i]).printOrder,']');
    for i := 0 to ti.FInnerClasses.Count - 1 do
    begin
      if (TObject(ti.FInnerClasses[i]) is TClassDeclType)
      and Assigned(TClassDeclType(ti.FInnerClasses[i]).FSuperClass)
      and (TClassDeclType(ti.FInnerClasses[i]).FSuperClass <> ti) then
        Write(#9, ':', TClassDeclType(ti.FInnerClasses[i]).FSuperClass.OrigName,
        '[',TClassDeclType(ti.FInnerClasses[i]).FSuperClass.printOrder,']');
    end;
    WriteLn;

    if ti.FRealDependances.Count = 1 then
    begin
      // try to fix circular dependance:
      // kill some innerclass
      st := TFPList.Create;
      t := FindNestedClassToKill(ti, st);
      st.Free;
      depCnt.Add(ti);
      for i := depCnt.Count - 2 downto 0 do
        if TBaseDeclType(depCnt[i]) = t then
          depCnt.Delete(i)
        else
          TBaseDeclType(depCnt[i]).FRealDependances.Remove(t);

      depCnt.Sort(@DepCount);

      t.killed := True;
      WriteLn('Killed: ', t.OrigName);
      Continue;
    end;

    if ti.FRealDependances.Count = 0 then
    begin
      //if p^.typedecl.FOrigName = 'java/lang/ref/Reference' then
      //  writeln;
      ti.printOrder := FPrintOrder.Add(ti);
    end else begin
      //raise Exception.Create('circular inheritance dependence');
      WriteLn(StringOfChar('=', 15));
      Break;
    end;

    for i := 0 to depCnt.Count - 1 do
      TInterfaceDeclType(depCnt[i]).FRealDependances.Remove(ti);
    depCnt.Sort(@DepCount);
  end;
  for i := 0 to depCnt.Count - 1 do
  begin
    ti := TInterfaceDeclType(depCnt[i]);
    if ti.OrigName='java/lang/ThreadLocal' then
      writeln;
    Write(ti.FRealDependances.Count, #9, ti.OrigName,#9);
    if (ti is TClassDeclType)
    and (TClassDeclType(ti).FSuperClass <> nil) then
      Write(#9, '(',TClassDeclType(ti).FSuperClass.OrigName,
      '[',TClassDeclType(ti).FSuperClass.printOrder,']',')');
    for j := 0 to ti.FInterfaces.Count - 1 do
      Write(#9, TBaseDeclType(ti.FInterfaces[j]).OrigName,
      '[',TBaseDeclType(ti.FInterfaces[j]).printOrder, ']');
    with ti.FInnerClasses do
      for j := 0 to Count - 1 do
        if (TObject(Items[j]) is TClassDeclType)
        and Assigned(TClassDeclType(Items[j]).FSuperClass)
        and (TClassDeclType(Items[j]).FSuperClass <> ti) then
          Write(#9, ':', TClassDeclType(Items[j]).FSuperClass.OrigName,
          '[',TClassDeclType(Items[j]).FSuperClass.printOrder,']');
    WriteLn;
  end;
  depCnt.Free;
  for i := 0 to FPrintOrder.Count - 1 do
  begin
    t := TBaseDeclType(FPrintOrder[i]);
    if Pos('$', t.FOrigName) = 0 then
      t.Print(fout, 2, MaxAccessType)
  end;
end;

{ TInterfaceDeclType }

function TInterfaceDeclType.AncestorHasField(FieldName: string): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to FInterfaces.Count - 1 do
    if TInterfaceDeclType(FInterfaces[i]).HasField(FieldName) then
      Exit;
  Result := False;
end;

procedure TInterfaceDeclType.CompleteToFullHeader(var s: string);
var
  i: Integer;
  p, c: string;
begin
  if IsFinal then s := s + ' sealed';
  i := RPos('/', FOrigName);
  p := StringReplace(Copy(FOrigName, 1, i - 1), '/', '.', [rfReplaceAll]);
  c := Copy(FOrigName, i + 1, MaxInt);
  if Pos('$', c) > 0 then c := FLocalName;
  s := s + ' external ''' + p + ''' name ''' + c + '''';
  s := s + Inheritance;
end;

procedure TInterfaceDeclType.EnsureSectionPrinted(DestStream: TStream;
  indent: Integer; Sec: TAccessType);
begin
  // interfaces do not have visibility sections
end;

function TInterfaceDeclType.Find(AOrigName: string;
  Strict: Boolean): TBaseDeclType;
begin
  if FOrigName = AOrigName then
    Result := Self
  else
    Result := FParent.Find(AOrigName, Strict)
end;

function TInterfaceDeclType.Header(Full: Boolean): string;
begin
  Result := FName + ' = interface';
  if Full then
    CompleteToFullHeader(Result);
end;

function TInterfaceDeclType.HasField(FieldName: string): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to High(FFields) do
    if FFields[i].FieldName = FieldName then
      Exit;
  Result := AncestorHasField(FieldName);
end;

function TInterfaceDeclType.Inheritance: string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to FInterfaces.Count - 1 do
  begin
    if Result <> '' then
      Result := Result + ', ';
    Result := Result + TBaseDeclType(FInterfaces[i]).GetName(Self)
  end;
  if Result <> '' then Result := ' (' + Result + ')';
end;

function TInterfaceDeclType.MethodModifiers(m: TMethodDecl): string;
begin
  Result := ' overload;';
end;

procedure TInterfaceDeclType.FixInnerName;

  function ExistsInnerClass(t: TInterfaceDeclType): Boolean;
  var
    i: Integer;
  begin
    Result := True;
    for i := 0 to t.FInnerClasses.Count - 1 do
      if TInterfaceDeclType(t.FInnerClasses[i]).FName = FName then
        Exit;
    Result := False;
  end;

var
  t, t1: TInterfaceDeclType;
  i: Integer;
  needFix: Boolean;
begin
  t := FParentClass;
  needFix := False;
  while t <> nil do
  begin
    if (t <> FParentClass) and ExistsInnerClass(t) then
    begin
      needFix := True;
      Break;
    end;
    if (t is TClassDeclType)
    and Assigned(TClassDeclType(t).FSuperClass)
    and ExistsInnerClass(TClassDeclType(t).FSuperClass) then
    begin
      needFix := True;
      Break;
    end;
    for i := 0 to t.FInterfaces.Count - 1 do
      if ExistsInnerClass(TInterfaceDeclType(t.FInterfaces[i])) then
      begin
        needFix := True;
        Break;
      end;
    if needFix then Break;
    t := t.FParentClass;
  end;
  if needFix then
  begin
    if FName[Length(FName)] in ['0'..'8'] then
      FName[Length(FName)] := Succ(FName[Length(FName)])
    else
      FName := FName + '2';
    FixInnerName;
  end;
end;

procedure TInterfaceDeclType.Parse;
var
  i, k: Integer;
  m_acc: Word;
  s, m_desc, loc_name: string;
  sl: TStrings;
  t: TInterfaceDeclType;
begin
  // parse interfaces and methods
  for i := 0 to FJavaClass.GetInterfacesCount - 1 do
    FInterfaces.Add(Find(FJavaClass.GetInterfaceName(i), True));

  { nested (inner) classes }
  for i := 0 to FJavaClass.GetInnerClassesCount - 1 do
  begin
    s := FJavaClass.GetInnerClassOuterClassName(i);
    if s = FOrigName then
    begin
      s := FJavaClass.GetInnerClassName(i);
      loc_name := FJavaClass.GetInnerClassLocalName(i);
      if loc_name <> '' then
      begin
        t := TInterfaceDeclType(Find(s, True)); // ! нельзя полностью парсить классы!
        t.FLocalName := loc_name;
        FInnerClasses.Add(t);
        t.FParentClass := Self;
        t.FixInnerName;
      end;
    end;
  end;

  //FParent.FInheritanceOrder.Add(Self);

  SetLength(FMethods, FJavaClass.GetMethodCount);
  for i := 0 to High(FMethods) do
    with FMethods[i] do
    begin
      m_acc := FJavaClass.GetMethodAccess(i);
      if m_acc and ACC_PUBLIC <> 0 then Access := acPublic else
      if m_acc and ACC_PROTECTED <> 0 then Access := acProtected else
      if m_acc and ACC_PRIVATE <> 0 then Access := acPrivate;
      IsFinal := m_acc and ACC_FINAL <> 0;
      IsStatic := m_acc and ACC_STATIC <> 0;
      IsAbstract := m_acc and ACC_ABSTRACT <> 0;
      IsVarArgs := m_acc and ACC_VARARGS <> 0;
      MethodName := FJavaClass.GetMethodName(i);
      m_desc := FJavaClass.GetMethodDescriptor(i);
      if m_desc[1] <> '(' then
        raise Exception.Create('Incorrect method descriptor: "' + m_desc + '"');
      Delete(m_desc, 1, 1);
      k := 0;
      while m_desc[1] <> ')' do
      begin
        s := ReadType(m_desc);
        SetLength(Params, k + 1);
        Params[k].ParamType := Find(s, True);
        AddDependance(Params[k].ParamType);
        while Pos('/', s) > 0 do
          Delete(s, 1, Pos('/', s)); // todo: leave all capital letters, not only first
        Params[k].ParamName := LowerCase(s[1]) + IntToStr(k + 1);
        Inc(k);
      end;
      Delete(m_desc, 1, 1);
      ResultType := Find(ReadType(m_desc), True);
      sl := FJavaClass.GetMethodExceptions(i);
      if sl <> nil then
        try
          SetLength(Exceptions, sl.Count);
          for k := 0 to sl.Count - 1 do
            Exceptions[k] := sl[k];
        finally
          sl.Free;
        end;
    end;
  { fields }
  ParseFields;
end;

procedure TInterfaceDeclType.ParseFields;
var
  i: Integer;
  f_desc: string;
  f_acc: Word;
begin
  // parse fields
  SetLength(FFields, FJavaClass.GetFieldCount);
  for i := 0 to High(FFields) do
    with FFields[i] do
    begin
      f_acc := FJavaClass.GetFieldAccess(i);
      if f_acc and ACC_PUBLIC <> 0 then Access := acPublic else
      if f_acc and ACC_PROTECTED <> 0 then Access := acProtected else
      if f_acc and ACC_PRIVATE <> 0 then Access := acPrivate;
      IsFinal := f_acc and ACC_FINAL <> 0;
      IsStatic := f_acc and ACC_STATIC <> 0;
      FieldName := FJavaClass.GetFieldName(i);
      f_desc := FJavaClass.GetFieldDescriptor(i);
      if IsStatic and IsFinal then
        ConstVal := FJavaClass.GetFieldConstValue(i);
      FieldType := Find(ReadType(f_desc), True);
      AddDependance(FieldType);
    end;
end;

procedure TInterfaceDeclType.PrintHeader(DestStream: TStream; indent: Integer);
var s: string;
begin
  s := Space(indent) + Header(True) + sLineBreak;
  DestStream.WriteBuffer(s[1], Length(s));
end;

procedure TInterfaceDeclType.PrintBody(DestStream: TStream; indent: Integer;
  MaxAccessType: TAccessType);
begin
  PrintConstants(DestStream, indent, MaxAccessType);
  PrintInnerClasses(DestStream, indent, MaxAccessType);
  PrintMethods(DestStream, indent, MaxAccessType);
end;

procedure TInterfaceDeclType.PrintInnerClasses(DestStream: TStream;
  indent: Integer; MaxAccessType: TAccessType);
var
  i, j: Integer;
  t: TBaseDeclType;
  s: string;
  type_printed: Boolean;
begin
  type_printed := False;
  { inner classes: predeclarations }
  for i := 0 to FInnerClasses.Count - 1 do
  begin
    t := TBaseDeclType(FInnerClasses[i]);
    if t.killed then Continue;
    if t.FAccessType > MaxAccessType then Continue;
    //if FParent.FNeedPredecl.IndexOf(t) >= 0 then
    begin
      EnsureSectionPrinted(DestStream, indent, t.FAccessType);
      if not type_printed then
      begin
        s := Space(indent + 2) + 'type' + sLineBreak;
        DestStream.WriteBuffer(s[1], Length(s));
        type_printed := True;
      end;
      TInterfaceDeclType(t).PrintPredecl(DestStream, indent + 4);
      { arrays }
      with TInterfaceDeclType(t).FArrayTypes do
        for j := 0 to Count - 1 do
          TArrayDeclType(Items[j]).Print(DestStream, indent + 4, MaxAccessType);
    end;
    DestStream.WriteBuffer(sLineBreak, Length(sLineBreak));
  end;
  { inner classes: declarations }
  FInnerClasses.Sort(@SortByPrintOrder);
  for i := 0 to FInnerClasses.Count - 1 do
  begin
    t := TBaseDeclType(FInnerClasses[i]);
    if t.killed then Continue;
    if t.FAccessType > MaxAccessType then Continue;
    EnsureSectionPrinted(DestStream, indent, t.FAccessType);
    if not type_printed then
    begin
      s := Space(indent + 2) + 'type' + sLineBreak;
      DestStream.WriteBuffer(s[1], Length(s));
      type_printed := True;
    end;
    t.Print(DestStream, indent + 4, MaxAccessType);
  end;
end;

procedure TInterfaceDeclType.PrintConstants(DestStream: TStream;
  indent: Integer; MaxAccessType: TAccessType);
var
  const_printed: Boolean;
  c: TFieldDecl;
  i, k: Integer;
  s, s1, s2: string;
begin
  const_printed := False;
  s := '';
  k := 0;
  for i := 0 to High(FFields) do
  begin
    c := FFields[i];
    if c.Access > MaxAccessType then Continue;
    if c.IsStatic and c.IsFinal and (c.ConstVal <> '') then
    begin
      EnsureSectionPrinted(DestStream, indent, c.Access);
      if sec_clear or not const_printed then
      begin
        s := Space(indent + 2) + 'const' + sLineBreak;
        DestStream.WriteBuffer(s[1], Length(s));
        const_printed := True;
        sec_clear := False;
      end;
      s := {s + }Space(indent + 4);
      if AncestorHasField(c.FieldName) then
      begin
        s := s + '//';
      end else
        Inc(k);
      s := s + FixName(c.FieldName) + ' = ';
      if c.FieldType.FOrigName = 'java/lang/String' then
      begin
        s1 := c.ConstVal;
        while Length(s1) > 60 do
        begin
          s2 := Copy(c.ConstVal, 1, 60);
          s := s + '''' + StringReplace(s2, '''', '''''', [rfReplaceAll]) + '''  +'
            + sLineBreak + Space(indent + 4 + 2);
          Delete(s1, 1, 60);
        end;
        s := s + '''' + StringReplace(s1, '''', '''''', [rfReplaceAll]) + ''';'
      end else
      if c.FieldType.FOrigName <> 'int' then
        s := s + c.FieldType.GetName(Self) + '(' + c.ConstVal + ');'
      else
        s := s + c.ConstVal + ';';
      s := s + sLineBreak;
      DestStream.WriteBuffer(s[1], Length(s));
    end;
  end;
{  if s <> '' then
  begin
    if k = 0 then s := '//' + s;
    DestStream.WriteBuffer(s[1], Length(s));
  end;}
end;

procedure TInterfaceDeclType.PrintMethods(DestStream: TStream; indent: Integer;
  MaxAccessType: TAccessType);
var
  i, j: Integer;
  m: TMethodDecl;
  s, p, s1, t1, metName: string;
  IsProcedure: Boolean;
  sec: TAccessType;
  notAccess: Boolean;
  t: TBaseDeclType;
begin
  for sec := MaxAccessType downto acPublic do
    for i := 0 to High(FMethods) do
    begin
      m := FMethods[i];
      if m.Access <> sec then Continue;
      notAccess := False;
      for j := 0 to High(m.Params) do
        if m.Params[j].ParamType.FAccessType > MaxAccessType then
        begin
          notAccess := True;
          Break;
        end;
      if notAccess then Continue;
      if m.ResultType.FAccessType > MaxAccessType then Continue;

      EnsureSectionPrinted(DestStream, indent, m.Access);
      s := Space(indent + 2);

      // check for killed
      for j := 0 to High(m.Params) do
      begin
        t := ResolveBaseType(m.Params[j].ParamType);
        if t.killed
        {or (t is TInterfaceDeclType)
        and Assigned(TInterfaceDeclType(t).FParentClass)
        and (TInterfaceDeclType(t).FParentClass <> Self)} then
        begin
          s := s + '//';
          Break;
        end;
      end;
      t := ResolveBaseType(m.ResultType);
      if t.killed
      {or (t is TInterfaceDeclType)
      and Assigned(TInterfaceDeclType(t).FParentClass)
      and (TInterfaceDeclType(t).FParentClass <> Self)} then
        s := s + '//';

      // nested classes not supported for parsing
{      if (Pos('$', m.MethodName) > 0)
      or (Pos('$', m.ResultType.OrigName) > 0) then
        s := s + '//'
      else
      for j := 0 to High(m.Params) do
        if Pos('$', m.Params[j].ParamType.OrigName) > 0 then
        begin
          s := s + '//';
          Break;
        end;}

      if m.IsStatic then
        s := s + 'class ';
      IsProcedure := m.ResultType.OrigName = 'void';
      metName := FixName(m.MethodName);
      if m.MethodName = '<init>' then
        s := s + 'constructor Create'
      else
      if m.MethodName = '<clinit>' then
        Continue//s := s + 'constructor classCreate'
      else
      if IsProcedure then
        s := s + 'procedure ' + metName
      else
        s := s + 'function ' + metName;
      if Length(m.Params) > 0 then
      begin
        p := '';
        s := s + '(';
        s1 := '';
        for j := 0 to High(m.Params) do
          if s1 = '' then
          begin
            s1 := m.Params[j].ParamName;
            t1 := m.Params[j].ParamType.GetName(Self);
          end else
          if m.Params[j].ParamType.GetName(Self) = t1 then
            s1 := s1 + ', ' + m.Params[j].ParamName
          else begin
            if p <> '' then p := p + '; ';
            p := p + s1 + ': ' + t1;
            s1 := m.Params[j].ParamName;
            t1 := m.Params[j].ParamType.GetName(Self);
          end;
        if p <> '' then p := p + '; ';
        s := s + p + s1 + ': ' + t1 + ')';
      end;
      if not IsProcedure then
        s := s + ': ' + m.ResultType.GetName(Self);
      s := s + ';';
      if (metName[1] <> '&') and (m.MethodName <> metName) then
        s := s + ' external name ''' + m.MethodName + ''';';
      if m.IsStatic then
        s := s + ' static;';
      s := s + MethodModifiers(m);
      if Length(m.Exceptions) > 0 then
      begin
        s := s + '  // throws ';
        s1 := '';
        for j := 0 to High(m.Exceptions) do
        begin
          if s1 <> '' then
            s1 := s1 + ', ';
          s1 := s1 + StringReplace(m.Exceptions[j], '/', '.', [rfReplaceAll])
        end;
        s := s + s1;
      end;
      s := s + sLineBreak;
      DestStream.WriteBuffer(s[1], Length(s));
    end;
end;

constructor TInterfaceDeclType.Create(Parent: TDeclTypes; Name: string;
  JavaClass: TJavaClassFile);
var
  acc: Word;
begin
  inherited Create(Parent, Name, JavaClass.GetClassName);
  {$ifdef DEBUG}
  WriteLn(FOrigName);
  {$endif}
  FInterfaces := TFPList.Create;
  FInnerClasses := TFPList.Create;
  FArrayTypes := TFPList.Create;
  FJavaClass := JavaClass;
  acc := FJavaClass.GetAccess;
  if acc and ACC_PUBLIC <> 0 then FAccessType := acPublic else
  if acc and ACC_PROTECTED <> 0 then FAccessType := acProtected else
  if acc and ACC_PRIVATE <> 0 then FAccessType := acPrivate;
  IsAbstract := acc and ACC_ABSTRACT <> 0;
  IsFinal := acc and ACC_FINAL <> 0;
end;

destructor TInterfaceDeclType.Destroy;
begin
  FInterfaces.Free;
  FInnerClasses.Free;
  FArrayTypes.Free;
  inherited Destroy;
end;

function TInterfaceDeclType.GetName(forWhat: TBaseDeclType): string;
var
  f: Boolean;
  ti: TInterfaceDeclType;
begin
  if not IsInner then
    Result := inherited GetName(forWhat)
  else begin
    f := False;
    ti := FParentClass;
    while ti <> nil do
    begin
      if forWhat = ti then
      begin
        f := True;
        Break;
      end;
      ti := ti.FParentClass;
    end;
    if f then
      Result := FName
    else
      Result := FParentClass.GetName(Self) + '.' + FName;
  end;
end;

function TInterfaceDeclType.IsInner: Boolean;
begin
  Result := Assigned(FParentClass);
end;

function TInterfaceDeclType.HasCommonAncestorWith(cl: TBaseDeclType): Boolean;
var t1, t2: TInterfaceDeclType;
begin
  Result := False;
  if not (cl is TInterfaceDeclType) then Exit;
  t1 := Self;
  while t1 <> nil do
  begin
    t2 := TInterfaceDeclType(cl);
    while t2 <> nil do
    begin
      if t1 = t2 then
      begin
        Result := True;
        Exit;
      end;
      t2 := t2.FParentClass;
    end;
    t1 := t1.FParentClass;
  end;
end;

procedure TInterfaceDeclType.PrintPredecl(fout: TStream; Indent: Integer);
var
  s: string;
begin
  s := Space(Indent) + Header(False) + ';' + sLineBreak;
  fout.WriteBuffer(s[1], Length(s));
end;

procedure TInterfaceDeclType.Print(DestStream: TStream; indent: Integer;
  MaxAccessType: TAccessType);
var
  s: string;
  i, j: Integer;
  t: TBaseDeclType;
begin
  if IsInner and (FAccessType > MaxAccessType) then Exit;
  PrintHeader(DestStream, indent);
  sec_printed := False;
  sec_clear := False;
  PrintBody(DestStream, indent, MaxAccessType);
  s := Space(indent) + 'end;' + sLineBreak + sLineBreak;
  DestStream.WriteBuffer(s[1], Length(s));
end;

{ TClassDeclType }

function TClassDeclType.AncestorHasField(FieldName: string): Boolean;
begin
  if Assigned(FSuperClass) and FSuperClass.HasField(FieldName) then
    Result := True
  else
    Result := inherited AncestorHasField(FieldName);
end;

procedure TClassDeclType.CompleteToFullHeader(var s: string);
begin
  if IsAbstract then s := s + ' abstract';
  inherited CompleteToFullHeader(s);
end;

function TClassDeclType.Header(Full: Boolean): string;
begin
  Result := FName + ' = class';
  if Full then
    CompleteToFullHeader(Result);
end;

procedure TClassDeclType.Parse;
var s: string;
begin
  s := FJavaClass.GetSuperClassName;
  if s <> '' then
  begin
    FSuperClass := FParent.Find(s, True) as TInterfaceDeclType;
    AddDependance(FSuperClass);
  end;
  inherited Parse;
end;

function TClassDeclType.Inheritance: string;
begin
  Result := inherited Inheritance;
  if FSuperClass <> nil then
  begin
    if Result <> '' then
      Result := ', ' + Copy(Result, 3, Length(Result) - 3);
    Result := ' (' + FSuperClass.GetName(Self) + Result + ')';
  end;
end;

procedure TClassDeclType.EnsureSectionPrinted(DestStream: TStream;
  indent: Integer; Sec: TAccessType);
const
  SecNames: array [acPublic..acPrivate] of string =
    ('public', 'strict protected', 'strict private');
var
  s: string;
begin
  if not sec_printed or (sec_type_printed <> Sec) then
  begin
    sec_printed := True;
    sec_type_printed := Sec;
    s := Space(indent) + SecNames[Sec] + sLineBreak;
    DestStream.WriteBuffer(s[1], Length(s));
    sec_clear := True;
  end;
end;

function TClassDeclType.MethodModifiers(m: TMethodDecl): string;
begin
  Result := inherited MethodModifiers(m);
  if not IsFinal then
  begin
    if not m.IsStatic then
      Result := Result + ' virtual;';
    if m.IsFinal then
      Result := Result + ' final;'
    else
    if m.IsAbstract then
      Result := Result + ' abstract;'
  end;
end;

procedure TClassDeclType.PrintBody(DestStream: TStream; indent: Integer;
  MaxAccessType: TAccessType);
begin
  if FName = 'JLConditionalSpecialCasing' then
    writeln;
  PrintConstants(DestStream, indent, MaxAccessType);
  PrintInnerClasses(DestStream, indent, MaxAccessType);
  PrintFields(DestStream, indent, MaxAccessType);
  PrintMethods(DestStream, indent, MaxAccessType);
end;

procedure TClassDeclType.PrintFields(DestStream: TStream; indent: Integer;
  MaxAccessType: TAccessType);
const
  varSec: array [Boolean, Boolean] of string = (
    ('var', 'final var'),
    ('class var', 'final class var')
  );
var
  var_printed: Boolean;
  f: TFieldDecl;
  i: Integer;
  s: string;
  sec: TAccessType;
  classVar, finalVar: Boolean;
  t: TBaseDeclType;
begin
  // var, class var, final var, final class var
  for sec := MaxAccessType downto acPublic do
    for classVar := False to True do
      for finalVar := False to True do
      begin
        var_printed := False;
        for i := 0 to High(FFields) do
        begin
          f := FFields[i];
          if f.Access <> sec then Continue;
          if f.IsStatic <> classVar then Continue;
          if f.IsFinal <> finalVar then Continue;
          if f.IsStatic and f.IsFinal and (f.ConstVal <> '') then Continue;

          EnsureSectionPrinted(DestStream, indent, f.Access);
          if not var_printed then
          begin
            s := Space(indent + 2) + varSec[classVar, finalVar] + sLineBreak;
            DestStream.WriteBuffer(s[1], Length(s));
            var_printed := True;
          end;
          s := Space(indent + 4);

          if AncestorHasField(f.FieldName) then
            s := s + '//';

          t := ResolveBaseType(f.FieldType);
          if t is TInterfaceDeclType then
            with TInterfaceDeclType(t) do
              if (t.FAccessType > MaxAccessType)
              or Assigned(FParentClass) and (FParentClass <> Self) then
                s := s + '//';

          s := s + 'f' + StringReplace(f.FieldName, '$', '_', [rfReplaceAll]) + ': '
            + f.FieldType.GetName(Self) + '; external name ''' + f.FieldName + ''';'
            + sLineBreak;
          DestStream.WriteBuffer(s[1], Length(s));
        end;
      end;
end;

{ TArrayDeclType }

constructor TArrayDeclType.Create(Parent: TDeclTypes; Name: string;
  ElemType: TBaseDeclType);
begin
  inherited Create(Parent, Name, ElemType.FOrigName + '[]');
  FElemType := ElemType;
  FDepends.Add(ElemType);
  FAccessType := ElemType.FAccessType;
  FState := stDone;
end;

function TArrayDeclType.GetName(forWhat: TBaseDeclType): string;
var
  ti1: TInterfaceDeclType;
  f: Boolean;
begin
  if not Assigned(FDeclaredIn) then
    Result := inherited GetName(forWhat)
  else
  if FDeclaredIn.HasCommonAncestorWith(forWhat) then
    Result := FName
  else
    Result := FDeclaredIn.GetName(forWhat) + '.' + FName;
end;

procedure TArrayDeclType.Print(DestStream: TStream; indent: Integer;
  MaxAccessType: TAccessType);
var s: string;
begin
  if FAccessType > MaxAccessType then Exit;
  s := Space(indent) + FName + ' = array of ' + FElemType.FName + ';' + sLineBreak;
  DestStream.Write(s[1], Length(s));
end;

{ TBaseDeclType }

procedure TBaseDeclType.AddDependance(dep: TBaseDeclType);
begin
  if FDepends.IndexOf(dep) < 0 then FDepends.Add(dep)
end;

constructor TBaseDeclType.Create(Parent: TDeclTypes; Name, AOrigName: string);
begin
  FParent := Parent;
  FDepends := TFPList.Create;
  FRealDependances := TFPList.Create;
  FAccessType := acPrivate;
  FName := Name;
  FOrigName := AOrigName;
end;

destructor TBaseDeclType.Destroy;
begin
  FDepends.Free;
  FRealDependances.Free;
  inherited Destroy;
end;

function TBaseDeclType.GetName(forWhat: TBaseDeclType): string;
begin
  Result := FName;
end;

initialization
  FPCReserved := TStringList.Create;
  FPCReserved.CaseSensitive := False;
  FPCReserved.Sorted := True;
  FPCReserved.Text :=
    'absolute' + sLineBreak +
    'and' + sLineBreak +
    'array' + sLineBreak +
    'as' + sLineBreak +
    'asm' + sLineBreak +
    'begin' + sLineBreak +
    'case' + sLineBreak +
    'class' + sLineBreak +
    'const' + sLineBreak +
    'constructor' + sLineBreak +
    'destructor' + sLineBreak +
    'dispinterface' + sLineBreak +
    'dispose' + sLineBreak +
    'div' + sLineBreak +
    'do' + sLineBreak +
    'downto' + sLineBreak +
    'else' + sLineBreak +
    'end' + sLineBreak +
    'except' + sLineBreak +
    'exit' + sLineBreak +
    'exports' + sLineBreak +
    'false' + sLineBreak +
    'file' + sLineBreak +
    'finalization' + sLineBreak +
    'finally' + sLineBreak +
    'for' + sLineBreak +
    'function' + sLineBreak +
    'goto' + sLineBreak +
    'if' + sLineBreak +
    'implementation' + sLineBreak +
    'in' + sLineBreak +
    'initialization' + sLineBreak +
    'inherited' + sLineBreak +
    'inline' + sLineBreak +
    'interface' + sLineBreak +
    'is' + sLineBreak +
    'label' + sLineBreak +
    'library' + sLineBreak +
    'mod' + sLineBreak +
    'new' + sLineBreak +
    'nil' + sLineBreak +
    'not' + sLineBreak +
    'object' + sLineBreak +
    'of' + sLineBreak +
    'on' + sLineBreak +
    'operator' + sLineBreak +
    'or' + sLineBreak +
    'out' + sLineBreak +
    'packed' + sLineBreak +
    'procedure' + sLineBreak +
    'property' + sLineBreak +
    'program' + sLineBreak +
    'public' + sLineBreak +
    'raise' + sLineBreak +
    'record' + sLineBreak +
    'reintroduce' + sLineBreak +
    'repeat' + sLineBreak +
    'resourcestring' + sLineBreak +
    'self' + sLineBreak +
    'set' + sLineBreak +
    'shl' + sLineBreak +
    'shr' + sLineBreak +
    'string' + sLineBreak +
    'then' + sLineBreak +
    'threadvar' + sLineBreak +
    'to' + sLineBreak +
    'true' + sLineBreak +
    'try' + sLineBreak +
    'type' + sLineBreak +
    'unit' + sLineBreak +
    'until' + sLineBreak +
    'uses' + sLineBreak +
    'var' + sLineBreak +
    'while' + sLineBreak +
    'with' + sLineBreak +
    'xor';

finalization
  FPCReserved.Free;

end.

