unit UGriffHelper;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, ShellApi, Vcl.StdCtrls;

const
  BellowsWidth = 0.5;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
    procedure FormCreate(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

  THeader = record
    quarterNoteTicks: integer;
    measureNom: integer;
    measureDenom: integer;
    function GetChordTicks(fraction, dots: integer): integer;
    function TicksPerMeasure: integer;
  end;


var
  Form1: TForm1;

function InsertBellows(FileName: string): boolean;


implementation

{$R *.dfm}

uses
  UXmlNode, UXmlParser;


const
  NoteNames: array [0..7] of string =
    ('whole', 'half', 'quarter', 'eighth', '16th', '32nd', '64th', '128th');

procedure TForm1.FormCreate(Sender: TObject);
begin
  DragAcceptFiles(Self.Handle, true);
end;

procedure TForm1.WMDropFiles(var Msg: TWMDropFiles);
var
  DropH: HDROP;               // drop handle
  DroppedFileCount: Integer;  // number of files dropped
  FileNameLength: Integer;    // length of a dropped file name
  FileName: string;           // a dropped file name
  i: integer;
  ext: string;
begin
  inherited;

  DropH := Msg.Drop;
  try
    DroppedFileCount := DragQueryFile(DropH, $FFFFFFFF, nil, 0);
    if (DroppedFileCount > 0) then
    begin
      for i := 0 to DroppedFileCount-1 do
      begin
        FileNameLength := DragQueryFile(DropH, i, nil, 0);
        SetLength(FileName, FileNameLength);
        DragQueryFile(DropH, i, PChar(FileName), FileNameLength + 1);
        ext := ExtractFileExt(Filename);
        if (LowerCase(ext) = '.mscz') or
           (LowerCase(ext) = '.mscx') then
        begin
          InsertBellows(FileName);
        end;
      end;
    end;
  finally
    DragFinish(DropH);
  end;
  Msg.Result := 0;
end;

function GetFraction(const sLen: string): integer;
var
  idx: integer;
begin
  result := 128;
  for idx := High(NoteNames) downto 0 do
    if sLen = NoteNames[idx] then
      break
    else
      result := result shr 1;
end;

function GetChild(Name: string; var Child: KXmlNode; Parent: KXmlNode): boolean;
var
  k: integer;
begin
  k := 0;
  result := false;
  Child := nil;

  while not result and (Parent <> nil) and (k < Parent.Count) do
  begin
    Child := Parent.ChildNodes[k];
    inc(k);
    result := Child.Name = Name;
  end;
end;

function THeader.TicksPerMeasure: integer;
begin
  result := 4*quarterNoteTicks*measureNom div measureDenom;
end;

function THeader.GetChordTicks(fraction, dots: integer): integer;
var
  h: integer;
begin
  result := 4*quarterNoteTicks div fraction;
  h := result;
  while dots > 0 do
  begin
    h := h div 2;
    inc(result, h);
    dec(dots);
  end;
end;

procedure AddSubtype(TextLine: KXmlNode; Sub: string; Y: string);
var
  Segment, Child: KXmlNode;
begin
  Segment := TextLine.AppendChildNode('Segment');
  Segment.AppendChildNode('subtype', Sub);
  Child := Segment.AppendChildNode('offset');
  Child.AppendAttr('x', '0');
  Child.AppendAttr('y', Y);
  Child := Segment.AppendChildNode('off2');
  Child.AppendAttr('x', '0');
  Child.AppendAttr('y', '0');
end;

function InsertBellows(FileName: string): boolean;
var
  Root: KXmlNode;
  Score, Staff, Measure, Voice, Chord, Child, Child1, Child2: KXmlNode;
  NextSpanner: KXmlNode;
  Spanner, TextLine: KXmlNode;
  offset: integer;
  SpannerOffset: integer;
  Header: THeader;

  procedure SetHeader;
  var
    Child, Child1: KXmlNode;
  begin
    with Header do
    begin
      quarterNoteTicks := 120;
      measureNom := 4;
      measureDenom := 4;
    end;
    if GetChild('Division', Child, Score) then
      Header.quarterNoteTicks := StrToIntDef(Child.Value, 120);
    if GetChild('Measure', Child, Staff) and
       GetChild('voice', Child, Child) and
       GetChild('TimeSig', Child, Child) then
    begin
      if GetChild('sigN', Child1, Child) then
        Header.measureNom := StrToIntDef(Child1.Value, 4);
      if GetChild('sigD', Child1, Child) then
        Header.measureDenom := StrToIntDef(Child1.Value, 4);
    end;
  end;

  procedure SetPrevSpanner;
  var
    off, mes, fra, quot: integer;
    Child1, Child2: KXmlNode;
  begin
    if NextSpanner <> nil then
    begin
      off := offset - SpannerOffset;
      mes := off div Header.TicksPerMeasure;
      fra := off mod Header.TicksPerMeasure;
      fra := 8*fra div Header.quarterNoteTicks;  // 32nd
      Child1 := NextSpanner.AppendChildNode('next');
      Child1 := Child1.AppendChildNode('location');
      if mes > 0 then
      begin
        Child1.AppendChildNode('measures', IntToStr(mes));
      end;
      if fra > 0 then
      begin
        quot := 32;
        while not odd(fra) do
        begin
          quot := quot div 2;
          fra := fra div 2;
        end;
        Child1.AppendChildNode('fractions', IntToStr(fra) + '/' + IntToStr(quot));
      end;

      Child1 := Spanner.AppendChildNode('prev');
      Child1 := Child1.AppendChildNode('location');
      if mes > 0 then
      begin
        Child1.AppendChildNode('measures', IntToStr(-mes));
      end;
      if fra > 0 then
      begin
        Child1.AppendChildNode('fractions', IntToStr(-fra) + '/' + IntToStr(quot));
      end;
      NextSpanner := nil;
    end;
  end;

var
  IsPull: boolean;
  hasColor: boolean;
  duration, dots: integer;
  i, v, iStartMeasure, iStartChord: integer;
begin
  result := false;
  if not KXmlParser.ParseFile(FileName, Root) then
    exit;

  Score := Root.ChildNodes[Root.Count-1];
  if (Score.Name <> 'Score') or
     not GetChild('Staff', Staff, Score) then
  begin
    Application.MessageBox('Error in MuseScore file!', 'Error');
    exit;
  end;

  // remove all "spanner  with type textline"
  for i := 0 to Staff.Count-1 do
  begin
    Measure := Staff.ChildNodes[i];
    if Measure.Name = 'Measure' then
    begin
      Measure.DeleteAttribute('id');
      if GetChild('voice', Voice, Measure) then
      begin
        v := 0;
        while v < Voice.Count-1 do
        begin
          Child := Voice.ChildNodes[v];
          if (Child.Name = 'Spanner') and
             (Child.Attributes['type'] = 'TextLine')  then
            Voice.PurgeChild(v)
          else
            inc(v);
        end;
      end;
    end;
  end;

  SetHeader;
  IsPull := true;
  iStartMeasure := -1;
  iStartChord := 0;
  offset := 0;
  v := 0;
  for i := 0 to Staff.Count-1 do
  begin
    Measure := Staff.ChildNodes[i];
    if Measure.Name = 'Measure' then
    begin
      if GetChild('voice', Voice, Measure) then
      begin
        v := 0;
        while v < Voice.Count do
        begin
          Chord := Voice.ChildNodes[v];
          if GetChild('durationType', Child, Chord) then
          begin
            duration := GetFraction(Child.Value);
            dots := 0;
            if GetChild('dots', Child, Chord) then
              dots := StrToIntDef(Child.Value, 0);
            if (Chord.Name = 'Chord') and
               GetChild('Note', Child, Chord) then
            begin
              hasColor := GetChild('color', Child1, Child);
              if IsPull <> hasColor then
              begin
                IsPull := hasColor;
                Spanner := KXmlNode.Create;
                Spanner.Name := 'Spanner';
                Spanner.AppendAttr('type', 'TextLine');
                if not IsPull then
                begin
                  SpannerOffset := offset;
                  NextSpanner := Spanner;
                  Voice.InsertChildNode(v, Spanner);
                  TextLine := Spanner.AppendChildNode('TextLine');
                  TextLine.AppendChildNode('placement', 'below');
                  TextLine.AppendChildNode('lineWidth', Format('%g', [BellowsWidth]));
                  AddSubtype(TextLine, '0', '3.5');
                  AddSubtype(TextLine, '3', '3.5');
                end else begin
                  Voice.InsertChildNode(v, Spanner);
                  SetPrevSpanner;
                end;
              end; // IsPull <> hasColor
            end;
            inc(offset, Header.GetChordTicks(duration, dots));
          end;
          inc(v);
        end;
      end
    end;
  end;

  if NextSpanner <> nil then
  begin
    Spanner := KXmlNode.Create;
    Spanner.Name := 'Spanner';
    Spanner.AppendAttr('type', 'TextLine');

    if GetChild('voice', Voice, Staff.ChildNodes[Staff.Count-1]) then
    begin
      Voice.InsertChildNode(v, Spanner);

      SetPrevSpanner;
    end;
  end;

  SetLength(Filename, Length(FileName)-Length(ExtractFileExt(FileName)));
  result := Root.SaveToXmlFile(FileName + '_balg.mscx');
end;

end.


