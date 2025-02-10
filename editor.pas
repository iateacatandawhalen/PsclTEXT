unit Editor;

interface

uses
  SysUtils, Classes, SyntaxC, SyntaxPython, SyntaxMarkdown, SyntaxPascal;

type
  TEditor = class
  private
    FContent: TStringList;
    procedure LoadFile(const FileName: string);
    procedure SaveFile(const FileName: string);
    procedure HighlightLine(const Line: string; var HighlightedLine: string; const FileExtension: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure OpenFile(const FileName: string);
    procedure DisplayContent;
    procedure SaveContent(const FileName: string);
  end;

implementation

constructor TEditor.Create;
begin
  FContent := TStringList.Create;
end;

destructor TEditor.Destroy;
begin
  FContent.Free;
  inherited;
end;

procedure TEditor.LoadFile(const FileName: string);
begin
  if FileExists(FileName) then
    FContent.LoadFromFile(FileName);
end;

procedure TEditor.SaveFile(const FileName: string);
begin
  FContent.SaveToFile(FileName);
end;

procedure TEditor.HighlightLine(const Line: string; var HighlightedLine: string; const FileExtension: string);
begin
  if FileExtension = '.c' then
    SyntaxC.HighlightCSyntax(Line, HighlightedLine)
  else if FileExtension = '.py' then
    SyntaxPython.HighlightPythonSyntax(Line, HighlightedLine)
  else if FileExtension = '.md' then
    SyntaxMarkdown.HighlightMarkdownSyntax(Line, HighlightedLine)
  else if FileExtension = '.pas' then
    SyntaxPascal.HighlightPascalSyntax(Line, HighlightedLine)
  else
    HighlightedLine := Line;  // Default behavior for unsupported types
end;

procedure TEditor.OpenFile(const FileName: string);
var
  Line, HighlightedLine, FileExtension: string;
  i: Integer;
begin
  LoadFile(FileName);
  
  // Extract file extension
  FileExtension := ExtractFileExt(FileName);

  // Display highlighted content
  for i := 0 to FContent.Count - 1 do
  begin
    Line := FContent[i];
    HighlightedLine := '';
    HighlightLine(Line, HighlightedLine, FileExtension);
    WriteLn(HighlightedLine);  // Display the highlighted line
  end;
end;

procedure TEditor.DisplayContent;
begin
  WriteLn(FContent.Text);
end;

procedure TEditor.SaveContent(const FileName: string);
begin
  SaveFile(FileName);
end;

end.
