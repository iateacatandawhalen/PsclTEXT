program TextEditor;

uses crt, SysUtils, SyntaxMarkdown, SyntaxPython, SyntaxPascal;

var
  fileName: string;
  fileContents: Text;
  line: string;
  i: Integer;
  fileLines: array of string;
  currentLine: Integer;
  totalLines: Integer;
  markdownSyntaxFileExists: Boolean;
  pythonSyntaxFileExists: Boolean;
  pascalSyntaxFileExists: Boolean;
  fileExtension: string;
  isModified: Boolean;

procedure ReadFileContents(filename: string);
begin
  Assign(fileContents, filename);
  try
    Reset(fileContents);
    totalLines := 0;
    while not Eof(fileContents) do
    begin
      ReadLn(fileContents, line);
      SetLength(fileLines, totalLines + 1);
      fileLines[totalLines] := line;
      totalLines := totalLines + 1;
    end;
    Close(fileContents);
  except
    on E: EInOutError do
    begin
      WriteLn('Error: Could not read file (', E.Message, ').');
      Halt(1);
    end;
  end;
end;

procedure SaveFile(filename: string);
var
  i: Integer;
  fileOut: Text;
begin
  Assign(fileOut, filename);
  try
    Rewrite(fileOut);
    for i := 0 to totalLines - 1 do
      WriteLn(fileOut, fileLines[i]);
    Close(fileOut);
    isModified := False;
    WriteLn('File saved.');
  except
    on E: EInOutError do
      WriteLn('Error: Could not save file (', E.Message, ').');
  end;
end;

function FileExistsByName(const fileName: string): Boolean;
begin
  Result := FileExists(fileName);
end;

function GetFileExtension(const filename: string): string;
begin
  Result := LowerCase(ExtractFileExt(filename));  // Case-insensitive
end;

procedure EditFile(language: string);
var
  command: char;
  tempLine: string;
begin
  ClrScr;
  WriteLn('Editing Mode. Press F2 to save. Press ESC to exit without saving.');

  repeat
    // Prevent out-of-bounds error
    if (currentLine < 0) or (currentLine >= totalLines) then
      currentLine := 0;

    // Syntax highlighting based on file extension
    if language = 'markdown' then
      if markdownSyntaxFileExists then
        PrintMarkdownSyntaxHighlighting(fileLines[currentLine])
      else
        WriteLn(fileLines[currentLine])
    else if language = 'python' then
      if pythonSyntaxFileExists then
        PrintPythonSyntaxHighlighting(fileLines[currentLine])
      else
        WriteLn(fileLines[currentLine])
    else if language = 'pascal' then
      if pascalSyntaxFileExists then
        PrintPascalSyntaxHighlighting(fileLines[currentLine])
      else
        WriteLn(fileLines[currentLine])
    else
      WriteLn(fileLines[currentLine]); // Default for unknown formats

    command := ReadKey;

    case command of
      #72: // Up arrow
        if currentLine > 0 then Dec(currentLine);
      #80: // Down arrow
        if currentLine < totalLines - 1 then Inc(currentLine);
      #13: // Enter key (edit line)
        begin
          Write('Edit line: ');
          ReadLn(tempLine);
          fileLines[currentLine] := tempLine;
          isModified := True;
        end;
      #27: // Escape key (exit)
        begin
          if isModified then
          begin
            Write('Unsaved changes! Exit without saving? (Y/N): ');
            command := ReadKey;
            if command in ['y', 'Y'] then Exit;
          end
          else
            Exit;
        end;
    end;

    ClrScr;
  until command = #112; // F2 to save

  SaveFile(fileName);
end;

begin
  ClrScr;
  Write('Enter filename: ');
  ReadLn(fileName);

  if not FileExists(fileName) then
  begin
    WriteLn('File does not exist. Create new file? (Y/N)');
    if ReadKey in ['y', 'Y'] then
    begin
      Assign(fileContents, fileName);
      Rewrite(fileContents);
      Close(fileContents);
    end
    else
    begin
      WriteLn('Exiting...');
      Exit;
    end;
  end;

  ReadFileContents(fileName);

  // Determine file extension for syntax
  fileExtension := GetFileExtension(fileName);

  // Check if syntax files exist
  markdownSyntaxFileExists := FileExistsByName('SyntaxMarkdown.pas');
  pythonSyntaxFileExists := FileExistsByName('SyntaxPython.pas');
  pascalSyntaxFileExists := FileExistsByName('SyntaxPascal.pas');

  // Assign language
  if fileExtension = '.md' then
    EditFile('markdown')
  else if fileExtension = '.py' then
    EditFile('python')
  else if fileExtension = '.pas' then
    EditFile('pascal')
  else
  begin
    WriteLn('Unknown file type. Proceeding without syntax highlighting.');
    EditFile('default');
  end;

  WriteLn('Done. Press any key to exit.');
  ReadLn;
end.
