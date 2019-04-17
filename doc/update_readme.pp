program update_readme;

{$mode objfpc}{$H+}

uses
  Classes,
  SysUtils,
  IniFiles,
  GLPT;

type
  OS = record
    Name: string;
    win: string;
    linux: string;
    mac: string;
  end;

var
  source: TStringList;
  list: TStringList;
  readme: TStringList;
  i: integer;
  s: string;
  p, j: integer;
  api_list: array of OS;
  ini: TIniFile;
  glpt_file, readme_file, ini_file: string;

  function asIcon(status: string): string;
  begin
    case status of
      'Working': Result :=
          '<img src="https://github.com/daar/GLPT/blob/master/doc/green.svg">';
      'Not implemented': Result :=
          '<img src="https://github.com/daar/GLPT/blob/master/doc/red.svg">';
      'Partially implemented': Result :=
          '<img src="https://github.com/daar/GLPT/blob/master/doc/orange.svg">';
      'Not applicable': Result :=
          '<img src="https://github.com/daar/GLPT/blob/master/doc/gray.svg">';
    end;
  end;

begin
  if ParamCount <> 1 then
  begin
    writeln('error: need root path to the PMake source folder');
    halt(1);
  end;

  if not DirectoryExists(ParamStr(1)) then
  begin
    writeln('error: root path to the PMake source folder does not exist');
    halt(1);
  end
  else
    writeln('-- root folder found');

  glpt_file := IncludeTrailingPathDelimiter(ParamStr(1)) + 'GLPT.pas';
  if not(FileExists(glpt_file)) then
  begin
    writeln('error: cannot find GLPT.pas');
    halt(1);
  end
  else
    writeln('-- GLPT.pas found');

  readme_file := IncludeTrailingPathDelimiter(ParamStr(1)) + 'README.md';
  if not(FileExists(readme_file)) then
  begin
    writeln('error: cannot find README.md');
    halt(1);
  end
  else
    writeln('-- README.md found');

  ini_file := IncludeTrailingPathDelimiter(ParamStr(1)) + 'doc/GLPT.ini';
  if not(FileExists(ini_file)) then
  begin
    writeln('error: cannot find GLPT.ini');
    halt(1);
  end
  else
    writeln('-- GLPT.ini found');

  source := TStringList.Create;
  source.LoadFromFile(glpt_file);

  list := TStringList.Create;

  //skip until 'interface' is found
  i := 0;
  while Trim(source[i]) <> 'interface' do
    Inc(i);

  //stop when 'implementation' is found
  while Trim(source[i]) <> 'implementation' do
  begin
    if pos('function', LowerCase(source[i])) = 1 then
    begin
      p := Pos('(', source[i]);
      if p = 0 then
        p := Pos(':', source[i]);

      s := Trim(Copy(source[i], 9, p - 9));
      list.Add(s);
    end;

    if pos('procedure', LowerCase(source[i])) = 1 then
    begin
      p := Pos('(', source[i]);
      if p = 0 then
        p := Pos(';', source[i]);

      s := Trim(Copy(source[i], 10, p - 10));
      list.Add(s);
    end;

    inc(i);
  end;
  source.Free;

  //open the ini file
  ini := TIniFile.Create(ini_file);

  //create the api_list
  list.Sort;
  SetLength(api_list, list.Count);
  for i := 0 to list.Count - 1 do
  begin
    api_list[i].Name := list[i];
    api_list[i].linux := ini.ReadString(list[i], 'Linux', 'Not implemented');
    api_list[i].mac := ini.ReadString(list[i], 'MacOSX', 'Not implemented');
    api_list[i].win := ini.ReadString(list[i], 'Windows', 'Not implemented');
  end;

  readme := TStringList.Create;
  readme.LoadFromFile(readme_file);

  //remove old API list
  i := 0;
  while readme[i] <> '<!-- API-SUPPORT-LIST:START -->' do
    inc(i);

  inc(i);
  while readme[i] <> '<!-- API-SUPPORT-LIST:END -->' do
    readme.Delete(i);

  //print API list
  for j := list.Count - 1 downto 0 do
  begin
    readme.Insert(i, '| ' + api_list[j].Name + ' | ' + asIcon(api_list[j].linux) +
      ' | ' + asIcon(api_list[j].mac) + ' | ' + asIcon(api_list[j].win) + ' | ');
  end;
  readme.Insert(i, '|---------------------------|-----------------|-----------------|-----------------|');
  readme.Insert(i, '| API function              | Linux (X11)     | Mac OSX (Cocoa) | Windows (GDI)   |');
  readme.Insert(i, '## API (v' + GLPT_GetVersionString + ') support');

  readme.SaveToFile(readme_file);
  readme.Free;

  //write INI
  for i := 0 to list.Count - 1 do
  begin
    ini.WriteString(api_list[i].Name, 'Linux', api_list[i].linux);
    ini.WriteString(api_list[i].Name, 'MacOSX', api_list[i].mac);
    ini.WriteString(api_list[i].Name, 'Windows', api_list[i].win);
  end;

  ini.Free;
  list.Free;
end.
