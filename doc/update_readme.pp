program update_readme;

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
  source := TStringList.Create;
  source.LoadFromFile('../GLPT.pas');

  list := TStringList.Create;

  //skip until 'interface' is found
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
  ini := TIniFile.Create('GLPT.ini');

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
  readme.LoadFromFile('../README.md');

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

  readme.SaveToFile('../README.md');
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
