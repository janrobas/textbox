unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls,
  SynEdit, SynCompletion, RTTICtrls, StrUtils, IniFiles, LazUTF8;

const
  APP_NAME = 'Textbox';

type

  { TForm1 }

  TForm1 = class(TForm)
    FindDialog1: TFindDialog;
    FontDialog1: TFontDialog;
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    MenuHelp: TMenuItem;
    MenuHelpAbout: TMenuItem;
    MenuViewDarkTheme: TMenuItem;
    MenuViewWordWrap: TMenuItem;
    MenuViewFont: TMenuItem;
    MenuView: TMenuItem;
    MenuSearchFindNext: TMenuItem;
    MenuSearchFind: TMenuItem;
    MenuItemFile: TMenuItem;
    MenuFileNew: TMenuItem;
    MenuFileOpen: TMenuItem;
    MenuFileSave: TMenuItem;
    MenuFileSaveAs: TMenuItem;
    MenuFileExit: TMenuItem;
    MenuItemSearch: TMenuItem;
    Separator1: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    procedure FindDialog1Find(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
    procedure MenuFileExitClick(Sender: TObject);
    procedure MenuFileNewClick(Sender: TObject);
    procedure MenuFileSaveAsClick(Sender: TObject);
    procedure MenuFileSaveClick(Sender: TObject);
    procedure MenuHelpAboutClick(Sender: TObject);
    procedure MenuViewDarkThemeClick(Sender: TObject);
    procedure MenuViewWordWrapClick(Sender: TObject);
    procedure MenuViewFontClick(Sender: TObject);
    procedure MenuSearchFindClick(Sender: TObject);
    procedure MenuFileOpenClick(Sender: TObject);
    procedure MenuSearchFindNextClick(Sender: TObject);
    procedure UpdateCaption;
    procedure LoadSettings;
    procedure SaveSettings;
    procedure SetTheme;
    procedure FileOpen(FileName: string);
  private
    FCurrentFileName: string;
    FFileModified: Boolean;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.MenuFileNewClick(Sender: TObject);
begin
  memo1.Clear;
  FCurrentFileName := '';
  FFileModified := False;
  UpdateCaption;
end;

procedure TForm1.MenuFileSaveAsClick(Sender: TObject);
begin
  if SaveDialog1.Execute then
  begin
    Memo1.Lines.SaveToFile(SaveDialog1.FileName, TEncoding.UTF8);     
    FCurrentFileName := SaveDialog1.FileName;
    FFileModified := False;
    UpdateCaption;
  end;
end;

procedure TForm1.MenuFileSaveClick(Sender: TObject);
var
  WordWrapPrej: Boolean;
begin
  if FCurrentFileName <> '' then
  begin
    WordWrapPrej := Memo1.WordWrap;
    Memo1.WordWrap := False;
    Memo1.Lines.SaveToFile(FCurrentFileName, TEncoding.UTF8);
    FFileModified := False;
    UpdateCaption;
    Memo1.WordWrap := WordWrapPrej;
  end
  else
  MenuFileSaveAsClick(Sender);
end;

procedure TForm1.MenuHelpAboutClick(Sender: TObject);
begin
  ShowMessage('Textbox.' + sLineBreak + sLineBreak + 'https://github.com/janrobas/textbox');
end;


procedure TForm1.MenuViewDarkThemeClick(Sender: TObject);
begin
  MenuViewDarkTheme.Checked := not MenuViewDarkTheme.Checked;
  SetTheme;
end;

procedure TForm1.SetTheme();
begin
  if MenuViewDarkTheme.Checked then
  begin
    Memo1.Color := clBlack;
    Memo1.Font.Color := TColor($DDDDDD);
  end
  else
  begin
    Memo1.Color := clWhite;
    Memo1.Font.Color := clBlack;
  end;

  //Memo1.Invalidate;
end;

procedure TForm1.MenuViewWordWrapClick(Sender: TObject);
begin
  MenuViewWordWrap.Checked := not MenuViewWordWrap.Checked;
  Memo1.WordWrap := MenuViewWordWrap.Checked;
end;

procedure TForm1.MenuViewFontClick(Sender: TObject);
begin
  FontDialog1.Font := Memo1.Font;
  if FontDialog1.Execute then
  begin
     Memo1.Font := FontDialog1.Font;
  end;
end;



procedure TForm1.MenuSearchFindClick(Sender: TObject);
begin
  FindDialog1.Execute;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  OpenDialog1.Filter := SaveDialog1.Filter;
  FCurrentFileName := '';
  FFileModified := False;
  UpdateCaption;
  LoadSettings;

  if (ParamCount > 0) and FileExists(ParamStr(1)) then
     FileOpen(ParamStr(1))
end;

// TODO: this does not work with čžš yet...
procedure TForm1.FindDialog1Find(Sender: TObject);
var
  StartPos, FoundPos: Integer;
  SearchText, MemoText: string;
begin
  SearchText := FindDialog1.FindText;
  if SearchText = '' then Exit;

  MemoText := Memo1.Text;

  StartPos := Memo1.SelStart + Memo1.SelLength + 1;

  if frMatchCase in FindDialog1.Options then
    FoundPos := Pos(SearchText, Copy(MemoText, StartPos, MaxInt))
  else
    FoundPos := Pos(UTF8LowerCase(SearchText), UTF8LowerCase(Copy(MemoText, StartPos, MaxInt)));

  if FoundPos > 0 then
  begin
    FoundPos := FoundPos + StartPos - 1;
    Memo1.SelStart := FoundPos - 1;
    Memo1.SelLength := Length(SearchText);
    Memo1.SetFocus;
  end
  else
  begin
    // wrap
    if frMatchCase in FindDialog1.Options then
      FoundPos := Pos(SearchText, MemoText)
    else
      FoundPos := Pos(UTF8LowerCase(SearchText), UTF8LowerCase(MemoText));

    if FoundPos > 0 then
    begin
      Memo1.SelStart := FoundPos - 1;
      Memo1.SelLength := Length(SearchText);
      Memo1.SetFocus;
    end
    else
      ShowMessage('Text not found: ' + SearchText);
  end;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SaveSettings;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  Response: TModalResult;
begin
  if not FFileModified then exit;

  Response := MessageDlg('The document has been modified.' + sLineBreak +
                        'Do you want to save your changes?',
                        mtConfirmation, [mbYes, mbNo, mbCancel], 0);

  if Response = mrCancel then
    CanClose := False
  else if Response = mrYes then
    MenuFileSaveClick(Sender)
end;

procedure TForm1.Memo1Change(Sender: TObject);
begin
  FFileModified := True;
  UpdateCaption;
end;

procedure TForm1.MenuFileExitClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.UpdateCaption;
var
  BaseName: string;
begin
  if FCurrentFileName = '' then
    BaseName := 'Untitled'
  else
    BaseName := ExtractFileName(FCurrentFileName);

  if FFileModified then
    Caption := BaseName + '* - ' + APP_NAME
  else
    Caption := BaseName + ' - ' + APP_NAME;
end;

procedure TForm1.MenuFileOpenClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    FileOpen(OpenDialog1.FileName);
  end;
end;

procedure TForm1.FileOpen(FileName: string);
begin          
    Memo1.Lines.LoadFromFile(FileName, TEncoding.UTF8);
    FCurrentFileName := FileName;
    FFileModified := False;
    UpdateCaption;
end;

procedure TForm1.MenuSearchFindNextClick(Sender: TObject);
begin
  if FindDialog1.FindText <> '' then
     FindDialog1Find(Sender)
  else
     FindDialog1.Execute;
end;

procedure TForm1.SaveSettings;
var
  Ini: TIniFile;
  IniPath: string;
begin
  IniPath := ExtractFilePath(ParamStr(0)) + 'settings.ini';
  Ini := TIniFile.Create(IniPath);
  try
    // font
    Ini.WriteString('Font', 'Name', Memo1.Font.Name);
    Ini.WriteInteger('Font', 'Size', Memo1.Font.Size);
    Ini.WriteBool('Font', 'Bold', fsBold in Memo1.Font.Style);
    Ini.WriteBool('Font', 'Italic', fsItalic in Memo1.Font.Style);

    if WindowState <> wsMaximized then
    begin
      Ini.WriteInteger('Window', 'Top', Top);
      Ini.WriteInteger('Window', 'Left', Left);
      Ini.WriteInteger('Window', 'Width', Width);
      Ini.WriteInteger('Window', 'Height', Height);
    end;           
    Ini.WriteBool('Window', 'Maximized', WindowState = wsMaximized);

    Ini.WriteBool('Options', 'WordWrap', MenuViewWordWrap.Checked);
    Ini.WriteBool('Options', 'DarkMode', MenuViewDarkTheme.Checked);
  finally
    Ini.Free;
  end;
end;

procedure TForm1.LoadSettings;
var
  Ini: TIniFile;
  IniPath: string;
  FontStyle: TFontStyles;
begin
  IniPath := ExtractFilePath(ParamStr(0)) + 'settings.ini';
  Ini := TIniFile.Create(IniPath);
  try
    // font
    Memo1.Font.Name := Ini.ReadString('Font', 'Name', 'Courier New');
    Memo1.Font.Size := Ini.ReadInteger('Font', 'Size', 10);

    FontStyle := [];
    if Ini.ReadBool('Font', 'Bold', False) then
      Include(FontStyle, fsBold);
    if Ini.ReadBool('Font', 'Italic', False) then
      Include(FontStyle, fsItalic);
    Memo1.Font.Style := FontStyle;

    Top := Ini.ReadInteger('Window', 'Top', Top);
    Left := Ini.ReadInteger('Window', 'Left', Left);
    Width := Ini.ReadInteger('Window', 'Width', 800);
    Height := Ini.ReadInteger('Window', 'Height', 600);

    if Ini.ReadBool('Window', 'Maximized', False) then
      WindowState := wsMaximized;

    MenuViewWordWrap.Checked := Ini.ReadBool('Options', 'WordWrap', False);
    Memo1.WordWrap := MenuViewWordWrap.Checked;
    MenuViewDarkTheme.Checked := Ini.ReadBool('Options', 'DarkMode', False);
    SetTheme
  finally
    Ini.Free;
  end;
end;

end.

