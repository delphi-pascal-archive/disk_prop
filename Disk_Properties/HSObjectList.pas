{
   Модуль: HSObjectList

   Описание: Класс списка объектов. Уничтожает содержащиеся объекты
             при собственном разрушении,
             имеет метод очистки без уничтожения содержимого.

   Ограничения применения: Элементы списка должны быть наследниками TObject

   Автор: Игорь Шевченко

   Дата создания: 21.11.2001

   История изменений:
   10.12.2003 - В класс THSObjectList добавлено свойство Objects.
}
unit HSObjectList;

interface
uses
  Classes;

type
  THSObjectList = class(TList)
  private
    function GetObjects(I: Integer): TObject;
  public
    property Objects[I: Integer]: TObject read GetObjects; 
    procedure Clear; override;
    { Очистка списка без разрушения содержимого }
    procedure RemoveAll;
  end;


implementation

{ THSObjectList }

procedure THSObjectList.Clear;
var 
  I: Integer;
begin
  for I:=0 to Pred(Count) do
    Objects[I].Free;
  inherited;
end;

function THSObjectList.GetObjects(I: Integer): TObject;
begin
  Result := TObject(inherited Items[I]);
end;

procedure THSObjectList.RemoveAll;
var 
  I: Integer;
begin
  for I:=Pred(Count) downto 0 do
    Delete(I);
end;

end.
