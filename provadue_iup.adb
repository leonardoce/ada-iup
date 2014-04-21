with Ada.Text_IO;

with Iup;
with Iup.Direct;

procedure provadue_iup is
    package IO renames Ada.Text_IO;
    use type Iup.Handle;
    use type Iup.Direct.Event_Type;

    Bt_Ok: Iup.Handle := Iup.Button("Press this button!");
    Dialog: Iup.Handle := Iup.Dialog(Bt_Ok);

    Event: Iup.Direct.Event_Occurrence_Type;
begin
    Iup.Direct.Stop_On_Action(Bt_Ok);
    Iup.Direct.Stop_On_Close(Dialog);
    Iup.Show(Dialog);

    loop
        Event := Iup.Direct.Get_Next_Signal;
        if Event.Event = Iup.Direct.Close then
            exit;
        elsif Event.Widget = Bt_Ok then
            IO.Put_Line("You have pressed the button!");
        end if;
    end loop;
end;
