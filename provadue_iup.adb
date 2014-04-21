with Ada.Text_IO;

with Iup;
with Iup.Util;
with Iup.Direct;

procedure provadue_iup is
    package IO renames Ada.Text_IO;
    use type Iup.Handle;
    use type Iup.Direct.Event_Type;

    Bt_Ok: Iup.Handle := Iup.Button("Ok");
    Bt_Cancel: Iup.Handle := Iup.Button("Cancel");

    Dialog: Iup.Handle;

    Tf_Username : Iup.Handle := Iup.Text;
    Tf_Password : Iup.Handle := Iup.Text;

    Event: Iup.Direct.Event_Occurrence_Type;
    Scatola: Iup.Handle;
begin
    Iup.Direct.Stop_On_Action(Bt_Ok);

    Scatola := Iup.Util.V_Box( (
        Iup.Util.Grid_Box(2, 
        (
            Iup.Label("Username:"), Tf_Username,
            Iup.Label("Password"), Tf_Password
        )),
        Iup.Util.H_Box(
        (
            Bt_Ok, Bt_Cancel
        ))
    ));

    Iup.Set_Attribute(Scatola, "NUMDIV", "2");

    Dialog := Iup.Dialog(Scatola);
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
