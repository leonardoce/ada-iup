with Ada.Text_IO;

with Iup;
with Iup.Util;
with Iup.Direct;

procedure provadue_iup is
    package IO renames Ada.Text_IO;
    use Iup;
    use Iup.Util;
    use type Iup.Direct.Event_Type;

    Bt_Ok: Iup.Handle := Iup.Button("Ok");
    Bt_Cancel: Iup.Handle := Iup.Button("Cancel");

    Main_Dialog: Iup.Handle;

    Tf_Username : Iup.Handle := Text( (1=>Expand(Horizontal)) );
    Tf_Password : Iup.Handle := Text( (Password(True), Expand(Horizontal)) );

    Event: Iup.Direct.Event_Occurrence_Type;
begin
    Iup.Direct.Stop_On_Action(Bt_Ok);

    Main_Dialog := Dialog( (Title("Login window"), Margin(5,5), Gap_Lin(7), Gap_Col(7) ),
        V_Box((
            Grid_Box( (NumDiv(2), Size_lin(1), Size_col(1) ), (
                Label("Username:"), Tf_Username,
                Label("Password:"), Tf_Password
            )),
            H_Box( (
                Bt_Ok, Bt_Cancel
            ))
        ))
    );

    Iup.Direct.Stop_On_Close(Main_Dialog);
    Iup.Show(Main_Dialog);

    loop
        Event := Iup.Direct.Get_Next_Signal;
        if Event.Event = Iup.Direct.Close then
            exit;
        elsif Event.Widget = Bt_Ok then
            IO.Put_Line("You have pressed the button!");
        end if;
    end loop;
end;
