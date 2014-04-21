with Iup;

procedure provaiup is
    use Iup;

    Button_Mammamia : Handle := Button("mammamia");
    Main_Dialog : Handle := Dialog(Button_Mammamia);
begin
    Set_Attribute(Main_Dialog, "TITLE", "Questa e' una prova!");
    Show(Main_Dialog);
    Main_Loop;
end;
