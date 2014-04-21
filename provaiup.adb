with Iup;

procedure provaiup is
    use Iup;

    Button_Mammamia : Handle := Button("mammamia");
    Tf_Name : Handle := Text;
    Box : Handle := V_Box;
    Main_Dialog : Handle := Dialog(Box);
begin
    Append(Box, Tf_Name);
    Append(Box, Button_Mammamia);

    Set_Attribute(Tf_Name, "EXPAND", "YES");
    Set_Attribute(Button_Mammamia, "EXPAND", "YES");
    Set_Attribute(Main_Dialog, "TITLE", "Questa e' una prova!");
    Show(Main_Dialog);
    Main_Loop;
end;
