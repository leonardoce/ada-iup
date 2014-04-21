with Iup; use Iup;
with Ada.Text_IO;

package body esempio is

    function Test_Callback(Ih:Handle) return Callback_Result_Type is
        use Ada.Text_IO;
    begin
        Put_Line("Questo e' un callback di test!");
        return Default;
    end;

    procedure main is
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
        Set_Callback(Button_Mammamia, "ACTION", Test_Callback'Access);

        Show(Main_Dialog);
        
        Main_Loop;
    end;
end;
