with Iup; use Iup;
with Ada.Text_IO;

package body esempio is

    function Test_Callback(Ih:Handle) return Callback_Result_Type is
        use Ada.Text_IO;
    begin
        Put_Line("Questo e' un callback di test!");
        return Default;
    end;

    function Callback_Chiusura(Ih:Handle) return Callback_Result_Type is
    begin
        return Close;
    end;

    procedure main is
        Button_Mammamia : Handle := Button("Prova callback");
        Button_Chiudi : Handle := Button("Chiudi");
        Tf_Name : Handle := Text;
        Box : Handle := V_Box;
        Scatola_Orizzontale : Handle := H_Box;
        Main_Dialog : Handle := Dialog(Box);

    begin
        Append(Scatola_Orizzontale, Button_Mammamia);
        Append(Scatola_Orizzontale, Button_Chiudi);

        Append(Box, Tf_Name);
        Append(Box, Scatola_Orizzontale);

        Set_Attribute(Tf_Name, "EXPAND", "YES");
        Set_Attribute(Button_Mammamia, "EXPAND", "YES");
        Set_Attribute(Main_Dialog, "TITLE", "Questa e' una prova!");

        Set_Callback(Button_Mammamia, "ACTION", Test_Callback'Access);
        Set_Callback(Button_Chiudi, "ACTION", Callback_Chiusura'Access);

        Show(Main_Dialog);
        
        Main_Loop;
    end;
end;
