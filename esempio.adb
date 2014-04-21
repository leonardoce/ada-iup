-- The MIT License (MIT)
-- 
-- Copyright (c) 2014, Leonardo Cecchi
-- 
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
-- 
-- The above copyright notice and this permission notice shall be included in
-- all copies or substantial portions of the Software.
-- 
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
-- THE SOFTWARE.

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
