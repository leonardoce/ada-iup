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
with Ada.Text_IO;

with Iup;
with Iup.Util;
with Iup.Direct;

procedure provadue_iup is
    package IO renames Ada.Text_IO;
    use Iup;
    use Iup.Util;
    use type Iup.Direct.Event_Type;

    Bt_Ok: Iup.Handle := Button("Ok", (1=>Expand(Horizontal)));
    Bt_Cancel: Iup.Handle := Button("Cancel", (1=>Expand(Horizontal)));

    Main_Dialog: Iup.Handle;

    Tf_Username : Iup.Handle := Text( (1=>Expand(Horizontal)) );
    Tf_Password : Iup.Handle := Text( (Password(True), Expand(Horizontal)) );

    Event: Iup.Direct.Event_Occurrence_Type;
begin
    Iup.Direct.Stop_On_Action(Bt_Ok);

    Main_Dialog := Dialog( (Title("Login window"), Margin(10,10), Gap_Lin(7), Gap_Col(7), Size(300, 100) ),
        V_Box((
            Grid_Box( (NumDiv(2), Size_lin(1), Size_col(1) ), (
                Label("Username:"), Tf_Username,
                Label("Password:"), Tf_Password
            )),
            H_Box((
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
