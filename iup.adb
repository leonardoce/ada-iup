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

with Ada.Containers;
with Ada.Containers.Vectors;

with Interfaces.C;
with Interfaces.C.Strings;

package body Iup is
    pragma Linker_Options("-liup");

    package C renames Interfaces.C;
    package CStrings renames Interfaces.C.Strings;

    procedure Iup_Open(argc: System.Address; argv:System.Address);
    pragma Import(C, Iup_Open, "IupOpen");

    function Button(Title:String) return Handle is
        function Iup_Button(title:C.char_array; Action:System.Address) return System.Address;
        pragma Import(C, Iup_Button, "IupButton");

    begin
        return Handle(Iup_Button(C.To_C(Title), System.Null_Address));
    end Button;

    procedure Set_Attribute(Ih:Handle; name:String; value:String) is
        procedure Iup_Store_Attribute(Id:Handle; name:C.char_array; value:C.char_array);
        pragma Import(C, Iup_Store_Attribute, "IupStoreAttribute");

    begin
        Iup_Store_Attribute(Ih, C.To_C(name), C.To_C(Value));
    end;

    function Get_Attribute(Ih:Handle; name:String) return String is
        use CStrings;

        function Iup_Get_Attribute(Id:Handle; name:C.char_array) return chars_ptr;
        pragma Import(C, Iup_Get_Attribute, "IupGetAttribute");

        result: chars_ptr := Iup_Get_Attribute(Ih, C.To_C(name));
    begin
        if result = Null_Ptr then 
            return "";
        else
            return Cstrings.Value(result);
        end if;
    end;

    function H_Box return Handle is
        function Iup_Hbox(Nope:System.Address) return Handle;
        pragma Import(C, Iup_Hbox, "IupHbox");
    begin
        return Iup_Hbox(System.Null_Address);
    end;

    function V_Box return Handle is
        function Iup_Vbox(Nope:System.Address) return Handle;
        pragma Import(C, Iup_Vbox, "IupVbox");
    begin
        return Iup_Vbox(System.Null_Address);
    end;

    function Z_Box return Handle is
        function Iup_Zbox(Nope:System.Address) return Handle;
        pragma Import(C, Iup_Zbox, "IupZbox");
    begin
        return Iup_Zbox(System.Null_Address);
    end;

    -- --------------------------------
    -- Callback return value management
    -- --------------------------------

    function Callback_Result_To_Integer(Callback_Result: Callback_Result_Type) return Integer is
    begin
        case Callback_Result is
            when Ignore => return -1;
            when Default => return -2;
            when Close => return -3;
            when Continue => return -4;
            when others => return -4;
        end case;
    end;

    function Integer_To_Callback_Result(V: Integer) return Callback_Result_Type is
    begin
        if V=(-1) then
            return Ignore;
        elsif V=(-2) then
            return Default;
        elsif V=(-3) then
            return Close;
        elsif V=(-4) then
            return Continue;
        else
            return Continue;
        end if;
    end;

    function Loop_Step return Callback_Result_Type is
        function Iup_Loop_Step return Integer;
        pragma Import(C, Iup_Loop_Step, "IupLoopStep");
    begin
        return Integer_To_Callback_Result(Iup_Loop_Step);
    end;

    function Loop_Step_Wait return Callback_Result_Type is
        function Iup_Loop_Step_Wait return Integer;
        pragma Import(C, Iup_Loop_Step_Wait, "IupLoopStepWait");
    begin
        return Integer_To_Callback_Result(Iup_Loop_Step_Wait);
    end;

    -- ------------------------------------------
    -- Callback management. Deep black magic here
    -- ------------------------------------------

    package Callback_Vector_Pkg is new Ada.Containers.Vectors(Positive, Callback_Type);
    
    Ada_Callback_Prefix : constant String := "__ADA_CALLBACK_ID__";
    Callback_Vector: Callback_Vector_Pkg.Vector;
    subtype Callback_Id_Type is Ada.Containers.Count_Type;


    function Internal_Callback(Ih:Handle) return Integer;
    pragma Convention(C, Internal_Callback);

    function Internal_Callback(Ih:Handle) return Integer is
        use type CStrings.chars_ptr;

        function Iup_Get_Action_Name return CStrings.chars_ptr;
        pragma Import(C, Iup_Get_Action_Name, "IupGetActionName");

        C_Callback_Name : CStrings.chars_ptr;
    begin
        C_Callback_Name := Iup_Get_Action_Name;
        if C_Callback_Name = CStrings.Null_Ptr then
            raise Program_Error with "IupAda callback invoked from a non Ada callback. Why?";
        end if;

        declare 
            Callback_Name : String := CStrings.Value(Iup_Get_Action_Name);
            Callback_Id : Callback_Id_Type;
            Callback_Result : Callback_Result_Type;
        begin
            if Callback_Name(Ada_Callback_Prefix'Range) /= Ada_Callback_Prefix then
                raise Program_Error with "IupAda callback invoked with the wrong name " & Callback_Name & ". This sounds like an internal error";
            end if;

            Callback_Id := Callback_Id_Type'Value(Callback_Name(Ada_Callback_Prefix'Last+1..Callback_Name'Last));
            Callback_Result := Callback_Vector_Pkg.Element(Callback_Vector, Positive(Callback_Id))(Ih);

            return Callback_Result_To_Integer(Callback_Result);
        exception
            when Constraint_Error => raise Program_Error with "IupAda callback with the wrong id " & Callback_Name;
        end;
    end;

    procedure Set_Callback(Ih:Handle; Name:String; Callback:Callback_Type) is
        type Internal_Callback_Access is access function(Ih:Handle) return Integer;
        pragma Convention(C, Internal_Callback_Access);

        procedure Iup_Set_Callback(Ih:Handle; Name: C.char_array; Callback:Internal_Callback_Access);
        pragma Import(C, Iup_Set_Callback, "IupSetCallback");

        procedure Iup_Set_Function(Name: C.char_array; Callback:Internal_Callback_Access);
        pragma Import(C, Iup_Set_Function, "IupSetFunction");

        Callback_Id : Callback_Id_Type;
    begin
        Callback_Vector_Pkg.Append(Callback_Vector, Callback);
        Callback_Id := Callback_Vector_Pkg.Length(Callback_Vector);

        declare
            Internal_Callback_Name : String := Ada_Callback_Prefix & Callback_Id_Type'Image(Callback_Id); 
        begin
            Iup_Set_Function(C.To_C(Internal_Callback_Name), Internal_Callback'Access);
            Set_Attribute(Ih, Name, Internal_Callback_Name);
        end;
    end;

    procedure Append(Ih:Handle; Children: Handle_Array) is
    begin
        for i in Children'Range loop
            Append(Ih, Children(i));
        end loop;
    end;
begin
    Iup_Open(System.Null_Address, System.Null_Address);
end Iup;
