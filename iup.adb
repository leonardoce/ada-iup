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
        procedure Iup_Set_Attribute(Id:Handle; name:C.char_array; value:C.char_array);
        pragma Import(C, Iup_Set_Attribute, "IupSetAttribute");

    begin
        Iup_Set_Attribute(Ih, C.To_C(name), C.To_C(Value));
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
        pragma Import(C, Iup_Hbox, "Iup_Hbox");
    begin
        return Iup_Hbox(System.Null_Address);
    end;
begin
    Iup_Open(System.Null_Address, System.Null_Address);
end Iup;
