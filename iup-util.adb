package body Iup.Util is
    function H_Box(Children:Handle_Array) return Handle is
        Result : Handle := H_Box;
    begin
        Append(Result, Children);
        return Result;
    end;

    function V_Box(Children:Handle_Array) return Handle is
        Result : Handle := V_Box;
    begin
        Append(Result, Children);
        return Result;
    end;

    function Grid_Box(Children:Handle_Array) return Handle is
        Result : Handle := Grid_Box;
    begin
        Append(Result, Children);
        return Result;
    end;

    function Grid_Box(Num_Div:Positive; Children:Handle_Array) return Handle is
        Result : Handle := Grid_Box(Children);
    begin
        Set_Attribute(Result, "NUMDIV", Positive'Image(Num_Div));
        return Result;
    end;
end;
