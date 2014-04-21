package body Iup.Util is
    -- -----------------
    -- Attribute setters
    -- -----------------
    
    function Common_Attribute(Name:String; Value:String) return Attribute_Declaration_Type is
        use UB;
    begin
        return (Name=>To_Unbounded_String(Name), Value=>To_Unbounded_String(Value));
    end;

    function Numdiv(Value:Positive) return Attribute_Declaration_Type is
    begin
        return Common_Attribute("NUMDIV", Positive'Image(Value));
    end;

    function Sizecol(Value:Positive) return Attribute_Declaration_Type is
    begin
        return Common_Attribute("SIZECOL", Positive'Image(Value));
    end;

    function Title(Value:String) return Attribute_Declaration_Type is
    begin
        return Common_Attribute("TITLE", Value);
    end;

    function Margin(X:Natural; Y:Natural) return Attribute_Declaration_Type is
    begin
        return Common_Attribute("MARGIN", Natural'Image(X) & "X" & Natural'Image(Y));
    end;

    function Gap(X:Natural; Y:Natural) return Attribute_Declaration_Type is
    begin
        return Common_Attribute("GAP", Natural'Image(X) & "X" & Natural'Image(Y));
    end;

    function Gap_Lin(X:Natural) return Attribute_Declaration_Type is
    begin
        return Common_Attribute("GAPLIN", Natural'Image(X));
    end;

    function Gap_Col(X:Natural) return Attribute_Declaration_Type is
    begin
        return Common_Attribute("GAPCOL", Natural'Image(X));
    end;

    function Password(Value:Boolean) return Attribute_Declaration_Type is
    begin
        if Value then
            return Common_Attribute("PASSWORD", "YES");
        else
            return Common_Attribute("PASSWORD", "NO");
        end if;
    end;

    function Expand(Value:Expand_Type) return Attribute_Declaration_Type is
    begin
        case Value is
            when Horizontal => return Common_Attribute("EXPAND", "HORIZONTAL");
            when Vertical => return Common_Attribute("EXPAND", "VERTICAL");
            when Yes => return Common_Attribute("EXPAND", "YES");
            when No => return Common_Attribute("EXPAND", "NO");
        end case;
    end;

    procedure Set(Ih: Handle; Attribute:Attribute_Declaration_Type) is
        use UB;
    begin
        Set_Attribute(Ih, To_String(Attribute.Name), To_String(Attribute.Value));
    end;

    procedure Set(Ih: Handle; Attribute:Attribute_Initialization_Type) is
        use UB;
    begin
        for i in Attribute'Range loop
            Set(Ih, Attribute(i));
        end loop;
    end;

    -- ---------------
    -- Widget creators
    -- ---------------

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

    function Grid_Box(Attributes:Attribute_Initialization_Type; Children:Handle_Array) return Handle is
        Result : Handle := Grid_Box(Children);
    begin
        Set(Result, Attributes);
        return Result;
    end;

    function Dialog(Attributes:Attribute_Initialization_Type; Child:Handle) return Handle is
        Result : Handle := Dialog(Child);
    begin
        Set(Result, Attributes);
        return Result;
    end;

    function Text(Attributes:Attribute_Initialization_Type) return Handle is
        Result : Handle := Text;
    begin
        Set(Result, Attributes);
        return Result;
    end;
end;
