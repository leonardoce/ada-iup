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

    function Size_col(Value:Positive) return Attribute_Declaration_Type is
    begin
        return Common_Attribute("SIZECOL", Positive'Image(Value));
    end;

    function Size_lin(Value:Positive) return Attribute_Declaration_Type is
    begin
        return Common_Attribute("SIZELIN", Positive'Image(Value));
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

    function Size(X:Natural; Y:Natural) return Attribute_Declaration_Type is
    begin
        return Common_Attribute("SIZE", Natural'Image(X) & "x" & Natural'Image(Y));
    end;

    function Alignment_To_String(Alignment:Line_Alignment_Type) return String is
    begin
        case Alignment is
            when Align_Top => return "ATOP";
            when Align_Center => return "ACENTER";
            when Align_Bottom => return "ABOTTOM";
        end case;
    end;

    function Alignment_To_String(Alignment:Column_Alignment_Type) return String is
    begin
        case Alignment is
            when Align_Left => return "ALEFT";
            when Align_Center => return "ACENTER";
            when Align_Right => return "ARIGHT";
        end case;
    end;

    function Alignment_Line(Line_Index: Positive; Alignment:Line_Alignment_Type) return Attribute_Declaration_Type is
    begin
        return Common_Attribute("ALIGNMENTLIN" & Integer'Image(Line_Index-1), Alignment_To_String(Alignment));
    end;

    function Alignment_Column(Column_Index: Positive; Alignment:Column_Alignment_Type) return Attribute_Declaration_Type is
    begin
        return Common_Attribute("ALIGNMENTCOL" & Integer'Image(Column_Index-1), Alignment_To_String(Alignment));
    end;

    function Alignment_Lines(Alignment:Line_Alignment_Type) return Attribute_Declaration_Type is
    begin
        return Common_Attribute("ALIGNMENTLIN", Alignment_To_String(Alignment));
    end;

    function Alignment_Columns(Alignment:Column_Alignment_Type) return Attribute_Declaration_Type is
    begin
        return Common_Attribute("ALIGNMENTCOL", Alignment_To_String(Alignment));
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

    function Button(Title:String; Attributes:Attribute_Initialization_Type) return Handle is
        Result : Handle := Button(Title);
    begin
        Set(Result, Attributes);
        return Result;
    end;
end;
