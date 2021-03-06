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

with Ada.Strings.Unbounded;

package Iup.Util is
    -- --------------------------
    -- Attribute setter functions
    -- --------------------------

    type Attribute_Declaration_Type is private;
    type Attribute_Initialization_Type is array (Positive range <>) of Attribute_Declaration_Type;

    type Expand_Type is (Yes, Horizontal, Vertical, No);
    type Line_Alignment_Type is (Align_Top, Align_Center, Align_Bottom);
    type Column_Alignment_Type is (Align_Left, Align_Center, Align_Right);

    function Numdiv(Value:Positive) return Attribute_Declaration_Type;
    function Size(X:Natural; Y:Natural) return Attribute_Declaration_Type;
    function Size_col(Value:Positive) return Attribute_Declaration_Type;
    function Size_lin(Value:Positive) return Attribute_Declaration_Type;
    function Margin(X:Natural; Y:Natural) return Attribute_Declaration_Type;
    function Gap(X:Natural; Y:Natural) return Attribute_Declaration_Type;
    function Gap_Lin(X:Natural) return Attribute_Declaration_Type;
    function Gap_Col(X:Natural) return Attribute_Declaration_Type;
    function Title(Value:String) return Attribute_Declaration_Type;
    function Password(Value:Boolean) return Attribute_Declaration_Type;
    function Expand(Value:Expand_Type) return Attribute_Declaration_Type;
    function Alignment_Line(Line_Index: Positive; Alignment:Line_Alignment_Type) return Attribute_Declaration_Type;
    function Alignment_Column(Column_Index: Positive; Alignment:Column_Alignment_Type) return Attribute_Declaration_Type;
    function Alignment_Lines(Alignment:Line_Alignment_Type) return Attribute_Declaration_Type;
    function Alignment_Columns(Alignment:Column_Alignment_Type) return Attribute_Declaration_Type;
    function Visible_Columns(Value:Positive) return Attribute_Declaration_Type;
    function Visible_Lines(Value:Positive) return Attribute_Declaration_Type;

    procedure Set(Ih: Handle; Attribute:Attribute_Declaration_Type);
    procedure Set(Ih: Handle; Attribute:Attribute_Initialization_Type);

    -- -----------------------
    -- Widget helper functions
    -- -----------------------

    -- Function: Grid_Box
    function Grid_Box(Attributes:Attribute_Initialization_Type; Children:Handle_Array) return Handle;

    -- Function: Grid_Box
    function Grid_Box(Children:Handle_Array) return Handle;

    -- Function: V_Box
    function V_Box(Children:Handle_Array) return Handle;

    -- Function: H_Box
    function H_Box(Children:Handle_Array) return Handle;

    -- Function: Dialog
    function Dialog(Attributes:Attribute_Initialization_Type; Child:Handle) return Handle;

    -- Function: Text
    function Text(Attributes:Attribute_Initialization_Type) return Handle;

    -- Function: Button
    function Button(Title:String; Attributes:Attribute_Initialization_Type) return Handle;
private
    package UB renames Ada.Strings.Unbounded;

    type Attribute_Declaration_Type is record
        Name: UB.Unbounded_String;
        Value: UB.Unbounded_String;
    end record;

end;
