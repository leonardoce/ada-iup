with Ada.Strings.Unbounded;

package Iup.Util is
    -- --------------------------
    -- Attribute setter functions
    -- --------------------------

    type Attribute_Declaration_Type is private;
    type Attribute_Initialization_Type is array (Positive range <>) of Attribute_Declaration_Type;

    type Expand_Type is (Yes, Horizontal, Vertical, No);

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
