with Ada.Strings.Unbounded;

package Iup.Util is
    -- --------------------------
    -- Attribute setter functions
    -- --------------------------

    type Attribute_Declaration_Type is private;
    type Attribute_Initialization_Type is array (Positive range <>) of Attribute_Declaration_Type;

    function Numdiv(Value:Positive) return Attribute_Declaration_Type;
    function Sizecol(Value:Positive) return Attribute_Declaration_Type;
    function Title(Value:String) return Attribute_Declaration_Type;

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

private
    package UB renames Ada.Strings.Unbounded;

    type Attribute_Declaration_Type is record
        Name: UB.Unbounded_String;
        Value: UB.Unbounded_String;
    end record;

end;
