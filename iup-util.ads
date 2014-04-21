package Iup.Util is
    -- -----------------------
    -- Widget helper functions
    -- -----------------------

    -- Function: Grid_Box
    -- Create a Grid Box with the specified number of divisions
    function Grid_Box(Num_Div:Positive; Children:Handle_Array) return Handle;

    -- Function: Grid_Box
    function Grid_Box(Children:Handle_Array) return Handle;

    -- Function: V_Box
    function V_Box(Children:Handle_Array) return Handle;

    -- Function: H_Box
    function H_Box(Children:Handle_Array) return Handle;
end;
