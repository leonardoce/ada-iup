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

-- Package: iup
-- This package in a thin binding for the IUP portable
-- GUI library. This package, in the initializiation procedure,
-- will initialize the IUP Library

with System;

package body Iup.Direct is
    Active_Widget : Handle;
    Active_Event : Event_Type;

    function Get_Next_Signal return Event_Occurrence_Type is
        Result : Event_Occurrence_Type;

        Null_Widget : constant Handle := Handle(System.Null_Address);
        Callback_Status : Callback_Result_Type;
    begin
        Active_Widget := Null_Widget;

        loop
            Callback_Status := Loop_Step_Wait;

            if Active_Widget/=Null_Widget then
                Result.Widget := Active_Widget;
                Result.Event := Active_Event;
                exit;
            end if;
        end loop;

        return Result;
    end;

    -- ---------------
    -- Action callback
    -- ---------------

    function Global_Action_Callback(Widget:Handle) return Callback_Result_Type is
    begin
        Active_Widget := Widget;
        Active_Event := Action;
        return Continue;
    end;

    procedure Stop_On_Action(Widget:Handle) is
    begin
        Set_Callback(Widget, "ACTION", Global_Action_Callback'Access);
    end;

    -- --------------
    -- Unmap callback
    -- --------------

    function Global_Unmap_Callback(Widget:Handle) return Callback_Result_Type is
    begin
        Active_Widget := Widget;
        Active_Event := Unmap;
        return Continue;
    end;

    procedure Stop_On_Unmap(Widget:Handle) is
    begin
        Set_Callback(Widget, "UNMAP_CB", Global_Unmap_Callback'Access);
    end;
    
    -- --------------
    -- Close callback
    -- --------------

    function Global_Close_Callback(Widget:Handle) return Callback_Result_Type is
    begin
        Active_Widget := Widget;
        Active_Event := Close;
        return Continue;
    end;

    procedure Stop_On_Close(Widget:Handle) is
    begin
        Set_Callback(Widget, "CLOSE_CB", Global_Close_Callback'Access);
    end;
    
end;
