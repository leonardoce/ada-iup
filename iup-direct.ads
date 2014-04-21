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

package Iup.Direct is
    type Event_Type is (Action, Unmap, Close);

    type Event_Occurrence_Type is record
        Widget: Handle;
        Event: Event_Type;
    end record;

    -- Get the next signal from the GUI interface. 
    function Get_Next_Signal return Event_Occurrence_Type;

    -- Mark a widget so that he send a signal when he receive an "ACTION" event
    procedure Stop_On_Action(Widget:Handle);

    -- Mark a widget so that he send a signal when he receive an "UNMAP" event
    procedure Stop_On_Unmap(Widget:Handle);

    -- Mark a widget so that he send a signal when he receive an "CLOSE" event
    procedure Stop_On_Close(Widget:Handle);
end;
