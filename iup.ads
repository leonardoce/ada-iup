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

-- See https://github.com/phasis68/iup_mac/blob/master/include/iup.h

with System;

package Iup is

    -- Represent an IUP Object
    type Handle is private;

    -- Represent an Array of IUP Objects
    type Handle_Array is array (Positive range <>) of Handle;

    -- This is the callback result
    type Callback_Result_Type is (Ignore, Default, Close, Continue);

    -- This function is returned by a callback invoked by IUP
    type Callback_Type is access function (Widget:Handle) return Callback_Result_Type;

    -- Function: Button
    --
    -- This function will create a new button widget
    --
    -- Arguments:
    --   title - The string written inside the button

    function Button(Title:String) return Handle;

    -- Function: Dialog
    --
    -- Creates a dialog element. It manages user interaction with the interface
    -- elements. For any interface element to be shown, it must be encapsulated in a
    -- dialog.
    --
    -- Arguments:
    --   Child -  Identifier of an interface element. The dialog has only one child.
    function Dialog(Child:Handle) return Handle;
    pragma Import(C, Dialog, "IupDialog");

    -- Procedure: Show
    --
    -- Displays a dialog in the current position, or changes a control VISIBLE 
    -- attribute. If the dialog needs to be mapped and the current position is 
    -- not known then the dialog is centered. 
    --
    -- Arguments:
    --   ih - identifier of the interface element.
    procedure Show(Ih:Handle);
    pragma Import(C, Show, "IupShow");

    -- Procedure: Main_Loop
    --
    -- Executes the user interaction until a callback returns IUP_CLOSE, 
    -- IupExitLoop is called, or hiding the last visible dialog.
    procedure Main_Loop;
    pragma Import(C, Main_Loop, "IupMainLoop");

    -- Procedure: Exit_Loop
    --
    -- Terminates the current message loop. It has the same effect of a 
    -- callback returning IUP_CLOSE. 
    procedure Exit_Loop;
    pragma Import(C, Exit_Loop, "IupExitLoop");

    -- Procedure: Set_Attribute
    --
    -- Sets an interface element attribute. See also the Attributes Guide section.
    --
    -- Arguments:
    --   ih - Identifier of the interface element. If NULL will set in the global environment.
    --   name - name of the attribute.
    --   value - value of the attribute
    procedure Set_Attribute(Ih:Handle; name:String; value:String);

    -- Function: Get_Attribute
    -- Returns the value of an interface element attribute. See also the Attributes Guide section.
    --
    -- Arguments:
    --   ih - Identifier of the interface element. If NULL will set in the global environment.
    --   name - name of the attribute.
    function Get_Attribute(Ih:Handle; name:String) return String;

    -- Function: H_Box
    -- Creates a void container for composing elements horizontally. It is a box that arranges 
    -- the elements it contains from left to right.
    function H_Box return Handle;
    
    -- Function: H_Box
    -- Creates a void container for composing elements vertically. It is a box that arranges 
    -- the elements it contains from top to bottom.
    function V_Box return Handle;
    
    -- Function: Z_Box
    -- Creates a void container for composing elements vertically. It is a box that arranges 
    -- the elements it contains from top to bottom.
    function Z_Box return Handle;

    -- Function: Grid_Box
    -- Creates a void container for composing elements in a regular grid. It is a box that 
    -- arranges the elements it contains from top to bottom and from left to right, but can 
    -- distribute the elements in lines or in columns.
    function Grid_Box return Handle;

    -- Procedure: Append
    -- Inserts an interface element at the end of the container, after the last element of the 
    -- container. Valid for any element that contains other elements like dialog, frame, hbox, 
    -- vbox, zbox or menu.
    --
    -- Arguments:
    --   ih - Identifier of the interface element. If NULL will set in the global environment.
    --   New_Child - Identifier of the new interface element.
    procedure Append(Ih:Handle; New_Child:Handle);
    pragma Import(C, Append, "IupAppend");

    -- Procedure: Append
    procedure Append(Ih:Handle; Children: Handle_Array);

    -- Function: Text
    -- Creates an editable text field.
    function Text return Handle;

    -- Procedure: Set_Callback
    -- Associates a callback to an event.
    --
    -- Arguments:
    --   ih - Identifier of the interface element.
    --   name - Attribute name of the callback
    --   callback - address of a function.
    procedure Set_Callback(Ih:Handle; Name:String; Callback:Callback_Type);

    -- Procedure: Loop_Step
    -- Runs one iteration of the message loop.
    function Loop_Step return Callback_Result_Type;

    -- Procedure: Loop_Step_Wait
    -- Runs one iteration of the message loop.
    function Loop_Step_Wait return Callback_Result_Type;

    -- Function: Label
    -- Creates a label interface element, which displays a separator, a text or an image.
    function Label(Title:String) return Handle;

private

    type Handle is new System.Address;

end;
