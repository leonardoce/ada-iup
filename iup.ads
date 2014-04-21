-- Package: iup
-- This package in a thin binding for the IUP portable
-- GUI library. This package, in the initializiation procedure,
-- will initialize the IUP Library

with System;

package Iup is

    type Handle is private;

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
    
private

    type Handle is new System.Address;

end;
