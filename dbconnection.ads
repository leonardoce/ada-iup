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

with System;
with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

package DbConnection is

    type Connection_Type is private;
    type Result_Set_Type is private;
    type Prepared_Query_Type is private;

    Db_Error : exception;
    Db_Not_Unique_Result : exception;

    -- Funzioni relative alla connessione con il database
    -- ==================================================

    -- Connetti ad un database
    procedure Connect(Connection:out Connection_Type; Database_Name:String);

    -- Si disconnette da un database
    procedure Disconnect(Connection:in out Connection_Type);

    -- Esegue una query
    procedure Exec(Connection:in Connection_Type; Sql:String);

    -- Esegue una query prendendo l'unico risultato
    function Exec_Into(Connection: in Connection_Type; Sql:String) return String;

    -- Esegue una query tornando un insteme di righe
    function Retrieve(Connection: Connection_Type; Sql:String) return Result_Set_Type;

    -- Prepara una query per l'esecuzione
    procedure Prepare(Connection: in out Connection_Type; Prepared:in out Prepared_Query_Type; Sql:String);

    -- Funzioni relative all'insieme di risultati
    -- ==========================================

    -- Quante colonne ci sono?
    function Get_Fields_Count(Result_Set: Result_Set_Type) return Integer;

    -- Dammi il nome di una certa colonna
    function Get_Column_Name(Result_Set: Result_Set_Type; Column_Number: Positive) return String;

    -- Sei alla fine?
    function Has_More_Records(Result_Set: Result_Set_Type) return Boolean;

    -- Mi dai un certo valore?
    function Get(Result_Set: Result_Set_Type; Column_Number: Positive) return String;

    -- Dealloca i risultati
    procedure Clear(Result_Set: in out Result_Set_Type);

    -- Va al prossimo risultato
    procedure Next(Result_Set: in out Result_Set_Type);

    -- Funzioni relative alle query preparate
    -- ======================================

    -- Quanti parametri ha?
    function Get_Parameter_Count(Prepared_Query: Prepared_Query_Type) return Natural;

    -- Imposta parametro
    procedure Set_Parameter(Prepared_Query: in out Prepared_Query_Type; Number:Positive; Value:String);

    -- Esegui come DML
    --procedure Exec(Prepared_Query: Prepared_Query_Type);

    -- Esegui come query con un risultato
    --function Exec_Into(Prepared_Query: Prepared_Query_Type) return String;

    -- Esegui come query con cursore
    --function Retrieve(Prepared_Query: Prepared_Query_Type) return Result_Set_Type;

    -- Chiudi
    procedure Clear(Prepared_Query: in out Prepared_Query_Type);

private

    package UB renames Ada.Strings.Unbounded;
    package Vec_Pkg is new Ada.Containers.Vectors(
    Index_Type=>Positive, 
    Element_Type=>UB.Unbounded_String,
    "="=>UB."="
    );

    type Connection_Type is record
    Handle: System.Address;
    Connected: Boolean;
    Last_Prepared: Natural;
    end record;

    type Result_Set_Type is record
    Handle: System.Address;
    Current_Row: Natural;
    Column_Count: Natural;
    Row_Count: Natural;
    end record;

    type Prepared_Query_Type is record
    Connection: Connection_Type;
    Sql: UB.Unbounded_String;
    Prepared_Name: UB.Unbounded_String;
    Params: Vec_Pkg.Vector;
    end record;
end;
