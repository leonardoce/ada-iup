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

with Interfaces.C;
with Interfaces.C.Strings;

package body DbConnection is

    package C renames Interfaces.C;
    package C_Strings renames Interfaces.C.Strings;
    
    pragma Linker_Options("-lpq");

    pgres_command_ok: constant Integer := 1;
    pgres_tuples_ok: constant Integer := 2; 

    -- Queste chiamate servono per utilizzare le librerie PQ di PostgreSQL {{{
    function PQ_Connect_Db(Connection_String:C.char_array) return System.Address;
    pragma Import(C, PQ_Connect_Db, "PQconnectdb");

    function PQ_Error_Message(Handle:System.Address) return C_Strings.chars_ptr;
    pragma Import(C, PQ_Error_Message, "PQerrorMessage");

    function PQ_Result_Error_Message(Handle:System.Address) return C_Strings.chars_ptr;
    pragma Import(C, PQ_Result_Error_Message, "PQresultErrorMessage");

    function PQ_Status(Handle:System.Address) return Integer;
    pragma Import(C, PQ_Status, "PQstatus");

    procedure PQ_Finish(Handle:System.Address);
    pragma Import(C, PQ_Finish, "PQfinish");

    function PQ_Exec(Handle:System.Address; Connection_String: C.char_array) return System.Address;
    pragma Import(C, PQ_Exec, "PQexec");

    function PQ_Result_Status(Handle:System.Address) return Integer;
    pragma Import(C, PQ_Result_Status, "PQresultStatus");

    procedure PQ_Clear(Handle:System.Address);
    pragma Import(C, PQ_Clear, "PQclear");

    function PQ_Num_Fields(Handle:System.Address) return Integer;
    pragma Import(C, PQ_Num_Fields, "PQnfields");

    function PQ_Num_Tuples(Handle:System.Address) return Integer;
    pragma Import(C, PQ_Num_Tuples, "PQntuples");

    function PQ_Get_Value(Handle:System.Address; Row, Col:Integer) return C_Strings.chars_ptr;
    pragma Import(C, PQ_Get_Value, "PQgetvalue");
    
    function PQ_Get_Field_Name(Handle:System.Address; Col:Integer) return C_Strings.chars_ptr;
    pragma Import(C, PQ_Get_Field_Name, "PQfname");

    function PQ_Prepare(Handle:System.Address; Statement_Name:C.char_array; Statement_Query: C.char_array; Params:Integer; NotUsed:System.Address) return System.Address;
    pragma Import(C, PQ_Prepare, "PQprepare");
    -- }}}

    -- Data connection {{{
    procedure Connect(Connection:out Connection_Type; Database_Name:String) is
        use System;

        chars: C.char_array(1..1024);
        temp: C.size_t;
    begin
        C.To_C(Database_Name, chars, temp);
        Connection.Handle := PQ_Connect_Db(chars);
        if Connection.Handle = System.Null_Address then
            raise Db_Error;
        end if;

        if PQ_Status(Connection.Handle) /= 0 then
            declare
                Error_Message:String := C_Strings.Value(PQ_Error_Message(Connection.Handle));
            begin
                PQ_Finish(Connection.Handle);
                Connection.Handle := Null_Address;
                raise Db_Error with Error_Message;
            end;
        end if;

        Connection.Connected := True;
    end Connect;

    procedure Disconnect(Connection:in out Connection_Type) is
    begin
        if not Connection.Connected then
            raise Db_Error;
        end if;

        PQ_Finish(Connection.Handle);
        Connection.Handle := System.Null_Address;
    end Disconnect;

    procedure Exec(Connection:in Connection_Type; Sql:String) is
        use System;

        chars: C.char_array(1..4096);
        temp: C.size_t;

        rs: System.Address;
    begin
        if not Connection.Connected then
           raise Db_Error;
        end if;

        C.To_C(Sql, chars, temp);
        rs := PQ_Exec(Connection.Handle, chars);
        if rs = Null_Address then
            raise Db_Error;
        end if;

        if PQ_Result_Status(rs)/=pgres_command_ok then
            PQ_Clear(rs);
            raise Db_Error;
        end if;
    end Exec;

    function Exec_Into(Connection: in Connection_Type; Sql:String) return String is
        use System;

        chars: C.char_array(1..4096);
        temp: C.size_t;

        rs: System.Address;
        res: C_Strings.chars_ptr;
    begin
        if not Connection.Connected then
           raise Db_Error;
        end if;

        C.To_C(Sql, chars, temp);
        rs := PQ_Exec(Connection.Handle, chars);
        if rs = Null_Address then
            raise Db_Error;
        end if;

        if PQ_Result_Status(rs)/=pgres_tuples_ok then
            PQ_Clear(rs);
            raise Db_Error;
        end if;

        if PQ_Num_Fields(rs)/=1 then
            PQ_Clear(rs);
            raise Db_Error;
        end if;

        if PQ_Num_Tuples(rs)/=1 then
            PQ_Clear(rs);
            raise Db_Error;
        end if;

        res := PQ_Get_Value(rs, 0, 0);
        declare
            string_result : String := C_Strings.Value(res);
        begin
            PQ_Clear(rs);
            return string_result;
        end;
    end Exec_Into;

    function Iterator_From_Address(RS: System.Address) return Result_Set_Type is
        Result: Result_Set_Type;
    begin
        Result.Handle := RS;
        Result.Column_Count := PQ_Num_Fields(RS);
        Result.Row_Count := PQ_Num_Tuples(RS);
        Result.Current_Row := 0;
        return Result;
    end;

    function Retrieve(Connection: Connection_Type; Sql:String) return Result_Set_Type is
        use System;

        chars: C.char_array(1..4096);
        temp: C.size_t;

        rs: System.Address;
        res: C_Strings.chars_ptr;
    begin
        if not Connection.Connected then
           raise Db_Error;
        end if;

        C.To_C(Sql, chars, temp);
        rs := PQ_Exec(Connection.Handle, chars);
        if rs = Null_Address then
            raise Db_Error with "Memory allocation error in LibPQ";
        end if;

        if PQ_Result_Status(rs)/=pgres_tuples_ok then
            declare 
                Error_Message:String := C_Strings.Value(PQ_Result_Error_Message(rs));
            begin
                PQ_Clear(rs);
                raise Db_Error with Error_Message;
            end;
        end if;

        return Iterator_From_Address(rs);
    end Retrieve;
    -- }}}

    -- Iteratore delle righe {{{
    function Get_Fields_Count(Result_Set: Result_Set_Type) return Integer is
    begin
        return Result_Set.Column_Count;
    end Get_Fields_Count;

    function Has_More_Records(Result_Set: Result_Set_Type) return Boolean is
    begin
        return Result_Set.Current_Row<Result_Set.Row_Count;
    end Has_More_Records;

    function Get_Column_Name(Result_Set: Result_Set_Type; Column_Number: Positive) return String is
        use System;
    begin
        if Column_Number>Result_Set.Column_Count then
            raise Constraint_Error;
        end if;

        if Result_Set.Handle = System.Null_Address then
            raise Constraint_Error;
        end if;

        return C_Strings.Value(PQ_Get_Field_Name(Result_Set.Handle, Column_Number-1));
    end Get_Column_Name;

    function Get(Result_Set: Result_Set_Type; Column_Number: Positive) return String is
        use System;
    begin
        if Result_Set.Handle = System.Null_Address then
            raise Constraint_Error;
        end if;

        if Column_Number>Result_Set.Column_Count then
            raise Constraint_Error;
        end if;

        if Result_Set.Current_Row>Result_Set.Row_Count then
            raise Constraint_Error;
        end if;

        return C_Strings.Value(PQ_Get_Value(Result_Set.Handle, Result_Set.Current_Row, Column_Number-1));
    end Get;

    procedure Clear(Result_Set: in out Result_Set_Type) is
        use System;
    begin
        if Result_Set.Handle = System.Null_Address then
            raise Constraint_Error;
        end if;

        PQ_Clear(Result_Set.Handle);

        Result_Set.Handle := System.Null_Address;
    end Clear;

    procedure Next(Result_Set: in out Result_Set_Type) is
    begin
        Result_Set.Current_Row := Result_Set.Current_Row + 1;
    end Next;
    -- }}}

    -- Query preparate
    
    procedure Prepare(Connection: in out Connection_Type; Prepared:in out Prepared_Query_Type; Sql:String) is
        use UB;
        use Vec_Pkg;
        use System;

        Parameters_Count: Natural := 0;
        Dest_Query: Unbounded_String;
        Buffer: C.char_array(1..1024);
        Buffer2: C.char_array(1..4096);
        Temp: C.size_t;
        RS: System.Address;
        Status: Integer;
    begin
        if Length(Prepared.Prepared_Name)/=0 then
            raise Constraint_Error with "Query already prepared";
        end if;

        for i in Sql'Range loop
            if Sql(i)='?' then
                Parameters_Count := Parameters_Count + 1;
                Dest_Query := Dest_Query & "$" & Natural'Image(Parameters_Count);
            else
                Dest_Query := Dest_Query & Sql(i);
            end if;
        end loop;
        
        Prepared.Sql := Dest_Query;
        Set_Length(Prepared.Params, Ada.Containers.Count_Type(Parameters_Count));
        
        Connection.Last_Prepared := Connection.Last_Prepared + 1;
        Prepared.Prepared_Name := To_Unbounded_String("PREPARED_" & Natural'Image(Connection.Last_Prepared));

        C.To_C(To_String(Prepared.Prepared_Name), Buffer, Temp);
        C.To_C(To_String(Prepared.Sql), Buffer2, Temp);
        RS := PQ_Prepare(Connection.Handle, Buffer, Buffer2, Parameters_Count, System.Null_Address);

        if RS=System.Null_Address then
            raise Db_Error with "Libpq memory allocation error";
        end if;

        Status := PQ_Result_Status(RS);
        if Status/=pgres_command_ok then
            declare
                Message : String := C_Strings.Value(PQ_Result_Error_Message(RS));
            begin
                PQ_Clear(RS);
                raise Db_Error with Message;
            end;
        end if;

        PQ_Clear(RS);
    end Prepare;

    function Get_Parameter_Count(Prepared_Query: Prepared_Query_Type) return Natural is
        use Vec_Pkg;
    begin
        return Natural(Length(Prepared_Query.Params));
    end Get_Parameter_Count;

    procedure Set_Parameter(Prepared_Query: in out Prepared_Query_Type; Number:Positive; Value:String) is
        use Vec_Pkg;
        use UB;
    begin
        Replace_Element(Prepared_Query.Params, Number, To_Unbounded_String(Value));
    end Set_Parameter;

    procedure Clear(Prepared_Query: in out Prepared_Query_Type) is
        use UB;
    begin
        Exec(Prepared_Query.Connection, "DEALLOCATE " & To_String(Prepared_Query.Prepared_Name));
    end Clear;
end;
