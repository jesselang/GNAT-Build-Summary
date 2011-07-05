with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

procedure GNAT_Build_Summary is
   type Summary_ID is (None, Operation); -- The possible types of summary.

   type Summary_Command is (GNAT_Make, GCC, GNAT_List, GNAT_Compile, GNAT_Bind, GNAT_Link); -- Recognized commands.

   type Summary_Info (ID : Summary_ID := None) is record
      case ID is
         when None =>
            null;
         when Operation =>
            Command : Summary_Command;
            File    : Ada.Strings.Unbounded.Unbounded_String;
      end case;
   end record;

   function Parse_Summary (Line : String) return Summary_Info;
   -- Parses Line, returning summary information.

   function Parse_Summary (Line : String) return Summary_Info is
      Result : Summary_Info (ID => Operation);

      Command_Start : Natural;
      Command_End   : Natural;
      Source_Index  : Natural;
      Path_Index    : Natural;
   begin -- Parse_Summary
      if Line (Line'First .. Line'First + 2) = "gcc" then
         Result.Command := GCC;
      else
         Command_Start := Ada.Strings.Fixed.Index (Line, Pattern => "gnat");

         if Command_Start = 0 then
            return (ID => None); -- We can't summarize, so return a result reflecting that.
         else
            if Command_Start /= Line'First or (Command_Start > Line'First and then Line (Command_Start - 1) /= '/') then
               -- Not the beginning of the line, or the path.
               Command_Start := Ada.Strings.Fixed.Index (Line (Command_Start + 1 .. Line'Last), Pattern => "gnat");

               if Command_Start = 0 then
                  return (ID => None); -- We can't summarize, so return a result reflecting that.
               end if;
            end if;

            if Line (Command_Start + 4) = ' ' then -- Skip a space after gnat, if there is one.
               Command_Start := Command_Start + 5;
            else
               Command_Start := Command_Start + 4;
            end if;

            Command_End := Ada.Strings.Fixed.Index (Line (Command_Start .. Line'Last), Pattern => " ");

            if Command_End = 0 then
               return (ID => None); -- We can't summarize, so return a result reflecting that.
            else
               Command_End := Command_End - 1;
            end if;

            if Line (Command_Start .. Command_End) = "make" then
               Result.Command := GNAT_Make;
            elsif Line (Command_Start .. Command_End) = "list" then
               Result.Command := GNAT_List;
            elsif Line (Command_Start .. Command_End) = "compile" then
               Result.Command := GNAT_Compile;
            elsif Line (Command_Start .. Command_End) = "bind" then
               Result.Command := GNAT_Bind;
            elsif Line (Command_Start .. Command_End) = "link" then
               Result.Command := GNAT_Link;
            else
               return (ID => None); -- We can't summarize, so return a result reflecting that.
            end if;
         end if;
      end if;

      Source_Index := Ada.Strings.Fixed.Index (Line, Pattern => ".ad", Going => Ada.Strings.Backward);

      if Source_Index = 0 then
         Source_Index := Ada.Strings.Fixed.Index (Line, Pattern => ".gpr", Going => Ada.Strings.Backward); -- Try a project file.

         if Source_Index = 0 then
            Source_Index := Ada.Strings.Fixed.Index
                               (Line, Pattern => ".ali", Going => Ada.Strings.Backward); -- Try a library file.

            if Source_Index = 0 then
               return (ID => None); -- We can't find what file the operation takes place on.
            end if;
         end if;
      end if;

      Path_Index := Ada.Strings.Fixed.Index (Line (Command_End + 1 .. Line'Last), Pattern => " /");

      if Path_Index = 0 or Path_Index > Source_Index then -- Check for a relative file path.
         Path_Index := Ada.Strings.Fixed.Index (Line (Command_End + 1 .. Line'Last), Pattern => " ");
      end if;

      Result.File := Ada.Strings.Unbounded.To_Unbounded_String (Line (Path_Index + 1 .. Source_Index + 3) );

      return Result;
   end Parse_Summary;

   Line    : String (1 .. 16000);
   Last    : Natural;
   Summary : Summary_Info;
begin
   Each_Line : loop
      exit Each_Line when Ada.Text_IO.End_Of_File (Ada.Text_IO.Current_Input);

      Ada.Text_IO.Get_Line (File => Ada.Text_IO.Current_Input, Item => Line, Last => Last);

      exit Each_Line when Last = Line'First - 1;

      Summary := Parse_Summary (Line => Line (Line'First .. Last) );

      case Summary.ID is
         when None =>
            Ada.Text_IO.Put_Line (Item => Line (Line'First .. Last) );
         when Operation =>
            Ada.Text_IO.Put (Item => "   [" & Summary_Command'Image (Summary.Command) & "]  ");
            Ada.Text_IO.Set_Col (To => 20);
            Ada.Text_IO.Put_Line (Item => Ada.Directories.Simple_Name (Ada.Strings.Unbounded.To_String (Summary.File) ) );
      end case;
   end loop Each_Line;
end GNAT_Build_Summary;

-- Copyright 2011 Solid Rock Data Solutions. All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without modification, are
-- permitted provided that the following conditions are met:
--
--    1. Redistributions of source code must retain the above copyright notice, this list of
--       conditions and the following disclaimer.
--
--    2. Redistributions in binary form must reproduce the above copyright notice, this list
--       of conditions and the following disclaimer in the documentation and/or other materials
--       provided with the distribution.
--
-- THIS SOFTWARE IS PROVIDED BY Solid Rock Data Solutions ``AS IS'' AND ANY EXPRESS OR IMPLIED
-- WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
-- FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL Solid Rock Data Solutions OR
-- CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
-- CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
-- SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
-- ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
-- NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
-- ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--
-- The views and conclusions contained in the software and documentation are those of the
-- authors and should not be interpreted as representing official policies, either expressed
-- or implied, of Solid Rock Data Solutions.
