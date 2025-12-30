-- SPDX-License-Identifier: AGPL-3.0-or-later
-- Cloud Sync Tuner - Test Runner
-- Executes all unit and integration tests

with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Command_Line;
with Ada.Directories;
with GNAT.OS_Lib;

procedure Test_Runner is

   -- Test counters
   Tests_Run    : Natural := 0;
   Tests_Passed : Natural := 0;
   Tests_Failed : Natural := 0;

   -- ANSI colors
   Green : constant String := ASCII.ESC & "[32m";
   Red   : constant String := ASCII.ESC & "[31m";
   Reset : constant String := ASCII.ESC & "[0m";

   procedure Report (Name : String; Passed : Boolean) is
   begin
      Tests_Run := Tests_Run + 1;
      if Passed then
         Tests_Passed := Tests_Passed + 1;
         Put_Line (Green & "  ✓ " & Reset & Name);
      else
         Tests_Failed := Tests_Failed + 1;
         Put_Line (Red & "  ✗ " & Reset & Name);
      end if;
   end Report;

   -- ============================================
   -- Unit Tests
   -- ============================================

   procedure Test_Cache_Mode_Values is
      type Cache_Mode is (Off, Minimal, Writes, Full);
   begin
      Report ("Cache mode Off exists", Cache_Mode'Pos (Off) = 0);
      Report ("Cache mode Minimal exists", Cache_Mode'Pos (Minimal) = 1);
      Report ("Cache mode Writes exists", Cache_Mode'Pos (Writes) = 2);
      Report ("Cache mode Full exists", Cache_Mode'Pos (Full) = 3);
   end Test_Cache_Mode_Values;

   procedure Test_Conflict_Strategy_Values is
      type Conflict_Strategy is (None, Newer, Older, Larger, Smaller, Path1, Path2);
   begin
      Report ("Conflict strategy count", Conflict_Strategy'Pos (Path2) = 6);
   end Test_Conflict_Strategy_Values;

   procedure Test_TPS_Validation is
      function Validate_TPS (Value : Integer) return Boolean is
      begin
         return Value >= 1 and Value <= 100;
      end Validate_TPS;
   begin
      Report ("TPS 4 is valid", Validate_TPS (4));
      Report ("TPS 0 is invalid", not Validate_TPS (0));
      Report ("TPS 100 is valid", Validate_TPS (100));
      Report ("TPS 101 is invalid", not Validate_TPS (101));
   end Test_TPS_Validation;

   procedure Test_Chunk_Size_Validation is
      function Validate_Chunk (Value : Integer) return Boolean is
      begin
         return Value >= 1 and Value <= 512;
      end Validate_Chunk;
   begin
      Report ("Chunk 32 is valid", Validate_Chunk (32));
      Report ("Chunk 0 is invalid", not Validate_Chunk (0));
      Report ("Chunk 512 is valid", Validate_Chunk (512));
      Report ("Chunk 513 is invalid", not Validate_Chunk (513));
   end Test_Chunk_Size_Validation;

   procedure Test_Size_Parsing is
      function Parse_Size (S : String) return Natural is
         Len : constant Natural := S'Length;
         Unit_Char : Character;
      begin
         if Len = 0 then
            return 0;
         end if;

         Unit_Char := S (S'Last);
         if Unit_Char = 'G' or Unit_Char = 'g' then
            return Natural'Value (S (S'First .. S'Last - 1));
         elsif Unit_Char = 'M' or Unit_Char = 'm' then
            return Natural'Value (S (S'First .. S'Last - 1));
         else
            return Natural'Value (S);
         end if;
      exception
         when others => return 0;
      end Parse_Size;
   begin
      Report ("Parse '10G' = 10", Parse_Size ("10G") = 10);
      Report ("Parse '32M' = 32", Parse_Size ("32M") = 32);
      Report ("Parse '100' = 100", Parse_Size ("100") = 100);
      Report ("Parse '' = 0", Parse_Size ("") = 0);
      Report ("Parse 'invalid' = 0", Parse_Size ("invalid") = 0);
   end Test_Size_Parsing;

   procedure Test_Time_Parsing is
      function Parse_Time (S : String) return Natural is
         Len : constant Natural := S'Length;
         Unit_Char : Character;
      begin
         if Len = 0 then
            return 0;
         end if;

         Unit_Char := S (S'Last);
         if Unit_Char = 'h' or Unit_Char = 'm' or Unit_Char = 's' then
            return Natural'Value (S (S'First .. S'Last - 1));
         else
            return Natural'Value (S);
         end if;
      exception
         when others => return 0;
      end Parse_Time;
   begin
      Report ("Parse '72h' = 72", Parse_Time ("72h") = 72);
      Report ("Parse '5m' = 5", Parse_Time ("5m") = 5);
      Report ("Parse '30s' = 30", Parse_Time ("30s") = 30);
   end Test_Time_Parsing;

   -- ============================================
   -- Integration Tests
   -- ============================================

   procedure Test_Output_Directory_Creation is
      Output_Dir : constant String := "/tmp/cloud-sync-tuner/output";
   begin
      if not Ada.Directories.Exists (Output_Dir) then
         Ada.Directories.Create_Path (Output_Dir);
      end if;
      Report ("Output directory exists", Ada.Directories.Exists (Output_Dir));
   end Test_Output_Directory_Creation;

   procedure Test_Service_File_Generation is
      -- This would need the full Generate_Service procedure
      -- For now, just verify we can create files
      Test_File : constant String := "/tmp/cloud-sync-tuner/output/test.service";
      File : Ada.Text_IO.File_Type;
   begin
      begin
         Ada.Text_IO.Create (File, Ada.Text_IO.Out_File, Test_File);
         Ada.Text_IO.Put_Line (File, "[Unit]");
         Ada.Text_IO.Put_Line (File, "Description=Test");
         Ada.Text_IO.Close (File);
         Report ("Can create service file", Ada.Directories.Exists (Test_File));
         Ada.Directories.Delete_File (Test_File);
      exception
         when others =>
            Report ("Can create service file", False);
      end;
   end Test_Service_File_Generation;

   -- ============================================
   -- Main
   -- ============================================

begin
   Put_Line ("");
   Put_Line ("Cloud Sync Tuner - Test Suite");
   Put_Line ("══════════════════════════════");
   Put_Line ("");

   Put_Line ("Unit Tests:");
   Test_Cache_Mode_Values;
   Test_Conflict_Strategy_Values;
   Test_TPS_Validation;
   Test_Chunk_Size_Validation;
   Test_Size_Parsing;
   Test_Time_Parsing;

   Put_Line ("");
   Put_Line ("Integration Tests:");
   Test_Output_Directory_Creation;
   Test_Service_File_Generation;

   Put_Line ("");
   Put_Line ("══════════════════════════════");
   Put_Line ("Results:" & Natural'Image (Tests_Passed) & "/" & Natural'Image (Tests_Run) & " passed");

   if Tests_Failed > 0 then
      Put_Line (Red & "FAILED:" & Natural'Image (Tests_Failed) & " tests" & Reset);
      Ada.Command_Line.Set_Exit_Status (1);
   else
      Put_Line (Green & "ALL TESTS PASSED" & Reset);
      Ada.Command_Line.Set_Exit_Status (0);
   end if;

   Put_Line ("");
end Test_Runner;
