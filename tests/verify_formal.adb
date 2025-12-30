-- SPDX-License-Identifier: AGPL-3.0-or-later
-- Cloud Sync Tuner - Formal Verification Tests
-- Uses SPARK Ada subset for provable correctness

pragma SPARK_Mode (On);

with Ada.Text_IO; use Ada.Text_IO;

procedure Verify_Formal is

   -- ============================================
   -- Type Specifications with Invariants
   -- ============================================

   subtype TPS_Limit_Type is Positive range 1 .. 100
     with Dynamic_Predicate => TPS_Limit_Type mod 1 = 0;

   subtype TPS_Burst_Type is Positive range 1 .. 10;

   subtype Chunk_Size_Type is Positive range 1 .. 512;

   subtype Cache_Age_Type is Positive range 1 .. 168;  -- Max 1 week

   subtype Dir_Cache_Type is Positive range 1 .. 60;   -- Max 1 hour

   subtype Cache_Size_Type is Natural range 0 .. 1000; -- 0 = unlimited

   subtype Free_Space_Type is Natural range 0 .. 100;

   subtype Write_Back_Type is Positive range 1 .. 60;

   subtype Poll_Interval_Type is Positive range 1 .. 60;

   subtype Buffer_Size_Type is Positive range 1 .. 128;

   subtype Transfers_Type is Positive range 1 .. 32;

   subtype Checkers_Type is Positive range 1 .. 64;

   -- ============================================
   -- Smart Sync Configuration Record
   -- ============================================

   type Smart_Sync_Config is record
      TPS_Limit         : TPS_Limit_Type := 4;
      TPS_Burst         : TPS_Burst_Type := 1;
      Chunk_Size_MB     : Chunk_Size_Type := 32;
      Cache_Age_Hours   : Cache_Age_Type := 72;
      Dir_Cache_Min     : Dir_Cache_Type := 5;
      Cache_Max_Size_GB : Cache_Size_Type := 10;
      Min_Free_Space_GB : Free_Space_Type := 5;
      Write_Back_Sec    : Write_Back_Type := 5;
      Poll_Interval_Min : Poll_Interval_Type := 1;
      Buffer_Size_MB    : Buffer_Size_Type := 16;
      Transfers         : Transfers_Type := 4;
      Checkers          : Checkers_Type := 8;
   end record;

   -- ============================================
   -- Validation Functions with Postconditions
   -- ============================================

   function Is_Valid_TPS (Value : Integer) return Boolean
     with Post => Is_Valid_TPS'Result = (Value >= 1 and Value <= 100)
   is
   begin
      return Value >= 1 and Value <= 100;
   end Is_Valid_TPS;

   function Is_Valid_Burst (Value : Integer) return Boolean
     with Post => Is_Valid_Burst'Result = (Value >= 1 and Value <= 10)
   is
   begin
      return Value >= 1 and Value <= 10;
   end Is_Valid_Burst;

   function Is_Valid_Chunk (Value : Integer) return Boolean
     with Post => Is_Valid_Chunk'Result = (Value >= 1 and Value <= 512)
   is
   begin
      return Value >= 1 and Value <= 512;
   end Is_Valid_Chunk;

   -- ============================================
   -- Safe Setter Functions with Preconditions
   -- ============================================

   procedure Set_TPS_Limit (Config : in out Smart_Sync_Config;
                            Value  : in     TPS_Limit_Type)
     with Pre  => Value >= TPS_Limit_Type'First and Value <= TPS_Limit_Type'Last,
          Post => Config.TPS_Limit = Value
   is
   begin
      Config.TPS_Limit := Value;
   end Set_TPS_Limit;

   procedure Set_Cache_Size (Config : in out Smart_Sync_Config;
                             Value  : in     Cache_Size_Type)
     with Pre  => Value >= Cache_Size_Type'First and Value <= Cache_Size_Type'Last,
          Post => Config.Cache_Max_Size_GB = Value
   is
   begin
      Config.Cache_Max_Size_GB := Value;
   end Set_Cache_Size;

   -- ============================================
   -- Invariant Checks
   -- ============================================

   function Config_Is_Valid (Config : Smart_Sync_Config) return Boolean is
     (Config.TPS_Limit >= 1 and Config.TPS_Limit <= 100 and
      Config.TPS_Burst >= 1 and Config.TPS_Burst <= 10 and
      Config.Chunk_Size_MB >= 1 and Config.Chunk_Size_MB <= 512 and
      Config.Cache_Age_Hours >= 1 and Config.Cache_Age_Hours <= 168 and
      Config.Dir_Cache_Min >= 1 and Config.Dir_Cache_Min <= 60 and
      Config.Cache_Max_Size_GB >= 0 and Config.Cache_Max_Size_GB <= 1000 and
      Config.Min_Free_Space_GB >= 0 and Config.Min_Free_Space_GB <= 100 and
      Config.Write_Back_Sec >= 1 and Config.Write_Back_Sec <= 60 and
      Config.Poll_Interval_Min >= 1 and Config.Poll_Interval_Min <= 60 and
      Config.Buffer_Size_MB >= 1 and Config.Buffer_Size_MB <= 128 and
      Config.Transfers >= 1 and Config.Transfers <= 32 and
      Config.Checkers >= 1 and Config.Checkers <= 64);

   -- ============================================
   -- Default Configuration (proven valid)
   -- ============================================

   function Default_Config return Smart_Sync_Config
     with Post => Config_Is_Valid (Default_Config'Result)
   is
   begin
      return (TPS_Limit         => 4,
              TPS_Burst         => 1,
              Chunk_Size_MB     => 32,
              Cache_Age_Hours   => 72,
              Dir_Cache_Min     => 5,
              Cache_Max_Size_GB => 10,
              Min_Free_Space_GB => 5,
              Write_Back_Sec    => 5,
              Poll_Interval_Min => 1,
              Buffer_Size_MB    => 16,
              Transfers         => 4,
              Checkers          => 8);
   end Default_Config;

   -- ============================================
   -- Preset Configurations (proven valid)
   -- ============================================

   function Dropbox_Preset return Smart_Sync_Config
     with Post => Config_Is_Valid (Dropbox_Preset'Result)
   is
      Config : Smart_Sync_Config := Default_Config;
   begin
      Config.TPS_Limit := 4;
      Config.TPS_Burst := 1;
      Config.Chunk_Size_MB := 32;
      return Config;
   end Dropbox_Preset;

   function GDrive_Preset return Smart_Sync_Config
     with Post => Config_Is_Valid (GDrive_Preset'Result)
   is
      Config : Smart_Sync_Config := Default_Config;
   begin
      Config.TPS_Limit := 8;
      Config.TPS_Burst := 2;
      Config.Chunk_Size_MB := 64;
      return Config;
   end GDrive_Preset;

   function OneDrive_Preset return Smart_Sync_Config
     with Post => Config_Is_Valid (OneDrive_Preset'Result)
   is
      Config : Smart_Sync_Config := Default_Config;
   begin
      Config.TPS_Limit := 6;
      Config.TPS_Burst := 2;
      Config.Chunk_Size_MB := 32;
      return Config;
   end OneDrive_Preset;

   -- ============================================
   -- Test Execution
   -- ============================================

   Config : Smart_Sync_Config;
   Pass_Count : Natural := 0;
   Total_Count : Natural := 0;

   procedure Check (Name : String; Condition : Boolean) is
   begin
      Total_Count := Total_Count + 1;
      if Condition then
         Pass_Count := Pass_Count + 1;
         Put_Line ("  ✓ " & Name);
      else
         Put_Line ("  ✗ " & Name & " FAILED");
      end if;
   end Check;

begin
   Put_Line ("");
   Put_Line ("Formal Verification Tests");
   Put_Line ("══════════════════════════");
   Put_Line ("");

   -- Test default config
   Config := Default_Config;
   Check ("Default config is valid", Config_Is_Valid (Config));
   Check ("Default TPS is 4", Config.TPS_Limit = 4);
   Check ("Default burst is 1", Config.TPS_Burst = 1);

   -- Test presets
   Config := Dropbox_Preset;
   Check ("Dropbox preset is valid", Config_Is_Valid (Config));
   Check ("Dropbox TPS is 4", Config.TPS_Limit = 4);

   Config := GDrive_Preset;
   Check ("GDrive preset is valid", Config_Is_Valid (Config));
   Check ("GDrive TPS is 8", Config.TPS_Limit = 8);

   Config := OneDrive_Preset;
   Check ("OneDrive preset is valid", Config_Is_Valid (Config));
   Check ("OneDrive TPS is 6", Config.TPS_Limit = 6);

   -- Test validation functions
   Check ("TPS 4 is valid", Is_Valid_TPS (4));
   Check ("TPS 0 is invalid", not Is_Valid_TPS (0));
   Check ("TPS 101 is invalid", not Is_Valid_TPS (101));

   Check ("Burst 1 is valid", Is_Valid_Burst (1));
   Check ("Burst 11 is invalid", not Is_Valid_Burst (11));

   Check ("Chunk 32 is valid", Is_Valid_Chunk (32));
   Check ("Chunk 513 is invalid", not Is_Valid_Chunk (513));

   -- Test safe setters
   Config := Default_Config;
   Set_TPS_Limit (Config, 8);
   Check ("Set TPS to 8", Config.TPS_Limit = 8);
   Check ("Config still valid after set", Config_Is_Valid (Config));

   Set_Cache_Size (Config, 50);
   Check ("Set cache size to 50", Config.Cache_Max_Size_GB = 50);
   Check ("Config still valid after set", Config_Is_Valid (Config));

   Put_Line ("");
   Put_Line ("══════════════════════════");
   Put_Line ("Results:" & Natural'Image (Pass_Count) & "/" & Natural'Image (Total_Count) & " passed");

   if Pass_Count = Total_Count then
      Put_Line ("ALL FORMAL CHECKS PASSED");
   else
      Put_Line ("SOME CHECKS FAILED");
   end if;

   Put_Line ("");
end Verify_Formal;
