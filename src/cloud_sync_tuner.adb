-- SPDX-License-Identifier: AGPL-3.0-or-later
-- Cloud Sync Tuner - Ada TUI for rclone mount configuration
-- Manages VFS cache modes for cloud storage mounts

with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Command_Line;
with Ada.Directories;
with GNAT.OS_Lib;

procedure Cloud_Sync_Tuner is

   -- Version
   Version : constant String := "0.2.0";

   -- ANSI color codes for TUI
   Reset      : constant String := ASCII.ESC & "[0m";
   Bold       : constant String := ASCII.ESC & "[1m";
   Dim        : constant String := ASCII.ESC & "[2m";
   Underline  : constant String := ASCII.ESC & "[4m";
   Green      : constant String := ASCII.ESC & "[32m";
   Yellow     : constant String := ASCII.ESC & "[33m";
   Blue       : constant String := ASCII.ESC & "[34m";
   Cyan       : constant String := ASCII.ESC & "[36m";
   Red        : constant String := ASCII.ESC & "[31m";
   White      : constant String := ASCII.ESC & "[37m";
   BG_Blue    : constant String := ASCII.ESC & "[44m";

   -- Cache mode enumeration
   type Cache_Mode is (Off, Minimal, Writes, Full);

   -- Cloud service record
   type Cloud_Service is record
      Name         : Unbounded_String;
      Remote       : Unbounded_String;
      Mount_Point  : Unbounded_String;
      Service_Name : Unbounded_String;
      Enabled      : Boolean;
   end record;

   -- Array of cloud services
   type Service_Array is array (Positive range <>) of Cloud_Service;

   -- Rate limit settings
   type Rate_Limit_Config is record
      TPS_Limit       : Positive := 4;
      TPS_Burst       : Positive := 1;
      Chunk_Size_MB   : Positive := 32;
      Cache_Age_Hours : Positive := 72;
      Dir_Cache_Min   : Positive := 5;
   end record;

   -- Configuration for cloud services
   Services : constant Service_Array (1 .. 3) := (
      1 => (To_Unbounded_String ("Dropbox"),
            To_Unbounded_String ("dropbox:"),
            To_Unbounded_String ("/run/media/hyper/eclipse/Cloud/Dropbox"),
            To_Unbounded_String ("rclone-dropbox"),
            True),
      2 => (To_Unbounded_String ("Google Drive"),
            To_Unbounded_String ("gdrive:"),
            To_Unbounded_String ("/run/media/hyper/eclipse/Cloud/GoogleDrive"),
            To_Unbounded_String ("rclone-gdrive"),
            True),
      3 => (To_Unbounded_String ("OneDrive"),
            To_Unbounded_String ("onedrive:"),
            To_Unbounded_String ("/run/media/hyper/eclipse/Cloud/OneDrive"),
            To_Unbounded_String ("rclone-onedrive"),
            True)
   );

   -- Current selection (default to Option 2: Writes mode)
   Current_Mode : Cache_Mode := Writes;
   Rate_Config  : Rate_Limit_Config;

   -- Help context tracking
   type Help_Context is (None, Mode_Select, Service_Select, Rate_Config_Help, Apply_Help);
   Current_Help : Help_Context := None;

   procedure Clear_Screen is
   begin
      Put (ASCII.ESC & "[2J" & ASCII.ESC & "[H");
   end Clear_Screen;

   procedure Print_Header is
   begin
      Put_Line (BG_Blue & Bold & White);
      Put_Line ("  ╔═══════════════════════════════════════════════════════════════╗  ");
      Put_Line ("  ║          CLOUD SYNC TUNER - rclone Configuration              ║  ");
      Put_Line ("  ║                     v" & Version & "                                      ║  ");
      Put_Line ("  ╚═══════════════════════════════════════════════════════════════╝  ");
      Put_Line (Reset);
      New_Line;
   end Print_Header;

   procedure Print_Context_Help is
   begin
      Put_Line (Dim & "───────────────────────────────────────────────────────────────" & Reset);
      case Current_Help is
         when None =>
            Put_Line (Cyan & "  [?]" & Reset & " Press ? for context-sensitive help");

         when Mode_Select =>
            Put_Line (Bold & "  Cache Mode Help:" & Reset);
            Put_Line ("    " & Green & "writes" & Reset & " is recommended for Dropbox (prevents rate limiting)");
            Put_Line ("    " & Yellow & "full" & Reset & " caches aggressively but may trigger API limits");
            Put_Line ("    " & Dim & "off" & Reset & " uses no cache - slowest but always current");
            Put_Line ("    Press " & Cyan & "[1-4]" & Reset & " to select, " & Cyan & "[?]" & Reset & " to hide help");

         when Service_Select =>
            Put_Line (Bold & "  Service Configuration Help:" & Reset);
            Put_Line ("    Services are defined in config/config.toml");
            Put_Line ("    Edit mount points and remote names there");
            Put_Line ("    " & Green & "●" & Reset & " = enabled, " & Dim & "○" & Reset & " = disabled");

         when Rate_Config_Help =>
            Put_Line (Bold & "  Rate Limiting Help:" & Reset);
            Put_Line ("    " & Cyan & "TPS Limit" & Reset & ": Max API calls per second (Dropbox: 4)");
            Put_Line ("    " & Cyan & "TPS Burst" & Reset & ": Extra calls allowed in burst (keep at 1)");
            Put_Line ("    " & Cyan & "Chunk Size" & Reset & ": Read size per request (32MB optimal)");
            Put_Line ("    " & Cyan & "Cache Age" & Reset & ": How long to keep cached data (72h default)");

         when Apply_Help =>
            Put_Line (Bold & "  Apply Help:" & Reset);
            Put_Line ("    Generated files go to /tmp/cloud-sync-tuner/output/");
            Put_Line ("    Copy to ~/.config/systemd/user/ and reload daemon");
            Put_Line ("    Use --apply to auto-copy and reload (requires sudo for some ops)");
      end case;
      Put_Line (Dim & "───────────────────────────────────────────────────────────────" & Reset);
   end Print_Context_Help;

   procedure Print_Mode_Info (Mode : Cache_Mode; Is_Selected : Boolean) is
      Prefix : constant String := (if Is_Selected then Green & "► " else "  ");
      Suffix : constant String := (if Is_Selected then " ◄" & Reset else Reset);
      Default_Tag : constant String := (if Mode = Writes then Yellow & " [DEFAULT]" & Reset else "");
      Checkbox : constant String := (if Is_Selected then "[×]" else "[ ]");
   begin
      case Mode is
         when Off =>
            Put_Line (Prefix & Checkbox & Bold & " 1. Off Mode" & Suffix & Default_Tag);
            Put_Line (Dim & "       No caching - direct cloud access" & Reset);
            Put_Line ("       API: " & Red & "🔴 Very High" & Reset & "  Disk: None");

         when Minimal =>
            Put_Line (Prefix & Checkbox & Bold & " 2. Minimal Mode" & Suffix & Default_Tag);
            Put_Line (Dim & "       Caches open files only" & Reset);
            Put_Line ("       API: " & Yellow & "🟠 High" & Reset & "  Disk: Low");

         when Writes =>
            Put_Line (Prefix & Checkbox & Bold & " 3. Writes Mode" & Suffix & Default_Tag);
            Put_Line (Dim & "       Caches writes, streams reads" & Reset);
            Put_Line ("       API: " & Green & "🟢 Low" & Reset & "  Disk: Medium");

         when Full =>
            Put_Line (Prefix & Checkbox & Bold & " 4. Full Mode" & Suffix & Default_Tag);
            Put_Line (Dim & "       Full local caching" & Reset);
            Put_Line ("       API: " & Red & "🔴 Very High" & Reset & "  Disk: High");
      end case;
      New_Line;
   end Print_Mode_Info;

   procedure Print_Services is
   begin
      Put_Line (Bold & "Connected Cloud Services:" & Reset);
      for I in Services'Range loop
         Put ("  ");
         if Services (I).Enabled then
            Put (Green & "●" & Reset);
         else
            Put (Dim & "○" & Reset);
         end if;
         Put (" " & To_String (Services (I).Name));
         Put_Line (Dim & " → " & To_String (Services (I).Mount_Point) & Reset);
      end loop;
      New_Line;
   end Print_Services;

   procedure Print_Rate_Limit_Settings is
   begin
      Put_Line (Bold & "Rate Limiting Configuration:" & Reset);
      Put_Line ("  --tpslimit " & Integer'Image (Rate_Config.TPS_Limit) &
                "         " & Dim & "(max transactions/second)" & Reset);
      Put_Line ("  --tpslimit-burst " & Integer'Image (Rate_Config.TPS_Burst) &
                "  " & Dim & "(burst allowance)" & Reset);
      Put_Line ("  --vfs-read-chunk-size " & Integer'Image (Rate_Config.Chunk_Size_MB) & "M");
      Put_Line ("  --vfs-cache-max-age " & Integer'Image (Rate_Config.Cache_Age_Hours) & "h");
      Put_Line ("  --dir-cache-time " & Integer'Image (Rate_Config.Dir_Cache_Min) & "m");
      New_Line;
   end Print_Rate_Limit_Settings;

   function Mode_To_String (Mode : Cache_Mode) return String is
   begin
      case Mode is
         when Off     => return "off";
         when Minimal => return "minimal";
         when Writes  => return "writes";
         when Full    => return "full";
      end case;
   end Mode_To_String;

   function Validate_Mode (S : String) return Boolean is
   begin
      return S = "off" or S = "minimal" or S = "writes" or S = "full";
   end Validate_Mode;

   function Validate_TPS (V : Integer) return Boolean is
   begin
      return V >= 1 and V <= 100;
   end Validate_TPS;

   function Validate_Chunk_Size (V : Integer) return Boolean is
   begin
      return V >= 1 and V <= 512;
   end Validate_Chunk_Size;

   procedure Print_Validation_Error (Field : String; Value : String; Valid_Range : String) is
   begin
      Put_Line (Red & "Error: " & Reset & "Invalid value for " & Bold & Field & Reset);
      Put_Line ("  Got: " & Value);
      Put_Line ("  Valid: " & Valid_Range);
   end Print_Validation_Error;

   procedure Generate_Service_File (Service : Cloud_Service; Mode : Cache_Mode) is
      File_Name : constant String :=
         "/tmp/cloud-sync-tuner/output/" & To_String (Service.Service_Name) & ".service";
      File : File_Type;
      Mode_Str : constant String := Mode_To_String (Mode);
   begin
      Create (File, Out_File, File_Name);

      Put_Line (File, "# SPDX-License-Identifier: AGPL-3.0-or-later");
      Put_Line (File, "# Generated by Cloud Sync Tuner v" & Version);
      Put_Line (File, "# Cache Mode: " & Mode_Str);
      Put_Line (File, "[Unit]");
      Put_Line (File, "Description=" & To_String (Service.Name) & " FUSE Mount");
      Put_Line (File, "After=network-online.target");
      Put_Line (File, "Wants=network-online.target");
      Put_Line (File, "");
      Put_Line (File, "[Service]");
      Put_Line (File, "Type=notify");
      Put_Line (File, "ExecStart=/home/hyper/.local/bin/rclone mount " &
                To_String (Service.Remote) & " " &
                To_String (Service.Mount_Point) & " \");
      Put_Line (File, "  --vfs-cache-mode " & Mode_Str & " \");
      Put_Line (File, "  --tpslimit" & Integer'Image (Rate_Config.TPS_Limit) & " \");
      Put_Line (File, "  --tpslimit-burst" & Integer'Image (Rate_Config.TPS_Burst) & " \");
      Put_Line (File, "  --vfs-read-chunk-size " & Integer'Image (Rate_Config.Chunk_Size_MB) & "M \");
      Put_Line (File, "  --vfs-cache-max-age " & Integer'Image (Rate_Config.Cache_Age_Hours) & "h \");
      Put_Line (File, "  --dir-cache-time " & Integer'Image (Rate_Config.Dir_Cache_Min) & "m");
      Put_Line (File, "ExecStop=/bin/fusermount -uz " & To_String (Service.Mount_Point));
      Put_Line (File, "Restart=on-failure");
      Put_Line (File, "RestartSec=5");
      Put_Line (File, "");
      Put_Line (File, "[Install]");
      Put_Line (File, "WantedBy=default.target");

      Close (File);
   end Generate_Service_File;

   procedure Apply_Configuration (Mode : Cache_Mode) is
   begin
      -- Create output directory
      Ada.Directories.Create_Path ("/tmp/cloud-sync-tuner/output");

      Put_Line (Bold & "Generating service files for " & Mode_To_String (Mode) & " mode..." & Reset);
      New_Line;

      for I in Services'Range loop
         if Services (I).Enabled then
            Generate_Service_File (Services (I), Mode);
            Put_Line (Green & "  ✓ " & Reset & To_String (Services (I).Service_Name) & ".service");
         end if;
      end loop;

      New_Line;
      Put_Line (Bold & "Service files generated in: " & Reset & "/tmp/cloud-sync-tuner/output/");
      New_Line;
      Put_Line ("To apply, run:");
      Put_Line (Cyan & "  cp /tmp/cloud-sync-tuner/output/*.service ~/.config/systemd/user/" & Reset);
      Put_Line (Cyan & "  systemctl --user daemon-reload" & Reset);
      Put_Line (Cyan & "  systemctl --user restart rclone-dropbox rclone-gdrive rclone-onedrive" & Reset);
      New_Line;
   end Apply_Configuration;

   procedure Print_Menu is
   begin
      Put_Line (Bold & "═══════════════════════════════════════════════════════════════" & Reset);
      Put_Line ("  " & Cyan & "[1-4]" & Reset & " Select mode  " &
                Cyan & "[R]" & Reset & "ate config  " &
                Cyan & "[A]" & Reset & "pply  " &
                Cyan & "[?]" & Reset & " Help  " &
                Cyan & "[Q]" & Reset & "uit");
      Put_Line (Bold & "═══════════════════════════════════════════════════════════════" & Reset);
   end Print_Menu;

   procedure Edit_Rate_Config is
      Choice : Character;
      Input_Line : String (1 .. 10);
      Last : Natural;
      Value : Integer;
   begin
      loop
         Clear_Screen;
         Print_Header;
         Put_Line (Bold & "Rate Limit Configuration:" & Reset);
         New_Line;
         Put_Line ("  [1] TPS Limit:      " & Integer'Image (Rate_Config.TPS_Limit) &
                   Dim & " (1-100, Dropbox safe: 4)" & Reset);
         Put_Line ("  [2] TPS Burst:      " & Integer'Image (Rate_Config.TPS_Burst) &
                   Dim & " (1-10, recommended: 1)" & Reset);
         Put_Line ("  [3] Chunk Size MB:  " & Integer'Image (Rate_Config.Chunk_Size_MB) &
                   Dim & " (1-512, optimal: 32)" & Reset);
         Put_Line ("  [4] Cache Age hrs:  " & Integer'Image (Rate_Config.Cache_Age_Hours) &
                   Dim & " (1-168, default: 72)" & Reset);
         Put_Line ("  [5] Dir Cache min:  " & Integer'Image (Rate_Config.Dir_Cache_Min) &
                   Dim & " (1-60, default: 5)" & Reset);
         New_Line;
         Put_Line ("  [D] Reset to Dropbox-safe defaults");
         Put_Line ("  [B] Back to main menu");
         New_Line;
         Put ("  Selection: ");
         Get_Immediate (Choice);
         New_Line;

         case Choice is
            when '1' =>
               Put ("  Enter TPS Limit (1-100): ");
               Get_Line (Input_Line, Last);
               begin
                  Value := Integer'Value (Input_Line (1 .. Last));
                  if Validate_TPS (Value) then
                     Rate_Config.TPS_Limit := Value;
                  else
                     Print_Validation_Error ("TPS Limit", Input_Line (1 .. Last), "1-100");
                     delay 2.0;
                  end if;
               exception
                  when others =>
                     Print_Validation_Error ("TPS Limit", Input_Line (1 .. Last), "integer 1-100");
                     delay 2.0;
               end;

            when '2' =>
               Put ("  Enter TPS Burst (1-10): ");
               Get_Line (Input_Line, Last);
               begin
                  Value := Integer'Value (Input_Line (1 .. Last));
                  if Value >= 1 and Value <= 10 then
                     Rate_Config.TPS_Burst := Value;
                  else
                     Print_Validation_Error ("TPS Burst", Input_Line (1 .. Last), "1-10");
                     delay 2.0;
                  end if;
               exception
                  when others =>
                     Print_Validation_Error ("TPS Burst", Input_Line (1 .. Last), "integer 1-10");
                     delay 2.0;
               end;

            when '3' =>
               Put ("  Enter Chunk Size MB (1-512): ");
               Get_Line (Input_Line, Last);
               begin
                  Value := Integer'Value (Input_Line (1 .. Last));
                  if Validate_Chunk_Size (Value) then
                     Rate_Config.Chunk_Size_MB := Value;
                  else
                     Print_Validation_Error ("Chunk Size", Input_Line (1 .. Last), "1-512 MB");
                     delay 2.0;
                  end if;
               exception
                  when others =>
                     Print_Validation_Error ("Chunk Size", Input_Line (1 .. Last), "integer 1-512");
                     delay 2.0;
               end;

            when '4' =>
               Put ("  Enter Cache Age hours (1-168): ");
               Get_Line (Input_Line, Last);
               begin
                  Value := Integer'Value (Input_Line (1 .. Last));
                  if Value >= 1 and Value <= 168 then
                     Rate_Config.Cache_Age_Hours := Value;
                  else
                     Print_Validation_Error ("Cache Age", Input_Line (1 .. Last), "1-168 hours");
                     delay 2.0;
                  end if;
               exception
                  when others =>
                     Print_Validation_Error ("Cache Age", Input_Line (1 .. Last), "integer 1-168");
                     delay 2.0;
               end;

            when '5' =>
               Put ("  Enter Dir Cache minutes (1-60): ");
               Get_Line (Input_Line, Last);
               begin
                  Value := Integer'Value (Input_Line (1 .. Last));
                  if Value >= 1 and Value <= 60 then
                     Rate_Config.Dir_Cache_Min := Value;
                  else
                     Print_Validation_Error ("Dir Cache", Input_Line (1 .. Last), "1-60 minutes");
                     delay 2.0;
                  end if;
               exception
                  when others =>
                     Print_Validation_Error ("Dir Cache", Input_Line (1 .. Last), "integer 1-60");
                     delay 2.0;
               end;

            when 'd' | 'D' =>
               Rate_Config := (TPS_Limit => 4, TPS_Burst => 1,
                               Chunk_Size_MB => 32, Cache_Age_Hours => 72,
                               Dir_Cache_Min => 5);
               Put_Line (Green & "  Reset to Dropbox-safe defaults" & Reset);
               delay 1.0;

            when 'b' | 'B' =>
               exit;

            when others =>
               null;
         end case;
      end loop;
   end Edit_Rate_Config;

   procedure Run_TUI is
      Choice : Character;
      Running : Boolean := True;
   begin
      while Running loop
         Clear_Screen;
         Print_Header;
         Print_Services;

         Put_Line (Bold & "VFS Cache Mode Options:" & Reset);
         Put_Line (Dim & "(Select with 1-4, checkmark shows current selection)" & Reset);
         New_Line;

         Print_Mode_Info (Off, Current_Mode = Off);
         Print_Mode_Info (Minimal, Current_Mode = Minimal);
         Print_Mode_Info (Writes, Current_Mode = Writes);
         Print_Mode_Info (Full, Current_Mode = Full);

         Print_Rate_Limit_Settings;

         if Current_Help /= None then
            Print_Context_Help;
         end if;

         Print_Menu;

         Put ("  Selection: ");
         Get_Immediate (Choice);
         New_Line;

         case Choice is
            when '1' =>
               Current_Mode := Off;
               Current_Help := Mode_Select;
            when '2' =>
               Current_Mode := Minimal;
               Current_Help := Mode_Select;
            when '3' =>
               Current_Mode := Writes;
               Current_Help := Mode_Select;
            when '4' =>
               Current_Mode := Full;
               Current_Help := Mode_Select;
            when 'r' | 'R' =>
               Current_Help := Rate_Config_Help;
               Edit_Rate_Config;
            when 'a' | 'A' =>
               Current_Help := Apply_Help;
               Apply_Configuration (Current_Mode);
               Put ("Press any key to continue...");
               Get_Immediate (Choice);
            when '?' =>
               Current_Help := (if Current_Help = None then Mode_Select else None);
            when 'q' | 'Q' => Running := False;
            when others => null;
         end case;
      end loop;

      Clear_Screen;
      Put_Line ("Cloud Sync Tuner exited.");
   end Run_TUI;

   procedure Print_Help is
   begin
      Put_Line ("Cloud Sync Tuner v" & Version);
      Put_Line ("Ada TUI for rclone cloud mount configuration with rate limiting");
      New_Line;
      Put_Line (Bold & "USAGE:" & Reset);
      Put_Line ("  cloud_sync_tuner                    Interactive TUI mode");
      Put_Line ("  cloud_sync_tuner <mode>             Apply mode directly");
      Put_Line ("  cloud_sync_tuner --help, -h         Show this help");
      Put_Line ("  cloud_sync_tuner --version, -v      Show version");
      Put_Line ("  cloud_sync_tuner --validate <mode>  Validate mode name");
      New_Line;
      Put_Line (Bold & "MODES:" & Reset);
      Put_Line ("  " & Cyan & "off" & Reset & "       No caching - direct cloud access");
      Put_Line ("  " & Cyan & "minimal" & Reset & "   Cache open files only");
      Put_Line ("  " & Cyan & "writes" & Reset & "    Cache writes, stream reads " & Yellow & "(recommended)" & Reset);
      Put_Line ("  " & Cyan & "full" & Reset & "      Full local caching");
      New_Line;
      Put_Line (Bold & "RATE LIMITING:" & Reset);
      Put_Line ("  Default settings are optimized for Dropbox (4 TPS, 1 burst).");
      Put_Line ("  Use TUI [R] option to customize rate limits.");
      New_Line;
      Put_Line (Bold & "EXAMPLES:" & Reset);
      Put_Line ("  cloud_sync_tuner writes     Generate service files with writes mode");
      Put_Line ("  cloud_sync_tuner --validate full");
      Put_Line ("                              Validates 'full' is a valid mode");
      New_Line;
      Put_Line (Bold & "FILES:" & Reset);
      Put_Line ("  ~/.config/cloud-sync-tuner/config.toml   Configuration");
      Put_Line ("  /tmp/cloud-sync-tuner/output/            Generated services");
      New_Line;
      Put_Line (Bold & "SEE ALSO:" & Reset);
      Put_Line ("  man cloud-sync-tuner        Full manual page");
      Put_Line ("  rclone(1), systemctl(1)");
      New_Line;
      Put_Line (Bold & "REPORTING BUGS:" & Reset);
      Put_Line ("  https://github.com/hyperpolymath/cloud-sync-tuner/issues");
   end Print_Help;

   procedure Print_Version is
   begin
      Put_Line ("cloud-sync-tuner " & Version);
      Put_Line ("Copyright (C) 2025 hyperpolymath");
      Put_Line ("License: AGPL-3.0-or-later");
   end Print_Version;

   -- Command line mode for scripting
   procedure Run_CLI is
      Arg1 : constant String := Ada.Command_Line.Argument (1);
   begin
      -- Help options
      if Arg1 = "--help" or Arg1 = "-h" then
         Print_Help;
         return;
      end if;

      -- Version option
      if Arg1 = "--version" or Arg1 = "-v" then
         Print_Version;
         return;
      end if;

      -- Validate option
      if Arg1 = "--validate" then
         if Ada.Command_Line.Argument_Count < 2 then
            Put_Line (Red & "Error: --validate requires a mode argument" & Reset);
            Put_Line ("Usage: cloud_sync_tuner --validate <mode>");
            Ada.Command_Line.Set_Exit_Status (1);
            return;
         end if;
         declare
            Mode_Arg : constant String := Ada.Command_Line.Argument (2);
         begin
            if Validate_Mode (Mode_Arg) then
               Put_Line (Green & "✓ Valid mode: " & Mode_Arg & Reset);
            else
               Put_Line (Red & "✗ Invalid mode: " & Mode_Arg & Reset);
               Put_Line ("Valid modes: off, minimal, writes, full");
               Ada.Command_Line.Set_Exit_Status (1);
            end if;
         end;
         return;
      end if;

      -- Mode argument
      if Validate_Mode (Arg1) then
         if Arg1 = "off" then
            Current_Mode := Off;
         elsif Arg1 = "minimal" then
            Current_Mode := Minimal;
         elsif Arg1 = "writes" then
            Current_Mode := Writes;
         elsif Arg1 = "full" then
            Current_Mode := Full;
         end if;
         Apply_Configuration (Current_Mode);
      else
         Put_Line (Red & "Error: Invalid mode '" & Arg1 & "'" & Reset);
         Put_Line ("Valid modes: off, minimal, writes, full");
         Put_Line ("Try 'cloud_sync_tuner --help' for more information.");
         Ada.Command_Line.Set_Exit_Status (1);
      end if;
   end Run_CLI;

begin
   if Ada.Command_Line.Argument_Count > 0 then
      Run_CLI;
   else
      Run_TUI;
   end if;
end Cloud_Sync_Tuner;
