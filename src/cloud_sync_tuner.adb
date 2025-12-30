-- SPDX-License-Identifier: AGPL-3.0-or-later
-- Cloud Sync Tuner - Ada TUI for rclone mount configuration
-- Manages VFS cache modes for cloud storage mounts

with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Command_Line;
with Ada.Directories;
with GNAT.OS_Lib;

procedure Cloud_Sync_Tuner is

   -- ANSI color codes for TUI
   Reset      : constant String := ASCII.ESC & "[0m";
   Bold       : constant String := ASCII.ESC & "[1m";
   Dim        : constant String := ASCII.ESC & "[2m";
   Green      : constant String := ASCII.ESC & "[32m";
   Yellow     : constant String := ASCII.ESC & "[33m";
   Blue       : constant String := ASCII.ESC & "[34m";
   Cyan       : constant String := ASCII.ESC & "[36m";
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

   procedure Clear_Screen is
   begin
      Put (ASCII.ESC & "[2J" & ASCII.ESC & "[H");
   end Clear_Screen;

   procedure Print_Header is
   begin
      Put_Line (BG_Blue & Bold & White);
      Put_Line ("  ╔═══════════════════════════════════════════════════════════════╗  ");
      Put_Line ("  ║          CLOUD SYNC TUNER - rclone Configuration              ║  ");
      Put_Line ("  ╚═══════════════════════════════════════════════════════════════╝  ");
      Put_Line (Reset);
      New_Line;
   end Print_Header;

   procedure Print_Mode_Info (Mode : Cache_Mode; Is_Selected : Boolean) is
      Prefix : constant String := (if Is_Selected then Green & "► " else "  ");
      Suffix : constant String := (if Is_Selected then " ◄" & Reset else Reset);
      Default_Tag : constant String := (if Mode = Writes then Yellow & " [DEFAULT]" & Reset else "");
   begin
      case Mode is
         when Off =>
            Put_Line (Prefix & Bold & "1. Off Mode" & Suffix & Default_Tag);
            Put_Line (Dim & "     No caching - direct cloud access" & Reset);
            Put_Line (Cyan & "     PROS:" & Reset);
            Put_Line ("       • Zero local disk usage");
            Put_Line ("       • Always see latest cloud state");
            Put_Line ("       • Good for read-only browsing");
            Put_Line (Yellow & "     CONS:" & Reset);
            Put_Line ("       • Slowest performance");
            Put_Line ("       • High API usage (rate limit risk)");
            Put_Line ("       • No offline access");
            Put_Line ("       • Every file read hits the network");
            New_Line;

         when Minimal =>
            Put_Line (Prefix & Bold & "2. Minimal Mode" & Suffix & Default_Tag);
            Put_Line (Dim & "     Caches open files only" & Reset);
            Put_Line (Cyan & "     PROS:" & Reset);
            Put_Line ("       • Low disk usage");
            Put_Line ("       • Faster than Off for open files");
            Put_Line ("       • Reduces some API calls");
            Put_Line (Yellow & "     CONS:" & Reset);
            Put_Line ("       • Still many API calls for listings");
            Put_Line ("       • No write buffering");
            Put_Line ("       • Moderate rate limit risk");
            New_Line;

         when Writes =>
            Put_Line (Prefix & Bold & "3. Writes Mode" & Suffix & Default_Tag);
            Put_Line (Dim & "     Caches writes, streams reads" & Reset);
            Put_Line (Cyan & "     PROS:" & Reset);
            Put_Line ("       • " & Green & "Best balance of speed vs API usage" & Reset);
            Put_Line ("       • Write operations are buffered");
            Put_Line ("       • Reduces rate limiting significantly");
            Put_Line ("       • Good for mixed read/write workloads");
            Put_Line (Yellow & "     CONS:" & Reset);
            Put_Line ("       • Moderate disk usage for writes");
            Put_Line ("       • Reads still hit network");
            Put_Line ("       • Small delay before writes sync");
            New_Line;

         when Full =>
            Put_Line (Prefix & Bold & "4. Full Mode" & Suffix & Default_Tag);
            Put_Line (Dim & "     Full local caching" & Reset);
            Put_Line (Cyan & "     PROS:" & Reset);
            Put_Line ("       • Fastest read performance");
            Put_Line ("       • Offline access to cached files");
            Put_Line ("       • Best for frequently accessed files");
            Put_Line (Yellow & "     CONS:" & Reset);
            Put_Line ("       • " & Yellow & "Highest API usage at startup" & Reset);
            Put_Line ("       • " & Yellow & "Most likely to trigger rate limits" & Reset);
            Put_Line ("       • High disk usage");
            Put_Line ("       • Cache can become stale");
            New_Line;
      end case;
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
      Put_Line ("  --tpslimit 4          " & Dim & "(max 4 transactions/second)" & Reset);
      Put_Line ("  --tpslimit-burst 1    " & Dim & "(no burst allowance)" & Reset);
      Put_Line ("  --vfs-read-chunk-size 32M");
      Put_Line ("  --vfs-cache-max-age 72h");
      Put_Line ("  --dir-cache-time 5m");
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

   procedure Generate_Service_File (Service : Cloud_Service; Mode : Cache_Mode) is
      File_Name : constant String :=
         "/tmp/cloud-sync-tuner/output/" & To_String (Service.Service_Name) & ".service";
      File : File_Type;
      Mode_Str : constant String := Mode_To_String (Mode);
   begin
      Create (File, Out_File, File_Name);

      Put_Line (File, "# SPDX-License-Identifier: AGPL-3.0-or-later");
      Put_Line (File, "# Generated by Cloud Sync Tuner");
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
      Put_Line (File, "  --tpslimit 4 \");
      Put_Line (File, "  --tpslimit-burst 1 \");
      Put_Line (File, "  --vfs-read-chunk-size 32M \");
      Put_Line (File, "  --vfs-cache-max-age 72h \");
      Put_Line (File, "  --dir-cache-time 5m");
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
      Put_Line ("  " & Cyan & "[1-4]" & Reset & " Select cache mode    " &
                Cyan & "[A]" & Reset & "pply    " &
                Cyan & "[Q]" & Reset & "uit");
      Put_Line (Bold & "═══════════════════════════════════════════════════════════════" & Reset);
   end Print_Menu;

   procedure Run_TUI is
      Choice : Character;
      Running : Boolean := True;
   begin
      while Running loop
         Clear_Screen;
         Print_Header;
         Print_Services;

         Put_Line (Bold & "VFS Cache Mode Options:" & Reset);
         Put_Line (Dim & "(Higher numbers = more caching, more disk use, potential rate limits)" & Reset);
         New_Line;

         Print_Mode_Info (Off, Current_Mode = Off);
         Print_Mode_Info (Minimal, Current_Mode = Minimal);
         Print_Mode_Info (Writes, Current_Mode = Writes);
         Print_Mode_Info (Full, Current_Mode = Full);

         Print_Rate_Limit_Settings;
         Print_Menu;

         Put ("  Selection: ");
         Get_Immediate (Choice);
         New_Line;

         case Choice is
            when '1' => Current_Mode := Off;
            when '2' => Current_Mode := Minimal;
            when '3' => Current_Mode := Writes;
            when '4' => Current_Mode := Full;
            when 'a' | 'A' =>
               Apply_Configuration (Current_Mode);
               Put ("Press any key to continue...");
               Get_Immediate (Choice);
            when 'q' | 'Q' => Running := False;
            when others => null;
         end case;
      end loop;

      Clear_Screen;
      Put_Line ("Cloud Sync Tuner exited.");
   end Run_TUI;

   -- Command line mode for scripting
   procedure Run_CLI is
      Mode_Arg : constant String := Ada.Command_Line.Argument (1);
   begin
      if Mode_Arg = "off" then
         Current_Mode := Off;
      elsif Mode_Arg = "minimal" then
         Current_Mode := Minimal;
      elsif Mode_Arg = "writes" then
         Current_Mode := Writes;
      elsif Mode_Arg = "full" then
         Current_Mode := Full;
      else
         Put_Line ("Usage: cloud_sync_tuner [off|minimal|writes|full]");
         Put_Line ("       cloud_sync_tuner  (interactive TUI)");
         return;
      end if;

      Apply_Configuration (Current_Mode);
   end Run_CLI;

begin
   if Ada.Command_Line.Argument_Count > 0 then
      Run_CLI;
   else
      Run_TUI;
   end if;
end Cloud_Sync_Tuner;
