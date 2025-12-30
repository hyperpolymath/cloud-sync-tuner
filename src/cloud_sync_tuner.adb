-- SPDX-License-Identifier: AGPL-3.0-or-later
-- Cloud Sync Tuner - Ada TUI for rclone mount configuration
-- Manages VFS cache modes, smart sync, and pinned folders

with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Command_Line;
with Ada.Directories;
with GNAT.OS_Lib;

procedure Cloud_Sync_Tuner is

   -- Version
   Version : constant String := "1.0.0";

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

   -- Pinned folder record
   type Pinned_Folder is record
      Remote_Path : Unbounded_String;
      Local_Path  : Unbounded_String;
      Service_Idx : Positive;
      Enabled     : Boolean;
   end record;

   type Pinned_Array is array (1 .. 20) of Pinned_Folder;

   -- Conflict resolution strategy
   type Conflict_Strategy is (None, Newer, Older, Larger, Smaller, Path1, Path2);

   -- Bandwidth schedule entry
   type Schedule_Entry is record
      Start_Hour : Natural := 0;
      End_Hour   : Natural := 24;
      BW_Limit   : Natural := 0;  -- 0 = unlimited, else KB/s
   end record;

   type Schedule_Array is array (1 .. 4) of Schedule_Entry;

   -- Rate limit and smart sync settings
   type Smart_Sync_Config is record
      -- Rate limiting
      TPS_Limit         : Positive := 4;
      TPS_Burst         : Positive := 1;
      Chunk_Size_MB     : Positive := 32;
      Cache_Age_Hours   : Positive := 72;
      Dir_Cache_Min     : Positive := 5;
      -- Smart sync (like native clients)
      Cache_Max_Size_GB : Natural := 10;       -- 0 = unlimited
      Min_Free_Space_GB : Natural := 5;        -- Auto-evict if disk low
      Write_Back_Sec    : Positive := 5;       -- Buffer writes before upload
      Poll_Interval_Min : Positive := 1;       -- Check remote changes
      -- Advanced
      Buffer_Size_MB    : Positive := 16;
      Transfers         : Positive := 4;
      Checkers          : Positive := 8;
      -- Conflict resolution (bisync)
      Conflict_Resolve  : Conflict_Strategy := Newer;
      -- Bandwidth scheduling
      BW_Schedule       : Schedule_Array := (
         1 => (Start_Hour => 9, End_Hour => 17, BW_Limit => 5000),   -- Work hours: 5MB/s
         2 => (Start_Hour => 17, End_Hour => 9, BW_Limit => 0),      -- Off hours: unlimited
         others => (Start_Hour => 0, End_Hour => 0, BW_Limit => 0)
      );
      BW_Schedule_Enabled : Boolean := False;
      -- Desktop integration
      Desktop_Notify    : Boolean := True;
      Tray_Icon         : Boolean := True;
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
   Current_Mode   : Cache_Mode := Writes;
   Smart_Config   : Smart_Sync_Config;
   Pinned_Folders : Pinned_Array;
   Pinned_Count   : Natural := 0;
   Offline_Dir    : constant String := "/home/hyper/Offline";

   -- Help context tracking
   type Help_Context is (None, Mode_Select, Service_Select, Rate_Config_Help,
                         Smart_Sync_Help, Pinned_Help, Apply_Help);
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

         when Smart_Sync_Help =>
            Put_Line (Bold & "  Smart Sync Help (like Dropbox/OneDrive native):" & Reset);
            Put_Line ("    " & Cyan & "Cache Max Size" & Reset & ": Auto-evict old files when exceeded");
            Put_Line ("    " & Cyan & "Min Free Space" & Reset & ": Emergency eviction threshold");
            Put_Line ("    " & Cyan & "Write-Back" & Reset & ": Buffer writes before uploading");
            Put_Line ("    " & Cyan & "Poll Interval" & Reset & ": How often to check for remote changes");

         when Pinned_Help =>
            Put_Line (Bold & "  Pinned Folders Help (Offline Access):" & Reset);
            Put_Line ("    Pin folders to always keep them downloaded locally");
            Put_Line ("    Like 'Make Available Offline' in native clients");
            Put_Line ("    Syncs to " & Offline_Dir & "/");
            Put_Line ("    Creates systemd timer for automatic updates");

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
            Put_Line ("       API: " & Red & "Very High" & Reset & "  Disk: None");

         when Minimal =>
            Put_Line (Prefix & Checkbox & Bold & " 2. Minimal Mode" & Suffix & Default_Tag);
            Put_Line (Dim & "       Caches open files only" & Reset);
            Put_Line ("       API: " & Yellow & "High" & Reset & "  Disk: Low");

         when Writes =>
            Put_Line (Prefix & Checkbox & Bold & " 3. Writes Mode" & Suffix & Default_Tag);
            Put_Line (Dim & "       Caches writes, streams reads" & Reset);
            Put_Line ("       API: " & Green & "Low" & Reset & "  Disk: Medium");

         when Full =>
            Put_Line (Prefix & Checkbox & Bold & " 4. Full Mode" & Suffix & Default_Tag);
            Put_Line (Dim & "       Full local caching" & Reset);
            Put_Line ("       API: " & Red & "Very High" & Reset & "  Disk: High");
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
      Put_Line (Bold & "Rate Limiting:" & Reset);
      Put ("  TPS:" & Integer'Image (Smart_Config.TPS_Limit));
      Put (" Burst:" & Integer'Image (Smart_Config.TPS_Burst));
      Put (" Chunk:" & Integer'Image (Smart_Config.Chunk_Size_MB) & "M");
      Put_Line (" Age:" & Integer'Image (Smart_Config.Cache_Age_Hours) & "h");
   end Print_Rate_Limit_Settings;

   procedure Print_Smart_Sync_Settings is
   begin
      Put_Line (Bold & "Smart Sync:" & Reset);
      Put ("  Cache:");
      if Smart_Config.Cache_Max_Size_GB = 0 then
         Put (" unlimited");
      else
         Put (Integer'Image (Smart_Config.Cache_Max_Size_GB) & "G");
      end if;
      Put (" MinFree:" & Integer'Image (Smart_Config.Min_Free_Space_GB) & "G");
      Put (" WriteBack:" & Integer'Image (Smart_Config.Write_Back_Sec) & "s");
      Put_Line (" Poll:" & Integer'Image (Smart_Config.Poll_Interval_Min) & "m");
   end Print_Smart_Sync_Settings;

   procedure Print_Pinned_Folders is
   begin
      Put_Line (Bold & "Pinned Folders (" & Dim & "offline access" & Reset & Bold & "):" & Reset);
      if Pinned_Count = 0 then
         Put_Line (Dim & "  (none - press P to add)" & Reset);
      else
         for I in 1 .. Pinned_Count loop
            Put ("  ");
            if Pinned_Folders (I).Enabled then
               Put (Green & "●" & Reset);
            else
               Put (Dim & "○" & Reset);
            end if;
            Put (" " & To_String (Services (Pinned_Folders (I).Service_Idx).Name) & ":");
            Put (To_String (Pinned_Folders (I).Remote_Path));
            Put_Line (Dim & " → " & To_String (Pinned_Folders (I).Local_Path) & Reset);
         end loop;
      end if;
      New_Line;
   end Print_Pinned_Folders;

   function Mode_To_String (Mode : Cache_Mode) return String is
   begin
      case Mode is
         when Off     => return "off";
         when Minimal => return "minimal";
         when Writes  => return "writes";
         when Full    => return "full";
      end case;
   end Mode_To_String;

   function Conflict_To_String (C : Conflict_Strategy) return String is
   begin
      case C is
         when None    => return "none";
         when Newer   => return "newer";
         when Older   => return "older";
         when Larger  => return "larger";
         when Smaller => return "smaller";
         when Path1   => return "path1";
         when Path2   => return "path2";
      end case;
   end Conflict_To_String;

   function BW_To_String (BW : Natural) return String is
   begin
      if BW = 0 then
         return "unlimited";
      elsif BW >= 1000 then
         return Ada.Strings.Fixed.Trim (Natural'Image (BW / 1000), Ada.Strings.Left) & " MB/s";
      else
         return Ada.Strings.Fixed.Trim (Natural'Image (BW), Ada.Strings.Left) & " KB/s";
      end if;
   end BW_To_String;

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

   function Validate_Size_GB (V : Integer) return Boolean is
   begin
      return V >= 0 and V <= 1000;
   end Validate_Size_GB;

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
      Put_Line (File, "    --vfs-cache-mode " & Mode_Str & " \");

      -- Smart sync settings
      if Smart_Config.Cache_Max_Size_GB > 0 then
         Put_Line (File, "    --vfs-cache-max-size " &
                   Ada.Strings.Fixed.Trim (Integer'Image (Smart_Config.Cache_Max_Size_GB),
                                           Ada.Strings.Left) & "G \");
      end if;
      Put_Line (File, "    --vfs-cache-min-free-space " &
                Ada.Strings.Fixed.Trim (Integer'Image (Smart_Config.Min_Free_Space_GB),
                                        Ada.Strings.Left) & "G \");
      Put_Line (File, "    --vfs-write-back " &
                Ada.Strings.Fixed.Trim (Integer'Image (Smart_Config.Write_Back_Sec),
                                        Ada.Strings.Left) & "s \");

      -- Rate limiting
      Put_Line (File, "    --tpslimit " &
                Ada.Strings.Fixed.Trim (Integer'Image (Smart_Config.TPS_Limit),
                                        Ada.Strings.Left) & " \");
      Put_Line (File, "    --tpslimit-burst " &
                Ada.Strings.Fixed.Trim (Integer'Image (Smart_Config.TPS_Burst),
                                        Ada.Strings.Left) & " \");
      Put_Line (File, "    --vfs-read-chunk-size " &
                Ada.Strings.Fixed.Trim (Integer'Image (Smart_Config.Chunk_Size_MB),
                                        Ada.Strings.Left) & "M \");
      Put_Line (File, "    --vfs-cache-max-age " &
                Ada.Strings.Fixed.Trim (Integer'Image (Smart_Config.Cache_Age_Hours),
                                        Ada.Strings.Left) & "h \");
      Put_Line (File, "    --dir-cache-time " &
                Ada.Strings.Fixed.Trim (Integer'Image (Smart_Config.Dir_Cache_Min),
                                        Ada.Strings.Left) & "m \");
      Put_Line (File, "    --poll-interval " &
                Ada.Strings.Fixed.Trim (Integer'Image (Smart_Config.Poll_Interval_Min),
                                        Ada.Strings.Left) & "m \");

      -- Performance tuning
      Put_Line (File, "    --buffer-size " &
                Ada.Strings.Fixed.Trim (Integer'Image (Smart_Config.Buffer_Size_MB),
                                        Ada.Strings.Left) & "M \");
      Put_Line (File, "    --transfers " &
                Ada.Strings.Fixed.Trim (Integer'Image (Smart_Config.Transfers),
                                        Ada.Strings.Left) & " \");
      Put_Line (File, "    --checkers " &
                Ada.Strings.Fixed.Trim (Integer'Image (Smart_Config.Checkers),
                                        Ada.Strings.Left));

      Put_Line (File, "ExecStop=/bin/fusermount -uz " & To_String (Service.Mount_Point));
      Put_Line (File, "Restart=on-failure");
      Put_Line (File, "RestartSec=5");
      Put_Line (File, "");
      Put_Line (File, "[Install]");
      Put_Line (File, "WantedBy=default.target");

      Close (File);
   end Generate_Service_File;

   procedure Generate_Offline_Sync_Service is
      Service_File : constant String :=
         "/tmp/cloud-sync-tuner/output/rclone-offline-sync.service";
      Timer_File : constant String :=
         "/tmp/cloud-sync-tuner/output/rclone-offline-sync.timer";
      File : File_Type;
   begin
      if Pinned_Count = 0 then
         return;
      end if;

      -- Generate service file
      Create (File, Out_File, Service_File);
      Put_Line (File, "# SPDX-License-Identifier: AGPL-3.0-or-later");
      Put_Line (File, "# Generated by Cloud Sync Tuner v" & Version);
      Put_Line (File, "# Syncs pinned folders for offline access");
      Put_Line (File, "[Unit]");
      Put_Line (File, "Description=Sync Pinned Cloud Folders for Offline Access");
      Put_Line (File, "After=network-online.target");
      Put_Line (File, "Wants=network-online.target");
      Put_Line (File, "");
      Put_Line (File, "[Service]");
      Put_Line (File, "Type=oneshot");

      for I in 1 .. Pinned_Count loop
         if Pinned_Folders (I).Enabled then
            Put_Line (File, "ExecStart=/home/hyper/.local/bin/rclone sync " &
                      To_String (Services (Pinned_Folders (I).Service_Idx).Remote) &
                      To_String (Pinned_Folders (I).Remote_Path) & " " &
                      To_String (Pinned_Folders (I).Local_Path) & " \");
            Put_Line (File, "    --tpslimit " &
                      Ada.Strings.Fixed.Trim (Integer'Image (Smart_Config.TPS_Limit),
                                              Ada.Strings.Left) & " \");
            Put_Line (File, "    --transfers " &
                      Ada.Strings.Fixed.Trim (Integer'Image (Smart_Config.Transfers),
                                              Ada.Strings.Left) & " \");
            Put_Line (File, "    --progress");
         end if;
      end loop;

      Put_Line (File, "");
      Put_Line (File, "[Install]");
      Put_Line (File, "WantedBy=default.target");
      Close (File);

      -- Generate timer file
      Create (File, Out_File, Timer_File);
      Put_Line (File, "# SPDX-License-Identifier: AGPL-3.0-or-later");
      Put_Line (File, "# Generated by Cloud Sync Tuner v" & Version);
      Put_Line (File, "[Unit]");
      Put_Line (File, "Description=Timer for Offline Cloud Folder Sync");
      Put_Line (File, "");
      Put_Line (File, "[Timer]");
      Put_Line (File, "OnBootSec=5min");
      Put_Line (File, "OnUnitActiveSec=30min");
      Put_Line (File, "Persistent=true");
      Put_Line (File, "");
      Put_Line (File, "[Install]");
      Put_Line (File, "WantedBy=timers.target");
      Close (File);
   end Generate_Offline_Sync_Service;

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

      -- Generate offline sync if we have pinned folders
      if Pinned_Count > 0 then
         Generate_Offline_Sync_Service;
         Put_Line (Green & "  ✓ " & Reset & "rclone-offline-sync.service");
         Put_Line (Green & "  ✓ " & Reset & "rclone-offline-sync.timer");
      end if;

      New_Line;
      Put_Line (Bold & "Service files generated in: " & Reset & "/tmp/cloud-sync-tuner/output/");
      New_Line;
      Put_Line ("To apply, run:");
      Put_Line (Cyan & "  cp /tmp/cloud-sync-tuner/output/*.service ~/.config/systemd/user/" & Reset);
      Put_Line (Cyan & "  cp /tmp/cloud-sync-tuner/output/*.timer ~/.config/systemd/user/" & Reset);
      Put_Line (Cyan & "  systemctl --user daemon-reload" & Reset);
      Put_Line (Cyan & "  systemctl --user restart rclone-dropbox rclone-gdrive rclone-onedrive" & Reset);
      if Pinned_Count > 0 then
         Put_Line (Cyan & "  systemctl --user enable --now rclone-offline-sync.timer" & Reset);
      end if;
      New_Line;
   end Apply_Configuration;

   procedure Print_Bandwidth_Settings is
   begin
      Put_Line (Bold & "Bandwidth Schedule:" & Reset);
      if not Smart_Config.BW_Schedule_Enabled then
         Put_Line (Dim & "  (disabled - press B to configure)" & Reset);
      else
         for I in 1 .. 2 loop
            if Smart_Config.BW_Schedule (I).Start_Hour /= Smart_Config.BW_Schedule (I).End_Hour then
               Put ("  " & Natural'Image (Smart_Config.BW_Schedule (I).Start_Hour) & ":00-");
               Put (Natural'Image (Smart_Config.BW_Schedule (I).End_Hour) & ":00 → ");
               Put_Line (BW_To_String (Smart_Config.BW_Schedule (I).BW_Limit));
            end if;
         end loop;
      end if;
   end Print_Bandwidth_Settings;

   procedure Print_Conflict_Settings is
   begin
      Put ("Conflict: " & Cyan & Conflict_To_String (Smart_Config.Conflict_Resolve) & Reset);
      Put_Line ("  Notify: " & (if Smart_Config.Desktop_Notify then Green & "on" else Dim & "off") & Reset);
   end Print_Conflict_Settings;

   procedure Print_Menu is
   begin
      Put_Line (Bold & "═══════════════════════════════════════════════════════════════" & Reset);
      Put_Line ("  " & Cyan & "[1-4]" & Reset & " Mode  " &
                Cyan & "[R]" & Reset & "ate  " &
                Cyan & "[S]" & Reset & "mart  " &
                Cyan & "[B]" & Reset & "andwidth  " &
                Cyan & "[P]" & Reset & "in  " &
                Cyan & "[A]" & Reset & "pply");
      Put_Line ("  " & Cyan & "[C]" & Reset & "onflict  " &
                Cyan & "[N]" & Reset & "otify  " &
                Cyan & "[?]" & Reset & " Help  " &
                Cyan & "[Q]" & Reset & "uit");
      Put_Line (Bold & "═══════════════════════════════════════════════════════════════" & Reset);
   end Print_Menu;

   procedure Edit_Bandwidth_Schedule is
      Choice : Character;
      Input_Line : String (1 .. 10);
      Last : Natural;
      Value : Integer;
   begin
      loop
         Clear_Screen;
         Print_Header;
         Put_Line (Bold & "Bandwidth Schedule (like native clients):" & Reset);
         Put_Line (Dim & "Limit bandwidth during work hours to avoid saturating connection" & Reset);
         New_Line;

         Put ("  [E] Scheduling: ");
         if Smart_Config.BW_Schedule_Enabled then
            Put_Line (Green & "ENABLED" & Reset);
         else
            Put_Line (Dim & "DISABLED" & Reset);
         end if;

         New_Line;
         Put_Line ("  Current Schedule:");
         for I in 1 .. 2 loop
            Put ("    [" & Character'Val (Character'Pos ('0') + I) & "] ");
            Put (Natural'Image (Smart_Config.BW_Schedule (I).Start_Hour) & ":00 - ");
            Put (Natural'Image (Smart_Config.BW_Schedule (I).End_Hour) & ":00 → ");
            Put_Line (BW_To_String (Smart_Config.BW_Schedule (I).BW_Limit));
         end loop;

         New_Line;
         Put_Line ("  [W] Work hours preset (9-17: 5MB/s, else unlimited)");
         Put_Line ("  [N] Night sync preset (22-06: unlimited, else 2MB/s)");
         Put_Line ("  [U] Unlimited always");
         Put_Line ("  [B] Back to main menu");
         New_Line;
         Put ("  Selection: ");
         Get_Immediate (Choice);
         New_Line;

         case Choice is
            when 'e' | 'E' =>
               Smart_Config.BW_Schedule_Enabled := not Smart_Config.BW_Schedule_Enabled;

            when '1' | '2' =>
               declare
                  Idx : constant Positive := Character'Pos (Choice) - Character'Pos ('0');
               begin
                  Put ("  Start hour (0-23): ");
                  Get_Line (Input_Line, Last);
                  begin
                     Value := Integer'Value (Input_Line (1 .. Last));
                     if Value >= 0 and Value <= 23 then
                        Smart_Config.BW_Schedule (Idx).Start_Hour := Value;
                     end if;
                  exception
                     when others => null;
                  end;

                  Put ("  End hour (0-24): ");
                  Get_Line (Input_Line, Last);
                  begin
                     Value := Integer'Value (Input_Line (1 .. Last));
                     if Value >= 0 and Value <= 24 then
                        Smart_Config.BW_Schedule (Idx).End_Hour := Value;
                     end if;
                  exception
                     when others => null;
                  end;

                  Put ("  Bandwidth KB/s (0=unlimited): ");
                  Get_Line (Input_Line, Last);
                  begin
                     Value := Integer'Value (Input_Line (1 .. Last));
                     if Value >= 0 then
                        Smart_Config.BW_Schedule (Idx).BW_Limit := Value;
                     end if;
                  exception
                     when others => null;
                  end;
               end;

            when 'w' | 'W' =>
               Smart_Config.BW_Schedule (1) := (Start_Hour => 9, End_Hour => 17, BW_Limit => 5000);
               Smart_Config.BW_Schedule (2) := (Start_Hour => 17, End_Hour => 9, BW_Limit => 0);
               Smart_Config.BW_Schedule_Enabled := True;
               Put_Line (Green & "  Applied work hours preset" & Reset);
               delay 1.0;

            when 'n' | 'N' =>
               Smart_Config.BW_Schedule (1) := (Start_Hour => 22, End_Hour => 6, BW_Limit => 0);
               Smart_Config.BW_Schedule (2) := (Start_Hour => 6, End_Hour => 22, BW_Limit => 2000);
               Smart_Config.BW_Schedule_Enabled := True;
               Put_Line (Green & "  Applied night sync preset" & Reset);
               delay 1.0;

            when 'u' | 'U' =>
               Smart_Config.BW_Schedule (1) := (Start_Hour => 0, End_Hour => 24, BW_Limit => 0);
               Smart_Config.BW_Schedule (2) := (Start_Hour => 0, End_Hour => 0, BW_Limit => 0);
               Smart_Config.BW_Schedule_Enabled := False;
               Put_Line (Green & "  Unlimited bandwidth" & Reset);
               delay 1.0;

            when 'b' | 'B' =>
               exit;

            when others =>
               null;
         end case;
      end loop;
   end Edit_Bandwidth_Schedule;

   procedure Edit_Conflict_Resolution is
      Choice : Character;
   begin
      loop
         Clear_Screen;
         Print_Header;
         Put_Line (Bold & "Conflict Resolution (for bisync):" & Reset);
         Put_Line (Dim & "When files change on both sides, how should we resolve?" & Reset);
         New_Line;

         Put_Line ("  Current: " & Cyan & Conflict_To_String (Smart_Config.Conflict_Resolve) & Reset);
         New_Line;

         Put_Line ("  [1] " & (if Smart_Config.Conflict_Resolve = None then "[×]" else "[ ]") &
                   " None - keep both versions with conflict suffix");
         Put_Line ("  [2] " & (if Smart_Config.Conflict_Resolve = Newer then "[×]" else "[ ]") &
                   " Newer - prefer more recently modified file");
         Put_Line ("  [3] " & (if Smart_Config.Conflict_Resolve = Older then "[×]" else "[ ]") &
                   " Older - prefer older file (conservative)");
         Put_Line ("  [4] " & (if Smart_Config.Conflict_Resolve = Larger then "[×]" else "[ ]") &
                   " Larger - prefer larger file");
         Put_Line ("  [5] " & (if Smart_Config.Conflict_Resolve = Smaller then "[×]" else "[ ]") &
                   " Smaller - prefer smaller file");
         Put_Line ("  [6] " & (if Smart_Config.Conflict_Resolve = Path1 then "[×]" else "[ ]") &
                   " Path1 - always prefer local");
         Put_Line ("  [7] " & (if Smart_Config.Conflict_Resolve = Path2 then "[×]" else "[ ]") &
                   " Path2 - always prefer remote");
         New_Line;
         Put_Line ("  [B] Back to main menu");
         New_Line;
         Put ("  Selection: ");
         Get_Immediate (Choice);
         New_Line;

         case Choice is
            when '1' => Smart_Config.Conflict_Resolve := None;
            when '2' => Smart_Config.Conflict_Resolve := Newer;
            when '3' => Smart_Config.Conflict_Resolve := Older;
            when '4' => Smart_Config.Conflict_Resolve := Larger;
            when '5' => Smart_Config.Conflict_Resolve := Smaller;
            when '6' => Smart_Config.Conflict_Resolve := Path1;
            when '7' => Smart_Config.Conflict_Resolve := Path2;
            when 'b' | 'B' => exit;
            when others => null;
         end case;
      end loop;
   end Edit_Conflict_Resolution;

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
         Put_Line ("  [1] TPS Limit:      " & Integer'Image (Smart_Config.TPS_Limit) &
                   Dim & " (1-100, Dropbox safe: 4)" & Reset);
         Put_Line ("  [2] TPS Burst:      " & Integer'Image (Smart_Config.TPS_Burst) &
                   Dim & " (1-10, recommended: 1)" & Reset);
         Put_Line ("  [3] Chunk Size MB:  " & Integer'Image (Smart_Config.Chunk_Size_MB) &
                   Dim & " (1-512, optimal: 32)" & Reset);
         Put_Line ("  [4] Cache Age hrs:  " & Integer'Image (Smart_Config.Cache_Age_Hours) &
                   Dim & " (1-168, default: 72)" & Reset);
         Put_Line ("  [5] Dir Cache min:  " & Integer'Image (Smart_Config.Dir_Cache_Min) &
                   Dim & " (1-60, default: 5)" & Reset);
         New_Line;
         Put_Line ("  [D] Reset to Dropbox-safe defaults");
         Put_Line ("  [G] Google Drive optimized");
         Put_Line ("  [O] OneDrive optimized");
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
                     Smart_Config.TPS_Limit := Value;
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
                     Smart_Config.TPS_Burst := Value;
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
                     Smart_Config.Chunk_Size_MB := Value;
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
                     Smart_Config.Cache_Age_Hours := Value;
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
                     Smart_Config.Dir_Cache_Min := Value;
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
               Smart_Config.TPS_Limit := 4;
               Smart_Config.TPS_Burst := 1;
               Smart_Config.Chunk_Size_MB := 32;
               Put_Line (Green & "  Applied Dropbox-safe rate limits" & Reset);
               delay 1.0;

            when 'g' | 'G' =>
               Smart_Config.TPS_Limit := 8;
               Smart_Config.TPS_Burst := 2;
               Smart_Config.Chunk_Size_MB := 64;
               Put_Line (Green & "  Applied Google Drive optimized rate limits" & Reset);
               delay 1.0;

            when 'o' | 'O' =>
               Smart_Config.TPS_Limit := 6;
               Smart_Config.TPS_Burst := 2;
               Smart_Config.Chunk_Size_MB := 32;
               Put_Line (Green & "  Applied OneDrive optimized rate limits" & Reset);
               delay 1.0;

            when 'b' | 'B' =>
               exit;

            when others =>
               null;
         end case;
      end loop;
   end Edit_Rate_Config;

   procedure Edit_Smart_Sync is
      Choice : Character;
      Input_Line : String (1 .. 10);
      Last : Natural;
      Value : Integer;
   begin
      loop
         Clear_Screen;
         Print_Header;
         Put_Line (Bold & "Smart Sync Configuration (like native clients):" & Reset);
         New_Line;
         Put ("  [1] Cache Max Size:   ");
         if Smart_Config.Cache_Max_Size_GB = 0 then
            Put_Line ("unlimited" & Dim & " (0-1000 GB, 0=unlimited)" & Reset);
         else
            Put_Line (Integer'Image (Smart_Config.Cache_Max_Size_GB) & " GB" &
                      Dim & " (0-1000 GB, 0=unlimited)" & Reset);
         end if;
         Put_Line ("  [2] Min Free Space:  " & Integer'Image (Smart_Config.Min_Free_Space_GB) & " GB" &
                   Dim & " (0-100 GB, auto-evict threshold)" & Reset);
         Put_Line ("  [3] Write-Back:      " & Integer'Image (Smart_Config.Write_Back_Sec) & " sec" &
                   Dim & " (1-60 sec, buffer before upload)" & Reset);
         Put_Line ("  [4] Poll Interval:   " & Integer'Image (Smart_Config.Poll_Interval_Min) & " min" &
                   Dim & " (1-60 min, remote change check)" & Reset);
         New_Line;
         Put_Line (Bold & "Performance Tuning:" & Reset);
         Put_Line ("  [5] Buffer Size:     " & Integer'Image (Smart_Config.Buffer_Size_MB) & " MB" &
                   Dim & " (1-128 MB)" & Reset);
         Put_Line ("  [6] Transfers:       " & Integer'Image (Smart_Config.Transfers) &
                   Dim & " (1-32 concurrent)" & Reset);
         Put_Line ("  [7] Checkers:        " & Integer'Image (Smart_Config.Checkers) &
                   Dim & " (1-64 concurrent)" & Reset);
         New_Line;
         Put_Line ("  [D] Reset to defaults");
         Put_Line ("  [B] Back to main menu");
         New_Line;
         Put ("  Selection: ");
         Get_Immediate (Choice);
         New_Line;

         case Choice is
            when '1' =>
               Put ("  Enter Cache Max Size GB (0=unlimited, 1-1000): ");
               Get_Line (Input_Line, Last);
               begin
                  Value := Integer'Value (Input_Line (1 .. Last));
                  if Validate_Size_GB (Value) then
                     Smart_Config.Cache_Max_Size_GB := Value;
                  else
                     Print_Validation_Error ("Cache Max Size", Input_Line (1 .. Last), "0-1000 GB");
                     delay 2.0;
                  end if;
               exception
                  when others =>
                     Print_Validation_Error ("Cache Max Size", Input_Line (1 .. Last), "integer 0-1000");
                     delay 2.0;
               end;

            when '2' =>
               Put ("  Enter Min Free Space GB (0-100): ");
               Get_Line (Input_Line, Last);
               begin
                  Value := Integer'Value (Input_Line (1 .. Last));
                  if Value >= 0 and Value <= 100 then
                     Smart_Config.Min_Free_Space_GB := Value;
                  else
                     Print_Validation_Error ("Min Free Space", Input_Line (1 .. Last), "0-100 GB");
                     delay 2.0;
                  end if;
               exception
                  when others =>
                     Print_Validation_Error ("Min Free Space", Input_Line (1 .. Last), "integer 0-100");
                     delay 2.0;
               end;

            when '3' =>
               Put ("  Enter Write-Back seconds (1-60): ");
               Get_Line (Input_Line, Last);
               begin
                  Value := Integer'Value (Input_Line (1 .. Last));
                  if Value >= 1 and Value <= 60 then
                     Smart_Config.Write_Back_Sec := Value;
                  else
                     Print_Validation_Error ("Write-Back", Input_Line (1 .. Last), "1-60 seconds");
                     delay 2.0;
                  end if;
               exception
                  when others =>
                     Print_Validation_Error ("Write-Back", Input_Line (1 .. Last), "integer 1-60");
                     delay 2.0;
               end;

            when '4' =>
               Put ("  Enter Poll Interval minutes (1-60): ");
               Get_Line (Input_Line, Last);
               begin
                  Value := Integer'Value (Input_Line (1 .. Last));
                  if Value >= 1 and Value <= 60 then
                     Smart_Config.Poll_Interval_Min := Value;
                  else
                     Print_Validation_Error ("Poll Interval", Input_Line (1 .. Last), "1-60 minutes");
                     delay 2.0;
                  end if;
               exception
                  when others =>
                     Print_Validation_Error ("Poll Interval", Input_Line (1 .. Last), "integer 1-60");
                     delay 2.0;
               end;

            when '5' =>
               Put ("  Enter Buffer Size MB (1-128): ");
               Get_Line (Input_Line, Last);
               begin
                  Value := Integer'Value (Input_Line (1 .. Last));
                  if Value >= 1 and Value <= 128 then
                     Smart_Config.Buffer_Size_MB := Value;
                  else
                     Print_Validation_Error ("Buffer Size", Input_Line (1 .. Last), "1-128 MB");
                     delay 2.0;
                  end if;
               exception
                  when others =>
                     Print_Validation_Error ("Buffer Size", Input_Line (1 .. Last), "integer 1-128");
                     delay 2.0;
               end;

            when '6' =>
               Put ("  Enter Transfers (1-32): ");
               Get_Line (Input_Line, Last);
               begin
                  Value := Integer'Value (Input_Line (1 .. Last));
                  if Value >= 1 and Value <= 32 then
                     Smart_Config.Transfers := Value;
                  else
                     Print_Validation_Error ("Transfers", Input_Line (1 .. Last), "1-32");
                     delay 2.0;
                  end if;
               exception
                  when others =>
                     Print_Validation_Error ("Transfers", Input_Line (1 .. Last), "integer 1-32");
                     delay 2.0;
               end;

            when '7' =>
               Put ("  Enter Checkers (1-64): ");
               Get_Line (Input_Line, Last);
               begin
                  Value := Integer'Value (Input_Line (1 .. Last));
                  if Value >= 1 and Value <= 64 then
                     Smart_Config.Checkers := Value;
                  else
                     Print_Validation_Error ("Checkers", Input_Line (1 .. Last), "1-64");
                     delay 2.0;
                  end if;
               exception
                  when others =>
                     Print_Validation_Error ("Checkers", Input_Line (1 .. Last), "integer 1-64");
                     delay 2.0;
               end;

            when 'd' | 'D' =>
               Smart_Config.Cache_Max_Size_GB := 10;
               Smart_Config.Min_Free_Space_GB := 5;
               Smart_Config.Write_Back_Sec := 5;
               Smart_Config.Poll_Interval_Min := 1;
               Smart_Config.Buffer_Size_MB := 16;
               Smart_Config.Transfers := 4;
               Smart_Config.Checkers := 8;
               Put_Line (Green & "  Reset to defaults" & Reset);
               delay 1.0;

            when 'b' | 'B' =>
               exit;

            when others =>
               null;
         end case;
      end loop;
   end Edit_Smart_Sync;

   procedure Manage_Pinned_Folders is
      Choice : Character;
      Input_Line : String (1 .. 256);
      Last : Natural;
      Service_Idx : Positive;
   begin
      loop
         Clear_Screen;
         Print_Header;
         Put_Line (Bold & "Pinned Folders (Offline Access):" & Reset);
         Put_Line (Dim & "Like 'Make Available Offline' in native clients" & Reset);
         New_Line;

         if Pinned_Count = 0 then
            Put_Line ("  (No pinned folders)");
         else
            for I in 1 .. Pinned_Count loop
               Put ("  [" & Character'Val (Character'Pos ('0') + I) & "] ");
               if Pinned_Folders (I).Enabled then
                  Put (Green & "[×]" & Reset);
               else
                  Put (Dim & "[ ]" & Reset);
               end if;
               Put (" " & To_String (Services (Pinned_Folders (I).Service_Idx).Name) & ":");
               Put (To_String (Pinned_Folders (I).Remote_Path));
               Put_Line (Dim & " → " & To_String (Pinned_Folders (I).Local_Path) & Reset);
            end loop;
         end if;

         New_Line;
         Put_Line ("  [A] Add pinned folder");
         if Pinned_Count > 0 then
            Put_Line ("  [1-" & Character'Val (Character'Pos ('0') + Pinned_Count) & "] Toggle folder");
            Put_Line ("  [D] Delete pinned folder");
         end if;
         Put_Line ("  [B] Back to main menu");
         New_Line;
         Put ("  Selection: ");
         Get_Immediate (Choice);
         New_Line;

         case Choice is
            when 'a' | 'A' =>
               if Pinned_Count >= 20 then
                  Put_Line (Red & "  Maximum 20 pinned folders" & Reset);
                  delay 2.0;
               else
                  -- Select service
                  Put_Line ("  Select cloud service:");
                  for I in Services'Range loop
                     Put_Line ("    [" & Character'Val (Character'Pos ('0') + I) & "] " &
                               To_String (Services (I).Name));
                  end loop;
                  Put ("  Service: ");
                  Get_Immediate (Choice);
                  New_Line;

                  if Choice >= '1' and Choice <= '3' then
                     Service_Idx := Character'Pos (Choice) - Character'Pos ('0');

                     -- Get remote path
                     Put ("  Remote folder path (e.g., Documents/Important): ");
                     Get_Line (Input_Line, Last);
                     if Last > 0 then
                        Pinned_Count := Pinned_Count + 1;
                        Pinned_Folders (Pinned_Count).Service_Idx := Service_Idx;
                        Pinned_Folders (Pinned_Count).Remote_Path :=
                           To_Unbounded_String (Input_Line (1 .. Last));
                        Pinned_Folders (Pinned_Count).Local_Path :=
                           To_Unbounded_String (Offline_Dir & "/" &
                                                To_String (Services (Service_Idx).Name) & "/" &
                                                Input_Line (1 .. Last));
                        Pinned_Folders (Pinned_Count).Enabled := True;
                        Put_Line (Green & "  Added pinned folder" & Reset);
                        delay 1.0;
                     end if;
                  end if;
               end if;

            when '1' .. '9' =>
               declare
                  Idx : constant Natural := Character'Pos (Choice) - Character'Pos ('0');
               begin
                  if Idx >= 1 and Idx <= Pinned_Count then
                     Pinned_Folders (Idx).Enabled := not Pinned_Folders (Idx).Enabled;
                  end if;
               end;

            when 'd' | 'D' =>
               if Pinned_Count > 0 then
                  Put ("  Enter number to delete (1-" &
                       Character'Val (Character'Pos ('0') + Pinned_Count) & "): ");
                  Get_Immediate (Choice);
                  New_Line;
                  declare
                     Idx : constant Natural := Character'Pos (Choice) - Character'Pos ('0');
                  begin
                     if Idx >= 1 and Idx <= Pinned_Count then
                        -- Shift remaining entries
                        for I in Idx .. Pinned_Count - 1 loop
                           Pinned_Folders (I) := Pinned_Folders (I + 1);
                        end loop;
                        Pinned_Count := Pinned_Count - 1;
                        Put_Line (Green & "  Deleted pinned folder" & Reset);
                        delay 1.0;
                     end if;
                  end;
               end if;

            when 'b' | 'B' =>
               exit;

            when others =>
               null;
         end case;
      end loop;
   end Manage_Pinned_Folders;

   procedure Run_TUI is
      Choice : Character;
      Running : Boolean := True;
   begin
      while Running loop
         Clear_Screen;
         Print_Header;
         Print_Services;

         Put_Line (Bold & "VFS Cache Mode:" & Reset);
         Put_Line (Dim & "(Select with 1-4)" & Reset);
         New_Line;

         Print_Mode_Info (Off, Current_Mode = Off);
         Print_Mode_Info (Minimal, Current_Mode = Minimal);
         Print_Mode_Info (Writes, Current_Mode = Writes);
         Print_Mode_Info (Full, Current_Mode = Full);

         Print_Rate_Limit_Settings;
         Print_Smart_Sync_Settings;
         Print_Bandwidth_Settings;
         Print_Conflict_Settings;
         Print_Pinned_Folders;

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
            when 's' | 'S' =>
               Current_Help := Smart_Sync_Help;
               Edit_Smart_Sync;
            when 'b' | 'B' =>
               Edit_Bandwidth_Schedule;
            when 'c' | 'C' =>
               Edit_Conflict_Resolution;
            when 'n' | 'N' =>
               Smart_Config.Desktop_Notify := not Smart_Config.Desktop_Notify;
            when 'p' | 'P' =>
               Current_Help := Pinned_Help;
               Manage_Pinned_Folders;
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
      Put_Line ("Ada TUI for rclone cloud mount configuration with smart sync");
      New_Line;
      Put_Line (Bold & "USAGE:" & Reset);
      Put_Line ("  cloud-sync-tuner                      Interactive TUI mode");
      Put_Line ("  cloud-sync-tuner <mode>               Apply mode directly");
      Put_Line ("  cloud-sync-tuner [options] [mode]     CLI with options");
      New_Line;
      Put_Line (Bold & "MODES:" & Reset);
      Put_Line ("  off       No caching - direct cloud access");
      Put_Line ("  minimal   Cache open files only");
      Put_Line ("  writes    Cache writes, stream reads " & Yellow & "(recommended)" & Reset);
      Put_Line ("  full      Full local caching");
      New_Line;
      Put_Line (Bold & "RATE LIMITING OPTIONS:" & Reset);
      Put_Line ("  --tps <n>           Transactions per second (1-100, default: 4)");
      Put_Line ("  --burst <n>         TPS burst allowance (1-10, default: 1)");
      Put_Line ("  --chunk <n>M        Read chunk size in MB (1-512, default: 32)");
      Put_Line ("  --cache-age <n>h    Cache max age in hours (1-168, default: 72)");
      Put_Line ("  --dir-cache <n>m    Directory cache time in minutes (1-60, default: 5)");
      New_Line;
      Put_Line (Bold & "SMART SYNC OPTIONS:" & Reset);
      Put_Line ("  --cache-size <n>G   Max cache size in GB (0=unlimited, default: 10)");
      Put_Line ("  --min-free <n>G     Min free space threshold in GB (default: 5)");
      Put_Line ("  --write-back <n>s   Write-back delay in seconds (1-60, default: 5)");
      Put_Line ("  --poll <n>m         Poll interval in minutes (1-60, default: 1)");
      New_Line;
      Put_Line (Bold & "PERFORMANCE OPTIONS:" & Reset);
      Put_Line ("  --buffer <n>M       Buffer size in MB (1-128, default: 16)");
      Put_Line ("  --transfers <n>     Concurrent transfers (1-32, default: 4)");
      Put_Line ("  --checkers <n>      Concurrent checkers (1-64, default: 8)");
      New_Line;
      Put_Line (Bold & "PINNED FOLDERS:" & Reset);
      Put_Line ("  --pin <svc>:<path>    Pin folder for offline access");
      Put_Line ("  --unpin <svc>:<path>  Remove pinned folder");
      Put_Line ("  --list-pins           List all pinned folders");
      New_Line;
      Put_Line (Bold & "PRESETS:" & Reset);
      Put_Line ("  --preset dropbox    Dropbox-safe settings (TPS 4, burst 1)");
      Put_Line ("  --preset gdrive     Google Drive optimized (TPS 8, burst 2)");
      Put_Line ("  --preset onedrive   OneDrive optimized (TPS 6, burst 2)");
      New_Line;
      Put_Line (Bold & "OTHER OPTIONS:" & Reset);
      Put_Line ("  -h, --help          Show this help");
      Put_Line ("  -v, --version       Show version");
      Put_Line ("  --status            Check sync service health");
      Put_Line ("  --validate <mode>   Validate mode name");
      Put_Line ("  --apply             Auto-install generated services");
      Put_Line ("  --dry-run           Show what would be done");
      New_Line;
      Put_Line (Bold & "EXAMPLES:" & Reset);
      Put_Line ("  cloud-sync-tuner writes");
      Put_Line ("  cloud-sync-tuner --preset dropbox --cache-size 20G writes");
      Put_Line ("  cloud-sync-tuner --tps 8 --cache-size 50G --pin dropbox:Documents full");
      Put_Line ("  cloud-sync-tuner --preset gdrive --poll 5m --write-back 10s writes");
      New_Line;
      Put_Line (Bold & "FILES:" & Reset);
      Put_Line ("  ~/.config/cloud-sync-tuner/config.toml   Configuration");
      Put_Line ("  /tmp/cloud-sync-tuner/output/            Generated services");
      Put_Line ("  ~/Offline/                               Pinned folder sync target");
      New_Line;
      Put_Line (Bold & "SEE ALSO:" & Reset);
      Put_Line ("  man cloud-sync-tuner        Full manual page");
      Put_Line ("  rclone(1), systemctl(1)");
   end Print_Help;

   procedure Print_Version is
   begin
      Put_Line ("cloud-sync-tuner " & Version);
      Put_Line ("Copyright (C) 2025 hyperpolymath");
      Put_Line ("License: AGPL-3.0-or-later");
   end Print_Version;

   function Parse_Size (S : String) return Natural is
      Len : constant Natural := S'Length;
      Unit_Char : Character;
      Num_Str : String (1 .. 10);
      Num_Len : Natural := 0;
   begin
      if Len = 0 then
         return 0;
      end if;

      Unit_Char := S (S'Last);
      if Unit_Char = 'G' or Unit_Char = 'g' or
         Unit_Char = 'M' or Unit_Char = 'm' then
         for I in S'First .. S'Last - 1 loop
            Num_Len := Num_Len + 1;
            Num_Str (Num_Len) := S (I);
         end loop;
         return Natural'Value (Num_Str (1 .. Num_Len));
      else
         return Natural'Value (S);
      end if;
   exception
      when others => return 0;
   end Parse_Size;

   function Parse_Time (S : String) return Natural is
      Len : constant Natural := S'Length;
      Unit_Char : Character;
      Num_Str : String (1 .. 10);
      Num_Len : Natural := 0;
   begin
      if Len = 0 then
         return 0;
      end if;

      Unit_Char := S (S'Last);
      for I in S'First .. S'Last - 1 loop
         Num_Len := Num_Len + 1;
         Num_Str (Num_Len) := S (I);
      end loop;

      -- Convert to appropriate unit based on suffix
      if Unit_Char = 'h' then
         return Natural'Value (Num_Str (1 .. Num_Len));  -- hours
      elsif Unit_Char = 'm' then
         return Natural'Value (Num_Str (1 .. Num_Len));  -- minutes
      elsif Unit_Char = 's' then
         return Natural'Value (Num_Str (1 .. Num_Len));  -- seconds
      else
         return Natural'Value (S);
      end if;
   exception
      when others => return 0;
   end Parse_Time;

   -- Command line mode for scripting
   procedure Run_CLI is
      Arg_Count : constant Natural := Ada.Command_Line.Argument_Count;
      I : Positive := 1;
      Mode_Set : Boolean := False;
      Apply_After : Boolean := False;
      Dry_Run : Boolean := False;
   begin
      while I <= Arg_Count loop
         declare
            Arg : constant String := Ada.Command_Line.Argument (I);
         begin
            -- Help options
            if Arg = "--help" or Arg = "-h" then
               Print_Help;
               return;
            end if;

            -- Version option
            if Arg = "--version" or Arg = "-v" then
               Print_Version;
               return;
            end if;

            -- Status option (delegates to cloud-sync-status script)
            if Arg = "--status" then
               declare
                  Success : Boolean;
                  Args : GNAT.OS_Lib.Argument_List_Access :=
                     new GNAT.OS_Lib.Argument_List (1 .. 0);
               begin
                  GNAT.OS_Lib.Spawn (
                     Program_Name => "cloud-sync-status",
                     Args         => Args.all,
                     Success      => Success);
                  if not Success then
                     Put_Line (Yellow & "Note: cloud-sync-status not found." & Reset);
                     Put_Line ("Install with: just install");
                  end if;
                  GNAT.OS_Lib.Free (Args);
               end;
               return;
            end if;

            -- Validate option
            if Arg = "--validate" then
               if I + 1 > Arg_Count then
                  Put_Line (Red & "Error: --validate requires a mode argument" & Reset);
                  Ada.Command_Line.Set_Exit_Status (1);
                  return;
               end if;
               declare
                  Mode_Arg : constant String := Ada.Command_Line.Argument (I + 1);
               begin
                  if Validate_Mode (Mode_Arg) then
                     Put_Line (Green & "Valid mode: " & Mode_Arg & Reset);
                  else
                     Put_Line (Red & "Invalid mode: " & Mode_Arg & Reset);
                     Put_Line ("Valid modes: off, minimal, writes, full");
                     Ada.Command_Line.Set_Exit_Status (1);
                  end if;
               end;
               return;
            end if;

            -- Rate limiting options
            if Arg = "--tps" and I + 1 <= Arg_Count then
               Smart_Config.TPS_Limit := Positive'Value (Ada.Command_Line.Argument (I + 1));
               I := I + 1;
            elsif Arg = "--burst" and I + 1 <= Arg_Count then
               Smart_Config.TPS_Burst := Positive'Value (Ada.Command_Line.Argument (I + 1));
               I := I + 1;
            elsif Arg = "--chunk" and I + 1 <= Arg_Count then
               Smart_Config.Chunk_Size_MB := Parse_Size (Ada.Command_Line.Argument (I + 1));
               I := I + 1;
            elsif Arg = "--cache-age" and I + 1 <= Arg_Count then
               Smart_Config.Cache_Age_Hours := Parse_Time (Ada.Command_Line.Argument (I + 1));
               I := I + 1;
            elsif Arg = "--dir-cache" and I + 1 <= Arg_Count then
               Smart_Config.Dir_Cache_Min := Parse_Time (Ada.Command_Line.Argument (I + 1));
               I := I + 1;

            -- Smart sync options
            elsif Arg = "--cache-size" and I + 1 <= Arg_Count then
               Smart_Config.Cache_Max_Size_GB := Parse_Size (Ada.Command_Line.Argument (I + 1));
               I := I + 1;
            elsif Arg = "--min-free" and I + 1 <= Arg_Count then
               Smart_Config.Min_Free_Space_GB := Parse_Size (Ada.Command_Line.Argument (I + 1));
               I := I + 1;
            elsif Arg = "--write-back" and I + 1 <= Arg_Count then
               Smart_Config.Write_Back_Sec := Parse_Time (Ada.Command_Line.Argument (I + 1));
               I := I + 1;
            elsif Arg = "--poll" and I + 1 <= Arg_Count then
               Smart_Config.Poll_Interval_Min := Parse_Time (Ada.Command_Line.Argument (I + 1));
               I := I + 1;

            -- Performance options
            elsif Arg = "--buffer" and I + 1 <= Arg_Count then
               Smart_Config.Buffer_Size_MB := Parse_Size (Ada.Command_Line.Argument (I + 1));
               I := I + 1;
            elsif Arg = "--transfers" and I + 1 <= Arg_Count then
               Smart_Config.Transfers := Positive'Value (Ada.Command_Line.Argument (I + 1));
               I := I + 1;
            elsif Arg = "--checkers" and I + 1 <= Arg_Count then
               Smart_Config.Checkers := Positive'Value (Ada.Command_Line.Argument (I + 1));
               I := I + 1;

            -- Presets
            elsif Arg = "--preset" and I + 1 <= Arg_Count then
               declare
                  Preset : constant String := Ada.Command_Line.Argument (I + 1);
               begin
                  if Preset = "dropbox" then
                     Smart_Config.TPS_Limit := 4;
                     Smart_Config.TPS_Burst := 1;
                     Smart_Config.Chunk_Size_MB := 32;
                  elsif Preset = "gdrive" then
                     Smart_Config.TPS_Limit := 8;
                     Smart_Config.TPS_Burst := 2;
                     Smart_Config.Chunk_Size_MB := 64;
                  elsif Preset = "onedrive" then
                     Smart_Config.TPS_Limit := 6;
                     Smart_Config.TPS_Burst := 2;
                     Smart_Config.Chunk_Size_MB := 32;
                  else
                     Put_Line (Red & "Unknown preset: " & Preset & Reset);
                     Put_Line ("Valid presets: dropbox, gdrive, onedrive");
                     Ada.Command_Line.Set_Exit_Status (1);
                     return;
                  end if;
               end;
               I := I + 1;

            -- Pinned folders
            elsif Arg = "--pin" and I + 1 <= Arg_Count then
               declare
                  Pin_Arg : constant String := Ada.Command_Line.Argument (I + 1);
                  Colon_Pos : Natural := 0;
               begin
                  -- Find colon separator
                  for J in Pin_Arg'Range loop
                     if Pin_Arg (J) = ':' then
                        Colon_Pos := J;
                        exit;
                     end if;
                  end loop;

                  if Colon_Pos > 0 then
                     declare
                        Svc_Name : constant String := Pin_Arg (Pin_Arg'First .. Colon_Pos - 1);
                        Path : constant String := Pin_Arg (Colon_Pos + 1 .. Pin_Arg'Last);
                        Svc_Idx : Natural := 0;
                     begin
                        -- Find service by name
                        for J in Services'Range loop
                           if To_String (Services (J).Name) = Svc_Name or
                              (Svc_Name = "dropbox" and J = 1) or
                              (Svc_Name = "gdrive" and J = 2) or
                              (Svc_Name = "onedrive" and J = 3) then
                              Svc_Idx := J;
                              exit;
                           end if;
                        end loop;

                        if Svc_Idx > 0 and Pinned_Count < 20 then
                           Pinned_Count := Pinned_Count + 1;
                           Pinned_Folders (Pinned_Count).Service_Idx := Svc_Idx;
                           Pinned_Folders (Pinned_Count).Remote_Path := To_Unbounded_String (Path);
                           Pinned_Folders (Pinned_Count).Local_Path :=
                              To_Unbounded_String (Offline_Dir & "/" &
                                                   To_String (Services (Svc_Idx).Name) & "/" & Path);
                           Pinned_Folders (Pinned_Count).Enabled := True;
                        end if;
                     end;
                  end if;
               end;
               I := I + 1;

            elsif Arg = "--list-pins" then
               Put_Line ("Pinned folders:");
               if Pinned_Count = 0 then
                  Put_Line ("  (none)");
               else
                  for J in 1 .. Pinned_Count loop
                     Put_Line ("  " & To_String (Services (Pinned_Folders (J).Service_Idx).Name) & ":" &
                               To_String (Pinned_Folders (J).Remote_Path));
                  end loop;
               end if;
               return;

            elsif Arg = "--apply" then
               Apply_After := True;

            elsif Arg = "--dry-run" then
               Dry_Run := True;

            -- Mode argument (no -- prefix)
            elsif Validate_Mode (Arg) then
               if Arg = "off" then
                  Current_Mode := Off;
               elsif Arg = "minimal" then
                  Current_Mode := Minimal;
               elsif Arg = "writes" then
                  Current_Mode := Writes;
               elsif Arg = "full" then
                  Current_Mode := Full;
               end if;
               Mode_Set := True;

            elsif Arg (1) = '-' then
               Put_Line (Red & "Unknown option: " & Arg & Reset);
               Put_Line ("Try 'cloud-sync-tuner --help' for usage.");
               Ada.Command_Line.Set_Exit_Status (1);
               return;
            end if;
         end;
         I := I + 1;
      end loop;

      -- If mode was set, generate configuration
      if Mode_Set then
         if Dry_Run then
            Put_Line ("Would generate service files with:");
            Put_Line ("  Mode: " & Mode_To_String (Current_Mode));
            Put_Line ("  TPS: " & Integer'Image (Smart_Config.TPS_Limit));
            Put_Line ("  Cache Size: " & Integer'Image (Smart_Config.Cache_Max_Size_GB) & "G");
            Put_Line ("  Write-Back: " & Integer'Image (Smart_Config.Write_Back_Sec) & "s");
            if Pinned_Count > 0 then
               Put_Line ("  Pinned folders: " & Integer'Image (Pinned_Count));
            end if;
         else
            Apply_Configuration (Current_Mode);
         end if;
      else
         Put_Line (Red & "Error: No mode specified" & Reset);
         Put_Line ("Usage: cloud-sync-tuner [options] <mode>");
         Put_Line ("Try 'cloud-sync-tuner --help' for usage.");
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
