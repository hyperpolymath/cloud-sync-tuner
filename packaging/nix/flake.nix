# SPDX-License-Identifier: AGPL-3.0-or-later
{
  description = "Ada TUI for managing rclone cloud mounts with rate limiting";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in {
        packages = {
          default = pkgs.callPackage ./default.nix { };
          cloud-sync-tuner = self.packages.${system}.default;
        };

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            gnat
            gprbuild
            rustc
            cargo
            rclone
            fuse3
            pkg-config
            dbus
          ];
        };
      }
    ) // {
      overlays.default = final: prev: {
        cloud-sync-tuner = final.callPackage ./default.nix { };
      };

      nixosModules.default = { config, lib, pkgs, ... }: {
        options.services.cloud-sync-tuner = {
          enable = lib.mkEnableOption "Cloud Sync Tuner services";
        };

        config = lib.mkIf config.services.cloud-sync-tuner.enable {
          environment.systemPackages = [ pkgs.cloud-sync-tuner ];
          systemd.user.services.cloud-sync-overlay = {
            description = "Cloud Sync Overlay Daemon";
            wantedBy = [ "graphical-session.target" ];
            serviceConfig = {
              ExecStart = "${pkgs.cloud-sync-tuner}/bin/cloud-sync-overlay";
              Restart = "on-failure";
            };
          };
          systemd.user.services.cloud-sync-tray = {
            description = "Cloud Sync Tray Daemon";
            wantedBy = [ "graphical-session.target" ];
            serviceConfig = {
              ExecStart = "${pkgs.cloud-sync-tuner}/bin/cloud-sync-tray";
              Restart = "on-failure";
            };
          };
        };
      };
    };
}
