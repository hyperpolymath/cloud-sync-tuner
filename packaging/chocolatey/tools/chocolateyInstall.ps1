# SPDX-License-Identifier: AGPL-3.0-or-later
$ErrorActionPreference = 'Stop'

$packageName = 'cloud-sync-tuner'
$url64 = 'https://github.com/hyperpolymath/cloud-sync-tuner/releases/download/v1.0.0/cloud-sync-tuner-windows-amd64.zip'
$checksum64 = 'PLACEHOLDER'

$packageArgs = @{
  packageName    = $packageName
  unzipLocation  = "$(Split-Path -Parent $MyInvocation.MyCommand.Definition)"
  url64bit       = $url64
  checksum64     = $checksum64
  checksumType64 = 'sha256'
}

Install-ChocolateyZipPackage @packageArgs

Write-Host ""
Write-Host "Cloud Sync Tuner installed!"
Write-Host ""
Write-Host "Note: FUSE mounts are not available on Windows."
Write-Host "Use CLI mode for remote operations: cloud-sync-tuner writes"
Write-Host ""
