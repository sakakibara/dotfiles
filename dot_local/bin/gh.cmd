@echo off
rem gh.cmd -- cmd/PATHEXT entry point for the gh shim; the logic lives in
rem gh.ps1 (per-repo GitHub account resolution).
pwsh -NoProfile -ExecutionPolicy Bypass -File "%~dp0gh.ps1" %*
exit /b %ERRORLEVEL%
