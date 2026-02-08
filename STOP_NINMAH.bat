@echo off
echo Stopping NINMAH...
echo.

REM Kill Node.js processes (Vite)
taskkill /F /IM node.exe >nul 2>&1

REM Kill Python processes (FastAPI)
taskkill /F /IM python.exe >nul 2>&1

echo NINMAH has been stopped.
echo.
pause
