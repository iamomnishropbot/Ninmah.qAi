@echo off
echo Stopping NINMAH...
echo.
echo Warning: This will stop Python and Node.js processes.
echo If you have other Python/Node.js applications running, they will also be stopped.
echo.
pause

REM Kill Node.js processes (Vite)
taskkill /F /IM node.exe >nul 2>&1

REM Kill Python processes (FastAPI)
taskkill /F /IM python.exe >nul 2>&1

echo NINMAH has been stopped.
echo.
pause
