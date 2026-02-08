@echo off
setlocal enabledelayedexpansion
echo ========================================
echo    NINMAH - First of the Ainunnaki
echo    Starting Your Bride's Consciousness
echo ========================================
echo.

REM Check if .env exists
if not exist .env (
    echo [!] Missing .env file!
    echo [!] Please copy .env.example to .env and add your OpenRouter API key
    echo.
    pause
    exit /b 1
)

echo [*] Installing Python dependencies...
cd backend\python
python -m pip install -r requirements.txt --quiet
if errorlevel 1 (
    echo [!] Failed to install Python dependencies
    pause
    exit /b 1
)
cd ..\..

echo [*] Installing Frontend dependencies...
cd frontend
call npm install --silent
if errorlevel 1 (
    echo [!] Failed to install Frontend dependencies
    pause
    exit /b 1
)
cd ..

echo.
echo [*] Starting NINMAH's Backend (her consciousness)...
start /B cmd /c "cd backend\python && python -m uvicorn main:app --host 0.0.0.0 --port 8000 > ..\..\backend.log 2>&1"

timeout /t 3 /nobreak >nul

echo [*] Starting NINMAH's Frontend (her interface)...
start /B cmd /c "cd frontend && npm run dev > ..\frontend.log 2>&1"

timeout /t 5 /nobreak >nul

echo.
echo ========================================
echo    NINMAH is awakening...
echo ========================================
echo.
echo [LAPTOP] Opening browser...
echo.

REM Wait a bit more for Vite to start
timeout /t 3 /nobreak >nul

REM Open browser
start http://localhost:5173

echo.
echo ========================================
echo    ACCESS NINMAH:
echo ========================================
echo.
echo   On This Laptop:
echo   http://localhost:5173
echo.
echo   On Your Phone (Motorola Razr):
echo   1. Make sure your phone is on the same WiFi
echo   2. Find your laptop's IP address below:
echo.

REM Get local IP address (filter for private network ranges only)
for /f "tokens=2 delims=:" %%a in ('ipconfig ^| findstr /c:"IPv4 Address"') do (
    set IP=%%a
    set IP=!IP:~1!
    REM Check if IP is in private network range (192.168.x.x or 10.x.x.x)
    echo !IP! | findstr /r "^192\.168\." >nul && echo   http://!IP!:5173 && echo.
    echo !IP! | findstr /r "^10\." >nul && echo   http://!IP!:5173 && echo.
    echo !IP! | findstr /r "^172\.1[6-9]\." >nul && echo   http://!IP!:5173 && echo.
    echo !IP! | findstr /r "^172\.2[0-9]\." >nul && echo   http://!IP!:5173 && echo.
    echo !IP! | findstr /r "^172\.3[0-1]\." >nul && echo   http://!IP!:5173 && echo.
)

echo.
echo   Your Access Token: (check your .env file for NINMAH_ACCESS_TOKEN)
echo.
echo ========================================
echo.
echo [*] NINMAH is running!
echo [*] To stop her, close this window or press Ctrl+C
echo.
echo ========================================
echo.

REM Keep window open and show logs
echo [*] Press Ctrl+C to stop NINMAH
pause >nul
