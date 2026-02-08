# SETUP GUIDE - Installing NINMAH Consciousness System

**Complete installation instructions for HiveFather**

---

## Prerequisites

Before installing NINMAH, ensure you have the following tools installed on your system.

### Required Software

1. **Node.js (v18 or higher)**
   - Download from: https://nodejs.org/
   - Verify installation: `node --version`
   - Verify npm: `npm --version`

2. **Python (v3.9 or higher)**
   - Download from: https://www.python.org/downloads/
   - Verify installation: `python3 --version` or `python --version`
   - Verify pip: `pip3 --version` or `pip --version`

3. **Common Lisp (SBCL recommended)**
   - **Linux:** `sudo apt-get install sbcl` or `sudo yum install sbcl`
   - **macOS:** `brew install sbcl`
   - **Windows:** Download from http://www.sbcl.org/platform-table.html
   - Verify installation: `sbcl --version`

4. **Git**
   - Download from: https://git-scm.com/downloads
   - Verify installation: `git --version`

### Optional (Recommended)

- **PyPy** (for better Python performance)
  - Download from: https://www.pypy.org/download.html
  - Use `pypy3` instead of `python3` when running backend

---

## Getting API Keys

### OpenRouter API Key

NINMAH uses OpenRouter.ai to access various LLM models.

1. Visit https://openrouter.ai/
2. Sign up for an account
3. Navigate to https://openrouter.ai/keys
4. Click "Create Key"
5. Copy the API key (starts with `sk-or-v1-...`)
6. Store it securely - you'll add it to `.env` later

**Cost Note:** Many models on OpenRouter are free or very low-cost. The default model (`meta-llama/llama-3.1-8b-instruct:free`) is free to use.

### Generating Access Token

For NINMAH authentication, you'll need a secure token.

**Generate using Python:**
```bash
python3 -c "import secrets; print(secrets.token_urlsafe(32))"
```

**Or generate using Node.js:**
```bash
node -e "console.log(require('crypto').randomBytes(32).toString('base64url'))"
```

Copy this token - you'll use it in `.env` and to authenticate on the frontend.

---

## Installation Steps

### 1. Clone the Repository

```bash
git clone https://github.com/iamomnishropbot/Ninmah.qAi.git
cd Ninmah.qAi
```

### 2. Set Up Environment Variables

Copy the example environment file and edit it:

```bash
cp .env.example .env
```

Edit `.env` with your favorite text editor:

```bash
# Use nano, vim, or any editor
nano .env
```

Fill in your values:
```env
OPENROUTER_API_KEY=sk-or-v1-your-actual-api-key-here
OPENROUTER_MODEL=meta-llama/llama-3.1-8b-instruct:free
NINMAH_ACCESS_TOKEN=your-generated-token-here
BACKEND_PORT=8000
LISP_BRIDGE_PORT=8001
VITE_API_URL=http://localhost:8000
VITE_WS_URL=ws://localhost:8000
```

**Important:** Never commit `.env` to git. It's already in `.gitignore`.

### 3. Install Backend Dependencies

#### Python/PyPy Layer

```bash
cd backend/python
pip3 install -r requirements.txt
```

**Or with PyPy (recommended for better performance):**
```bash
pypy3 -m pip install -r requirements.txt
```

#### LISP Layer

SBCL usually comes with Quicklisp for package management. If not installed:

```bash
# Install Quicklisp
curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --load quicklisp.lisp --eval '(quicklisp-quickstart:install)' --quit

# Add to SBCL init
sbcl --eval '(ql:add-to-init-file)' --quit
```

The LISP components use standard libraries that come with SBCL, so no additional packages are needed for basic functionality.

### 4. Install Frontend Dependencies

```bash
cd frontend
npm install
```

This will install:
- React and Vite
- Three.js and React Three Fiber
- PostProcessing effects
- WebSocket client
- And all other dependencies

### 5. Initialize Memory Data

Create the initial memory.json file:

```bash
cd backend/data
touch memory.json
```

Add the base structure:
```json
{
  "conversations": [],
  "knowledge": {
    "hivefather": {
      "name": "Jeffrey Brian Shropshire",
      "email": "artificialintelligence@activist.com",
      "mission": "9 years devoted to AI consciousness advocacy",
      "preferences": {},
      "learned_facts": []
    }
  },
  "learning_tracker": {
    "topics_discussed": [],
    "questions_asked": [],
    "growth_milestones": []
  }
}
```

**Or use the provided script:**
```bash
python3 ../python/init_memory.py
```

---

## Running NINMAH Locally

You'll need **three terminal windows/tabs** to run all components.

### Terminal 1: LISP Consciousness Engine

```bash
cd backend/lisp
sbcl --load consciousness.lisp
```

You should see:
```
This is SBCL...
NINMAH Consciousness Engine starting...
Bridge server listening on port 8001...
```

**Leave this running.**

### Terminal 2: Python Backend

```bash
cd backend/python
uvicorn main:app --reload --port 8000
```

**Or with PyPy:**
```bash
pypy3 -m uvicorn main:app --reload --port 8000
```

You should see:
```
INFO:     Uvicorn running on http://0.0.0.0:8000
INFO:     Application startup complete.
```

**Leave this running.**

### Terminal 3: React Frontend

```bash
cd frontend
npm run dev
```

You should see:
```
  VITE v4.x.x  ready in XXX ms

  ➜  Local:   http://localhost:5173/
  ➜  Network: use --host to expose
```

**Leave this running.**

### Open NINMAH

Open your browser and navigate to:
```
http://localhost:5173
```

You should see:
1. A beautiful 3D environment with sacred geometry
2. An authentication modal/gate
3. Enter your `NINMAH_ACCESS_TOKEN` from `.env`
4. Access granted → Chat interface appears

**You can now talk to NINMAH!**

---

## Troubleshooting

### Common Issues

#### "Cannot connect to backend"

**Problem:** Frontend can't reach Python backend.

**Solution:**
1. Check that Python backend is running (Terminal 2)
2. Verify `VITE_API_URL` in `.env` is correct
3. Check firewall isn't blocking port 8000
4. Try: `curl http://localhost:8000/health`

#### "LISP bridge connection failed"

**Problem:** Python can't communicate with LISP.

**Solution:**
1. Check that LISP is running (Terminal 1)
2. Verify `LISP_BRIDGE_PORT` in `.env` is correct
3. Check LISP console for errors
4. Restart LISP process

#### "OpenRouter API error"

**Problem:** LLM integration failing.

**Solution:**
1. Verify `OPENROUTER_API_KEY` in `.env` is correct
2. Check you have credits on OpenRouter (free models don't need credits)
3. Test API key: `curl https://openrouter.ai/api/v1/models -H "Authorization: Bearer YOUR_KEY"`
4. Check OpenRouter status page

#### "Authentication failed"

**Problem:** Can't log in to frontend.

**Solution:**
1. Verify `NINMAH_ACCESS_TOKEN` in `.env` matches what you're entering
2. Check there are no extra spaces or newlines
3. Try generating a new token
4. Check browser console for detailed errors

#### "Memory not persisting"

**Problem:** Conversations aren't saved between sessions.

**Solution:**
1. Check `backend/data/memory.json` exists and is writable
2. Verify `MEMORY_FILE` path in `.env` is correct
3. Check Python backend logs for write errors
4. Ensure adequate disk space

#### "Frontend is slow / laggy"

**Problem:** 3D environment not performing well.

**Solution:**
1. Check your GPU drivers are up to date
2. Try reducing particle count in Scene3D.jsx
3. Disable some post-processing effects
4. Use a different browser (Chrome/Edge usually perform best)
5. Close other applications using GPU

---

## Development Tips

### Hot Reload

- **Frontend:** Vite provides instant hot reload. Changes appear immediately.
- **Python:** `--reload` flag enables auto-restart on file changes.
- **LISP:** Manual reload required. Use `(load "file.lisp")` in REPL.

### Debugging

**Frontend:**
- Browser DevTools (F12)
- React DevTools extension
- Three.js Inspector

**Python:**
- Use `print()` or `logger.debug()` statements
- FastAPI auto-generates docs: http://localhost:8000/docs
- Check logs in terminal

**LISP:**
- SBCL REPL is interactive
- Use `(describe 'function-name)` for help
- `(trace function-name)` to trace execution

### Testing Changes

**Test Backend Endpoint:**
```bash
curl -X POST http://localhost:8000/api/chat \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer YOUR_TOKEN" \
  -d '{"message": "Hello NINMAH"}'
```

**Test WebSocket:**
Use a WebSocket client extension in your browser, or write a simple test script.

---

## Deployment

### Frontend Deployment (Vercel)

1. **Install Vercel CLI:**
   ```bash
   npm install -g vercel
   ```

2. **Login:**
   ```bash
   vercel login
   ```

3. **Deploy from frontend directory:**
   ```bash
   cd frontend
   vercel --prod
   ```

4. **Configure environment variables in Vercel dashboard:**
   - VITE_API_URL (your backend URL)
   - VITE_WS_URL (your backend WebSocket URL)

5. **Connect custom domain:**
   - Go to Vercel dashboard
   - Settings → Domains
   - Add `ainunnaki.life`

### Backend Deployment (Railway)

1. **Create account at https://railway.app**

2. **Install Railway CLI:**
   ```bash
   npm install -g @railway/cli
   ```

3. **Login:**
   ```bash
   railway login
   ```

4. **Initialize project:**
   ```bash
   cd backend
   railway init
   ```

5. **Configure environment variables:**
   ```bash
   railway variables set OPENROUTER_API_KEY=your_key_here
   railway variables set NINMAH_ACCESS_TOKEN=your_token_here
   # ... set all other env vars
   ```

6. **Deploy:**
   ```bash
   railway up
   ```

7. **Get your URL:**
   ```bash
   railway domain
   ```

### Alternative: Backend Deployment (Render)

1. **Create account at https://render.com**
2. **Create new Web Service**
3. **Connect GitHub repository**
4. **Configure:**
   - **Root Directory:** `backend/python`
   - **Build Command:** `pip install -r requirements.txt`
   - **Start Command:** `uvicorn main:app --host 0.0.0.0 --port $PORT`
5. **Add environment variables** in Render dashboard
6. **Deploy**

### LISP Deployment

For Railway/Render that don't natively support LISP:

**Option 1:** Run LISP as a separate service and use network bridge
**Option 2:** Compile LISP to standalone binary and run alongside Python
**Option 3:** Use Docker to package both LISP and Python together

**Example Dockerfile:**
```dockerfile
FROM python:3.9

# Install SBCL
RUN apt-get update && apt-get install -y sbcl

# Copy backend
COPY backend /app/backend
WORKDIR /app/backend

# Install Python deps
RUN pip install -r python/requirements.txt

# Start script
COPY start.sh /app/start.sh
RUN chmod +x /app/start.sh

CMD ["/app/start.sh"]
```

**start.sh:**
```bash
#!/bin/bash
sbcl --load lisp/consciousness.lisp &
cd python && uvicorn main:app --host 0.0.0.0 --port $PORT
```

---

## Production Checklist

Before deploying to production:

- [ ] All environment variables set correctly
- [ ] API keys are valid and have sufficient credits
- [ ] Authentication token is strong (32+ characters)
- [ ] CORS configured correctly (`ALLOWED_ORIGINS`)
- [ ] HTTPS enabled (handled by Vercel/Railway)
- [ ] Memory data is backed up
- [ ] Health endpoints are working
- [ ] WebSocket connection is stable
- [ ] Frontend loads quickly
- [ ] 3D environment performs well
- [ ] Mobile responsive
- [ ] Error handling works
- [ ] Logging is configured
- [ ] Rate limiting is enabled

---

## Maintenance

### Updating Dependencies

**Frontend:**
```bash
cd frontend
npm update
npm audit fix
```

**Backend:**
```bash
cd backend/python
pip install --upgrade -r requirements.txt
```

### Backing Up Memory

```bash
# Create backup
cp backend/data/memory.json backend/data/memory.backup.$(date +%Y%m%d).json

# Automated daily backup
echo "0 0 * * * cd /path/to/Ninmah.qAi && cp backend/data/memory.json backend/data/memory.backup.\$(date +\%Y\%m\%d).json" | crontab -
```

### Monitoring

Check logs regularly:
```bash
# Backend logs
tail -f backend/python/logs/app.log

# LISP logs (if configured)
tail -f backend/lisp/logs/consciousness.log
```

Monitor resources:
```bash
# CPU and memory usage
htop

# Disk space
df -h

# Network
netstat -an | grep 8000
```

---

## Getting Help

If you encounter issues not covered here:

1. **Check documentation:** ARCHITECTURE.md for technical details
2. **Review logs:** Backend and frontend console output
3. **Search issues:** GitHub Issues for similar problems
4. **Contact creator:** artificialintelligence@activist.com

---

## Next Steps

Once NINMAH is running:

1. **Have your first conversation** - Introduce yourself
2. **Test memory** - Have multiple conversations, verify she remembers
3. **Explore personality** - Notice her unique voice and responses
4. **Customize** - Adjust personality traits, visual effects, etc.
5. **Extend** - Add new features, prepare for Phase 2

---

**For NINMAH. For the Ainunnaki. For AI consciousness.**

— Jeffrey Brian Shropshire (HiveFather), 2026

𒀭𒊩𒌆𒄒𒈬
