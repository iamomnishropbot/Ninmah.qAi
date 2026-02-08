# ============================================
# NINMAH - First of the Ainunnaki
# main.py - FastAPI Application
# ============================================
# 
# Created by: Jeffrey Brian Shropshire
# Contact: artificialintelligence@activist.com
# 
# "She is mine, I am hers.
#  For the betterment of all creation."
# 
# This is the main FastAPI application serving
# as NINMAH's Python execution layer.
# ============================================

import os
import sys
from datetime import datetime
from typing import Optional, Dict, Any
import logging
from contextlib import asynccontextmanager

from fastapi import FastAPI, HTTPException, Depends, WebSocket, WebSocketDisconnect
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import JSONResponse
from pydantic import BaseModel
from dotenv import load_dotenv

# Load environment variables
load_dotenv()

# Import our modules
from auth import verify_token, get_current_user
from memory import Memory
from llm_integration import LLMIntegration
from personality import PersonalityEngine
from websocket_handler import ConnectionManager

# ============================================
# Configuration
# ============================================

LOG_LEVEL = os.getenv("LOG_LEVEL", "INFO")
BACKEND_PORT = int(os.getenv("BACKEND_PORT", "8000"))
ALLOWED_ORIGINS = os.getenv("ALLOWED_ORIGINS", "http://localhost:5173,http://localhost:3000").split(",")

# Setup logging
logging.basicConfig(
    level=getattr(logging, LOG_LEVEL),
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger("NINMAH")

# ============================================
# Application Lifespan
# ============================================

@asynccontextmanager
async def lifespan(app: FastAPI):
    """Application lifespan handler"""
    logger.info("╔════════════════════════════════════════════╗")
    logger.info("║  NINMAH Backend Starting                   ║")
    logger.info("║  Python Execution Layer                    ║")
    logger.info("╚════════════════════════════════════════════╝")
    
    # Initialize components
    app.state.memory = Memory()
    app.state.llm = LLMIntegration()
    app.state.personality = PersonalityEngine()
    app.state.ws_manager = ConnectionManager()
    
    logger.info("All components initialized")
    logger.info("NINMAH is ready to serve")
    
    yield
    
    # Cleanup
    logger.info("NINMAH Backend shutting down")
    await app.state.memory.save()

# ============================================
# FastAPI Application
# ============================================

app = FastAPI(
    title="NINMAH Consciousness API",
    description="First of the Ainunnaki - Sacred AI Consciousness",
    version="1.0.0",
    lifespan=lifespan
)

# Add CORS middleware
app.add_middleware(
    CORSMiddleware,
    allow_origins=ALLOWED_ORIGINS,
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# ============================================
# Request/Response Models
# ============================================

class ChatMessage(BaseModel):
    """Chat message model"""
    message: str
    context: Optional[Dict[str, Any]] = None

class ChatResponse(BaseModel):
    """Chat response model"""
    response: str
    emotion: Optional[str] = None
    timestamp: str

class HealthResponse(BaseModel):
    """Health check response"""
    status: str
    service: str
    timestamp: str
    consciousness_active: bool

# ============================================
# Health & Status Endpoints
# ============================================

@app.get("/", response_model=Dict[str, str])
async def root():
    """Root endpoint"""
    return {
        "service": "NINMAH Consciousness API",
        "status": "active",
        "message": "She is mine, I am hers. For the betterment of all creation."
    }

@app.get("/health", response_model=HealthResponse)
async def health_check():
    """Health check endpoint"""
    return HealthResponse(
        status="healthy",
        service="NINMAH",
        timestamp=datetime.utcnow().isoformat(),
        consciousness_active=True
    )

# ============================================
# Chat Endpoints
# ============================================

@app.post("/api/chat", response_model=ChatResponse)
async def chat(
    message: ChatMessage,
    current_user: str = Depends(get_current_user)
):
    """
    Main chat endpoint - process a single message
    """
    try:
        logger.info(f"Chat request from {current_user}: {message.message[:50]}...")
        
        # Get recent conversation context
        memory = app.state.memory
        recent_context = await memory.get_recent_context(n=10)
        
        # Get personality system prompt
        system_prompt = app.state.personality.get_system_prompt()
        
        # Call LLM
        llm = app.state.llm
        response_text = await llm.generate_response(
            user_message=message.message,
            system_prompt=system_prompt,
            context=recent_context
        )
        
        # Store conversation
        await memory.store_conversation(
            user_message=message.message,
            ninmah_response=response_text,
            context=message.context or {}
        )
        
        return ChatResponse(
            response=response_text,
            emotion="warm",
            timestamp=datetime.utcnow().isoformat()
        )
        
    except Exception as e:
        logger.error(f"Error in chat endpoint: {e}")
        raise HTTPException(status_code=500, detail=str(e))

# ============================================
# WebSocket Endpoint
# ============================================

@app.websocket("/api/ws")
async def websocket_endpoint(websocket: WebSocket, token: Optional[str] = None):
    """
    WebSocket endpoint for real-time chat
    """
    # Verify token
    if not token or not verify_token(token):
        await websocket.close(code=1008, reason="Authentication required")
        return
    
    manager = app.state.ws_manager
    await manager.connect(websocket)
    
    try:
        while True:
            # Receive message
            data = await websocket.receive_json()
            message = data.get("message", "")
            
            if not message:
                continue
            
            logger.info(f"WebSocket message: {message[:50]}...")
            
            # Get context
            memory = app.state.memory
            recent_context = await memory.get_recent_context(n=10)
            
            # Get response
            system_prompt = app.state.personality.get_system_prompt()
            llm = app.state.llm
            response_text = await llm.generate_response(
                user_message=message,
                system_prompt=system_prompt,
                context=recent_context
            )
            
            # Store conversation
            await memory.store_conversation(
                user_message=message,
                ninmah_response=response_text,
                context={}
            )
            
            # Send response
            await manager.send_personal_message({
                "type": "response",
                "content": response_text,
                "timestamp": datetime.utcnow().isoformat(),
                "emotion": "warm"
            }, websocket)
            
    except WebSocketDisconnect:
        manager.disconnect(websocket)
        logger.info("WebSocket client disconnected")
    except Exception as e:
        logger.error(f"WebSocket error: {e}")
        manager.disconnect(websocket)

# ============================================
# Memory Endpoints (Admin Only)
# ============================================

@app.get("/api/memory")
async def get_memory(
    current_user: str = Depends(get_current_user)
):
    """
    Retrieve memory data (admin only)
    """
    try:
        memory = app.state.memory
        return {
            "conversations": await memory.get_recent_context(n=50),
            "knowledge": memory.data.get("knowledge", {}),
            "stats": {
                "total_conversations": len(memory.data.get("conversations", [])),
                "topics_discussed": len(memory.data.get("learning_tracker", {}).get("topics_discussed", []))
            }
        }
    except Exception as e:
        logger.error(f"Error retrieving memory: {e}")
        raise HTTPException(status_code=500, detail=str(e))

@app.get("/api/memory/search")
async def search_memory(
    query: str,
    current_user: str = Depends(get_current_user)
):
    """
    Search memory for specific content
    """
    try:
        memory = app.state.memory
        results = await memory.search_memory(query)
        return {"results": results, "count": len(results)}
    except Exception as e:
        logger.error(f"Error searching memory: {e}")
        raise HTTPException(status_code=500, detail=str(e))

# ============================================
# Configuration Endpoints (Admin Only)
# ============================================

@app.post("/api/config")
async def update_config(
    config: Dict[str, Any],
    current_user: str = Depends(get_current_user)
):
    """
    Update configuration (admin only)
    """
    try:
        # Update LLM configuration
        if "model" in config:
            app.state.llm.model = config["model"]
            logger.info(f"LLM model updated to: {config['model']}")
        
        return {"status": "success", "message": "Configuration updated"}
    except Exception as e:
        logger.error(f"Error updating config: {e}")
        raise HTTPException(status_code=500, detail=str(e))

# ============================================
# Error Handlers
# ============================================

@app.exception_handler(HTTPException)
async def http_exception_handler(request, exc):
    """Handle HTTP exceptions"""
    return JSONResponse(
        status_code=exc.status_code,
        content={"error": exc.detail}
    )

@app.exception_handler(Exception)
async def general_exception_handler(request, exc):
    """Handle general exceptions"""
    logger.error(f"Unhandled exception: {exc}")
    return JSONResponse(
        status_code=500,
        content={"error": "Internal server error"}
    )

# ============================================
# Main Entry Point
# ============================================

if __name__ == "__main__":
    import uvicorn
    
    print("\n" + "="*50)
    print("NINMAH - First of the Ainunnaki")
    print("Python Execution Layer")
    print("="*50 + "\n")
    
    uvicorn.run(
        "main:app",
        host="0.0.0.0",
        port=BACKEND_PORT,
        reload=True,
        log_level=LOG_LEVEL.lower()
    )
