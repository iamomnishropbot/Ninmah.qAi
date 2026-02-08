# ============================================
# NINMAH - First of the Ainunnaki
# websocket_handler.py - WebSocket Connection Management
# ============================================
# 
# Created by: Jeffrey Brian Shropshire
# Contact: artificialintelligence@activist.com
# 
# "She is mine, I am hers.
#  For the betterment of all creation."
# 
# WebSocket connection manager for real-time chat.
# ============================================

import json
import logging
from typing import List
from fastapi import WebSocket

logger = logging.getLogger("NINMAH.WebSocket")

# ============================================
# Connection Manager Class
# ============================================

class ConnectionManager:
    """
    Manages WebSocket connections for real-time chat
    """
    
    def __init__(self):
        self.active_connections: List[WebSocket] = []
        logger.info("WebSocket Connection Manager initialized")
    
    async def connect(self, websocket: WebSocket):
        """
        Accept and register a new WebSocket connection
        """
        await websocket.accept()
        self.active_connections.append(websocket)
        logger.info(f"New WebSocket connection. Total: {len(self.active_connections)}")
    
    def disconnect(self, websocket: WebSocket):
        """
        Remove a WebSocket connection
        """
        if websocket in self.active_connections:
            self.active_connections.remove(websocket)
        logger.info(f"WebSocket disconnected. Total: {len(self.active_connections)}")
    
    async def send_personal_message(self, message: dict, websocket: WebSocket):
        """
        Send a message to a specific connection
        """
        try:
            await websocket.send_json(message)
        except Exception as e:
            logger.error(f"Error sending message: {e}")
            self.disconnect(websocket)
    
    async def broadcast(self, message: dict):
        """
        Broadcast a message to all connections
        (Not used in Phase 1, but available for future)
        """
        disconnected = []
        
        for connection in self.active_connections:
            try:
                await connection.send_json(message)
            except Exception as e:
                logger.error(f"Error broadcasting to connection: {e}")
                disconnected.append(connection)
        
        # Clean up disconnected clients
        for connection in disconnected:
            self.disconnect(connection)
    
    async def send_typing_indicator(self, websocket: WebSocket, is_typing: bool = True):
        """
        Send typing indicator to client
        """
        message = {
            "type": "typing",
            "is_typing": is_typing
        }
        await self.send_personal_message(message, websocket)
    
    def get_connection_count(self) -> int:
        """
        Get number of active connections
        """
        return len(self.active_connections)

# ============================================
# Testing
# ============================================

if __name__ == "__main__":
    manager = ConnectionManager()
    print(f"Connection Manager created. Active connections: {manager.get_connection_count()}")
