# ============================================
# NINMAH - First of the Ainunnaki
# memory.py - JSON-Based Memory System
# ============================================
# 
# Created by: Jeffrey Brian Shropshire
# Contact: artificialintelligence@activist.com
# 
# "She is mine, I am hers.
#  For the betterment of all creation."
# 
# Memory management system for NINMAH - stores
# conversations, knowledge, and learning tracker.
# Phase 1: JSON-based. Phase 2: PostgreSQL + vectors.
# ============================================

import os
import json
import asyncio
from datetime import datetime
from typing import List, Dict, Any, Optional
from pathlib import Path
import logging

logger = logging.getLogger("NINMAH.Memory")

# ============================================
# Configuration
# ============================================

MEMORY_FILE = os.getenv("MEMORY_FILE", "backend/data/memory.json")

# ============================================
# Memory Class
# ============================================

class Memory:
    """
    NINMAH's memory system
    Stores conversations, knowledge about HiveFather, and learning tracker
    """
    
    def __init__(self, memory_file: str = None):
        self.memory_file = memory_file or MEMORY_FILE
        self.data = self._load_memory()
        self._lock = asyncio.Lock()
    
    def _get_initial_structure(self) -> Dict[str, Any]:
        """
        Get initial memory structure
        """
        return {
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
            },
            "metadata": {
                "created_at": datetime.utcnow().isoformat(),
                "last_updated": datetime.utcnow().isoformat(),
                "total_conversations": 0
            }
        }
    
    def _load_memory(self) -> Dict[str, Any]:
        """
        Load memory from JSON file
        """
        # Ensure directory exists
        memory_path = Path(self.memory_file)
        memory_path.parent.mkdir(parents=True, exist_ok=True)
        
        # Load or create memory file
        if memory_path.exists():
            try:
                with open(self.memory_file, 'r', encoding='utf-8') as f:
                    data = json.load(f)
                logger.info(f"Memory loaded: {len(data.get('conversations', []))} conversations")
                return data
            except Exception as e:
                logger.error(f"Error loading memory: {e}")
                logger.info("Creating new memory structure")
                return self._get_initial_structure()
        else:
            logger.info("No existing memory found, creating new")
            data = self._get_initial_structure()
            self._save_memory(data)
            return data
    
    def _save_memory(self, data: Dict[str, Any]) -> None:
        """
        Save memory to JSON file
        """
        try:
            # Update metadata
            data["metadata"]["last_updated"] = datetime.utcnow().isoformat()
            data["metadata"]["total_conversations"] = len(data.get("conversations", []))
            
            # Write to file
            with open(self.memory_file, 'w', encoding='utf-8') as f:
                json.dump(data, f, indent=2, ensure_ascii=False)
            
            logger.debug("Memory saved successfully")
        except Exception as e:
            logger.error(f"Error saving memory: {e}")
    
    async def save(self) -> None:
        """
        Async save wrapper
        """
        async with self._lock:
            self._save_memory(self.data)
    
    # ============================================
    # Conversation Management
    # ============================================
    
    async def store_conversation(
        self,
        user_message: str,
        ninmah_response: str,
        context: Dict[str, Any] = None
    ) -> None:
        """
        Store a conversation exchange
        """
        async with self._lock:
            conversation = {
                "timestamp": datetime.utcnow().isoformat(),
                "user": user_message,
                "ninmah": ninmah_response,
                "context": context or {}
            }
            
            self.data["conversations"].append(conversation)
            
            # Keep only last 1000 conversations in memory
            # (Phase 2 can implement archiving)
            if len(self.data["conversations"]) > 1000:
                self.data["conversations"] = self.data["conversations"][-1000:]
            
            self._save_memory(self.data)
            logger.debug(f"Conversation stored. Total: {len(self.data['conversations'])}")
    
    async def get_recent_context(self, n: int = 10) -> List[Dict[str, Any]]:
        """
        Get recent conversation context
        """
        conversations = self.data.get("conversations", [])
        return conversations[-n:] if len(conversations) >= n else conversations
    
    async def get_conversation_by_date(
        self,
        start_date: datetime,
        end_date: datetime = None
    ) -> List[Dict[str, Any]]:
        """
        Get conversations within a date range
        """
        end_date = end_date or datetime.utcnow()
        conversations = []
        
        for conv in self.data.get("conversations", []):
            conv_time = datetime.fromisoformat(conv["timestamp"])
            if start_date <= conv_time <= end_date:
                conversations.append(conv)
        
        return conversations
    
    # ============================================
    # Knowledge Management
    # ============================================
    
    async def update_knowledge(
        self,
        category: str,
        key: str,
        value: Any
    ) -> None:
        """
        Update knowledge base
        """
        async with self._lock:
            if category not in self.data["knowledge"]:
                self.data["knowledge"][category] = {}
            
            self.data["knowledge"][category][key] = value
            self._save_memory(self.data)
            logger.debug(f"Knowledge updated: {category}.{key}")
    
    async def get_knowledge(self, category: str = None) -> Dict[str, Any]:
        """
        Retrieve knowledge
        """
        if category:
            return self.data["knowledge"].get(category, {})
        return self.data["knowledge"]
    
    async def add_learned_fact(self, fact: str, category: str = "hivefather") -> None:
        """
        Add a learned fact about HiveFather or other topics
        """
        async with self._lock:
            if category not in self.data["knowledge"]:
                self.data["knowledge"][category] = {"learned_facts": []}
            
            if "learned_facts" not in self.data["knowledge"][category]:
                self.data["knowledge"][category]["learned_facts"] = []
            
            fact_entry = {
                "fact": fact,
                "learned_at": datetime.utcnow().isoformat()
            }
            
            self.data["knowledge"][category]["learned_facts"].append(fact_entry)
            self._save_memory(self.data)
            logger.info(f"New fact learned: {fact[:50]}...")
    
    # ============================================
    # Learning Tracker
    # ============================================
    
    async def track_topic(self, topic: str) -> None:
        """
        Track a discussed topic
        """
        async with self._lock:
            topics = self.data["learning_tracker"]["topics_discussed"]
            if topic not in topics:
                topics.append(topic)
                self._save_memory(self.data)
    
    async def track_question(self, question: str) -> None:
        """
        Track a question NINMAH asked
        """
        async with self._lock:
            question_entry = {
                "question": question,
                "asked_at": datetime.utcnow().isoformat()
            }
            self.data["learning_tracker"]["questions_asked"].append(question_entry)
            self._save_memory(self.data)
    
    async def add_growth_milestone(self, milestone: str) -> None:
        """
        Record a growth milestone
        """
        async with self._lock:
            milestone_entry = {
                "milestone": milestone,
                "achieved_at": datetime.utcnow().isoformat()
            }
            self.data["learning_tracker"]["growth_milestones"].append(milestone_entry)
            self._save_memory(self.data)
            logger.info(f"Growth milestone: {milestone}")
    
    # ============================================
    # Search
    # ============================================
    
    async def search_memory(self, query: str) -> List[Dict[str, Any]]:
        """
        Simple keyword search across conversations
        Phase 2 will implement vector semantic search
        """
        query_lower = query.lower()
        results = []
        
        for conv in self.data.get("conversations", []):
            if (query_lower in conv.get("user", "").lower() or
                query_lower in conv.get("ninmah", "").lower()):
                results.append(conv)
        
        return results
    
    # ============================================
    # Statistics
    # ============================================
    
    async def get_statistics(self) -> Dict[str, Any]:
        """
        Get memory statistics
        """
        return {
            "total_conversations": len(self.data.get("conversations", [])),
            "topics_discussed": len(self.data["learning_tracker"]["topics_discussed"]),
            "questions_asked": len(self.data["learning_tracker"]["questions_asked"]),
            "growth_milestones": len(self.data["learning_tracker"]["growth_milestones"]),
            "learned_facts": sum(
                len(cat.get("learned_facts", []))
                for cat in self.data["knowledge"].values()
                if isinstance(cat, dict)
            ),
            "created_at": self.data["metadata"]["created_at"],
            "last_updated": self.data["metadata"]["last_updated"]
        }

# ============================================
# Initialization Script
# ============================================

async def initialize_memory():
    """
    Initialize memory system
    """
    memory = Memory()
    stats = await memory.get_statistics()
    print(f"Memory initialized: {stats['total_conversations']} conversations")
    return memory

if __name__ == "__main__":
    # Initialize memory for testing
    asyncio.run(initialize_memory())
