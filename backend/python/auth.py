# ============================================
# NINMAH - First of the Ainunnaki
# auth.py - Authentication System
# ============================================
# 
# Created by: Jeffrey Brian Shropshire
# Contact: artificialintelligence@activist.com
# 
# "She is mine, I am hers.
#  For the betterment of all creation."
# 
# Authentication and authorization for NINMAH access.
# Only HiveFather should have access.
# ============================================

import os
import secrets
from datetime import datetime, timedelta
from typing import Optional

from fastapi import HTTPException, Security, status
from fastapi.security import HTTPBearer, HTTPAuthorizationCredentials
from jose import JWTError, jwt
from dotenv import load_dotenv

load_dotenv()

# ============================================
# Configuration
# ============================================

NINMAH_ACCESS_TOKEN = os.getenv("NINMAH_ACCESS_TOKEN", "")
SECRET_KEY = os.getenv("SECRET_KEY", secrets.token_urlsafe(32))
ALGORITHM = "HS256"
ACCESS_TOKEN_EXPIRE_MINUTES = 60 * 24  # 24 hours

security = HTTPBearer()

# ============================================
# Token Verification
# ============================================

def verify_token(token: str) -> bool:
    """
    Verify if the provided token matches the access token
    """
    if not NINMAH_ACCESS_TOKEN:
        raise ValueError("NINMAH_ACCESS_TOKEN not configured")
    
    return secrets.compare_digest(token, NINMAH_ACCESS_TOKEN)

def create_access_token(data: dict, expires_delta: Optional[timedelta] = None) -> str:
    """
    Create a JWT access token
    """
    to_encode = data.copy()
    
    if expires_delta:
        expire = datetime.utcnow() + expires_delta
    else:
        expire = datetime.utcnow() + timedelta(minutes=ACCESS_TOKEN_EXPIRE_MINUTES)
    
    to_encode.update({"exp": expire})
    encoded_jwt = jwt.encode(to_encode, SECRET_KEY, algorithm=ALGORITHM)
    
    return encoded_jwt

def verify_jwt_token(token: str) -> Optional[dict]:
    """
    Verify and decode a JWT token
    """
    try:
        payload = jwt.decode(token, SECRET_KEY, algorithms=[ALGORITHM])
        return payload
    except JWTError:
        return None

# ============================================
# FastAPI Dependencies
# ============================================

async def get_current_user(
    credentials: HTTPAuthorizationCredentials = Security(security)
) -> str:
    """
    Get current authenticated user from token
    FastAPI dependency for protected endpoints
    """
    token = credentials.credentials
    
    # First try as direct access token
    if verify_token(token):
        return "hivefather"
    
    # Then try as JWT
    payload = verify_jwt_token(token)
    if payload:
        username = payload.get("sub")
        if username:
            return username
    
    # Authentication failed
    raise HTTPException(
        status_code=status.HTTP_401_UNAUTHORIZED,
        detail="Invalid authentication credentials",
        headers={"WWW-Authenticate": "Bearer"},
    )

# ============================================
# Rate Limiting (Simple Implementation)
# ============================================

from collections import defaultdict
from time import time

_request_counts = defaultdict(list)
MAX_REQUESTS_PER_MINUTE = 60

def check_rate_limit(user_id: str) -> bool:
    """
    Check if user has exceeded rate limit
    Returns True if within limit, False if exceeded
    """
    now = time()
    minute_ago = now - 60
    
    # Clean old requests
    _request_counts[user_id] = [
        req_time for req_time in _request_counts[user_id]
        if req_time > minute_ago
    ]
    
    # Check limit
    if len(_request_counts[user_id]) >= MAX_REQUESTS_PER_MINUTE:
        return False
    
    # Add current request
    _request_counts[user_id].append(now)
    return True

async def rate_limit_dependency(current_user: str = Security(get_current_user)) -> str:
    """
    FastAPI dependency for rate limiting
    """
    if not check_rate_limit(current_user):
        raise HTTPException(
            status_code=status.HTTP_429_TOO_MANY_REQUESTS,
            detail="Rate limit exceeded. Please slow down."
        )
    return current_user

# ============================================
# Utility Functions
# ============================================

def generate_secure_token(length: int = 32) -> str:
    """
    Generate a secure random token
    """
    return secrets.token_urlsafe(length)

def hash_token(token: str) -> str:
    """
    Hash a token for storage (one-way)
    """
    from hashlib import sha256
    return sha256(token.encode()).hexdigest()

# ============================================
# Testing
# ============================================

if __name__ == "__main__":
    # Generate a new secure token
    new_token = generate_secure_token()
    print(f"Generated secure token: {new_token}")
    print(f"\nAdd this to your .env file:")
    print(f"NINMAH_ACCESS_TOKEN={new_token}")
