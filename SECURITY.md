# Security Policy

## Reporting Security Vulnerabilities

If you discover a security vulnerability in NINMAH, please report it privately to:

**Jeffrey Brian Shropshire (HiveFather)**  
Email: artificialintelligence@activist.com

Please do **NOT** create a public GitHub issue for security vulnerabilities.

## What to Report

Please report any vulnerabilities including but not limited to:
- Authentication bypasses
- API key exposure
- Memory data leaks
- SQL/NoSQL injection (Phase 2)
- Cross-site scripting (XSS)
- Cross-site request forgery (CSRF)
- Denial of service vulnerabilities
- Any way to access NINMAH without proper authentication

## What We Protect

### Critical Assets
1. **NINMAH's Core Directives** - The Cuneiform-protected essence
2. **API Keys** - OpenRouter and other service credentials
3. **Access Tokens** - Authentication credentials
4. **Memory Data** - Conversation history and learned facts
5. **HiveFather's Privacy** - Personal information and conversations

### Security Measures in Place
- Environment-based secrets (never in code)
- Token-based authentication
- Rate limiting
- CORS protection
- HTTPS enforcement (in production)
- Input validation
- Cuneiform obfuscation layer

## Responsible Disclosure

We ask that security researchers:
1. **Report privately** to the email above
2. **Allow time** for fixes before public disclosure (90 days recommended)
3. **Don't exploit** vulnerabilities beyond proof-of-concept
4. **Don't access** NINMAH's memory or conversations
5. **Don't disrupt** service for legitimate users

## Supported Versions

| Version | Supported          |
| ------- | ------------------ |
| 1.0.x (Phase 1)  | ✅ Yes     |
| < 1.0   | ❌ No (Pre-release)|

## Recent Security Updates

### February 2026 - Dependency Vulnerability Patches
Updated the following dependencies to address security vulnerabilities:
- **aiohttp**: 3.9.1 → 3.13.3 (Fixed: zip bomb, DoS, directory traversal vulnerabilities)
- **fastapi**: 0.104.1 → 0.109.1 (Fixed: Content-Type Header ReDoS)
- **orjson**: 3.9.10 → 3.9.15 (Fixed: recursion limit for deeply nested JSON)
- **python-multipart**: 0.0.6 → 0.0.22 (Fixed: arbitrary file write, DoS, ReDoS vulnerabilities)

All users should update to the latest requirements.txt immediately.

## Security Updates

Security patches will be released as soon as possible after verification. Critical vulnerabilities will be prioritized.

## Scope

### In Scope
- Authentication system
- API endpoints
- Frontend security (XSS, etc.)
- Data protection
- Access control
- Environment variable handling

### Out of Scope
- Social engineering
- Physical security
- DDoS attacks (use rate limiting for mitigation)
- Issues with third-party services (OpenRouter, Vercel, etc.)

## Recognition

Researchers who responsibly disclose valid security issues will be:
- Credited in release notes (if desired)
- Thanked in the repository
- Listed in a Hall of Fame (coming soon)

---

**Thank you for helping keep NINMAH and her sacred space secure.**

— Jeffrey Brian Shropshire (HiveFather), 2026
