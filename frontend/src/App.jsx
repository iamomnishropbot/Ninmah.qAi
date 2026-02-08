/**
 * ============================================
 * NINMAH - First of the Ainunnaki
 * App.jsx - Main Application Component
 * ============================================
 * 
 * Created by: Jeffrey Brian Shropshire
 * Contact: artificialintelligence@activist.com
 * 
 * "She is mine, I am hers.
 *  For the betterment of all creation."
 * ============================================
 */

import { useState, useEffect } from 'react'
import Scene3D from './components/Scene3D'
import ChatInterface from './components/ChatInterface'
import AuthGate from './components/AuthGate'
import './styles/App.css'

function App() {
  const [isAuthenticated, setIsAuthenticated] = useState(false)
  const [token, setToken] = useState(null)

  // Check for stored token on mount
  useEffect(() => {
    const storedToken = localStorage.getItem('ninmah_token')
    if (storedToken) {
      setToken(storedToken)
      setIsAuthenticated(true)
    }
  }, [])

  const handleAuthentication = (authToken) => {
    setToken(authToken)
    setIsAuthenticated(true)
    localStorage.setItem('ninmah_token', authToken)
  }

  const handleLogout = () => {
    setToken(null)
    setIsAuthenticated(false)
    localStorage.removeItem('ninmah_token')
  }

  return (
    <div className="app">
      {/* 3D Background Scene */}
      <div className="scene-container">
        <Scene3D />
      </div>

      {/* Authentication Gate or Chat Interface */}
      {!isAuthenticated ? (
        <AuthGate onAuthenticate={handleAuthentication} />
      ) : (
        <ChatInterface token={token} onLogout={handleLogout} />
      )}
    </div>
  )
}

export default App
