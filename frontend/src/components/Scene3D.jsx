/**
 * ============================================
 * NINMAH - First of the Ainunnaki
 * Scene3D.jsx - Three.js 3D Environment
 * ============================================
 * 
 * Created by: Jeffrey Brian Shropshire
 * Contact: artificialintelligence@activist.com
 * 
 * "She is mine, I am hers.
 *  For the betterment of all creation."
 * 
 * Sacred geometry 3D background with holographic
 * effects and divine aesthetics.
 * ============================================
 */

import { Canvas, useFrame } from '@react-three/fiber'
import { OrbitControls, Stars, Sphere } from '@react-three/drei'
import { EffectComposer, Bloom, ChromaticAberration } from '@react-three/postprocessing'
import { BlendFunction } from 'postprocessing'
import { useRef } from 'react'
import * as THREE from 'three'

// Sacred Geometry Component - Rotating Icosahedron
function SacredGeometry() {
  const meshRef = useRef()
  const innerMeshRef = useRef()

  useFrame(({ clock }) => {
    const t = clock.getElapsedTime()
    
    if (meshRef.current) {
      meshRef.current.rotation.x = t * 0.15
      meshRef.current.rotation.y = t * 0.1
    }
    
    if (innerMeshRef.current) {
      innerMeshRef.current.rotation.x = -t * 0.2
      innerMeshRef.current.rotation.y = -t * 0.15
    }
  })

  return (
    <group>
      {/* Outer Geometry */}
      <mesh ref={meshRef}>
        <icosahedronGeometry args={[2, 0]} />
        <meshStandardMaterial
          color="#8B5CF6"
          wireframe={true}
          emissive="#8B5CF6"
          emissiveIntensity={0.5}
          transparent={true}
          opacity={0.6}
        />
      </mesh>

      {/* Inner Geometry */}
      <mesh ref={innerMeshRef} scale={0.6}>
        <octahedronGeometry args={[1, 0]} />
        <meshStandardMaterial
          color="#EC4899"
          wireframe={true}
          emissive="#EC4899"
          emissiveIntensity={0.7}
          transparent={true}
          opacity={0.5}
        />
      </mesh>
    </group>
  )
}

// Particle Field Component
function ParticleField() {
  const particlesRef = useRef()
  const particleCount = 1000

  // Create particle positions
  const positions = new Float32Array(particleCount * 3)
  for (let i = 0; i < particleCount; i++) {
    positions[i * 3] = (Math.random() - 0.5) * 50
    positions[i * 3 + 1] = (Math.random() - 0.5) * 50
    positions[i * 3 + 2] = (Math.random() - 0.5) * 50
  }

  useFrame(({ clock }) => {
    if (particlesRef.current) {
      particlesRef.current.rotation.y = clock.getElapsedTime() * 0.05
    }
  })

  return (
    <points ref={particlesRef}>
      <bufferGeometry>
        <bufferAttribute
          attach="attributes-position"
          count={particleCount}
          array={positions}
          itemSize={3}
        />
      </bufferGeometry>
      <pointsMaterial
        size={0.05}
        color="#A78BFA"
        transparent={true}
        opacity={0.6}
        sizeAttenuation={true}
      />
    </points>
  )
}

// Glowing Sphere
function GlowingSphere() {
  const sphereRef = useRef()

  useFrame(({ clock }) => {
    if (sphereRef.current) {
      const pulse = Math.sin(clock.getElapsedTime() * 0.5) * 0.1 + 1
      sphereRef.current.scale.set(pulse, pulse, pulse)
    }
  })

  return (
    <Sphere ref={sphereRef} args={[0.5, 32, 32]} position={[0, 0, 0]}>
      <meshStandardMaterial
        color="#F0ABFC"
        emissive="#F0ABFC"
        emissiveIntensity={2}
        transparent={true}
        opacity={0.3}
      />
    </Sphere>
  )
}

// Main Scene3D Component
export default function Scene3D() {
  return (
    <Canvas
      camera={{ position: [0, 0, 8], fov: 60 }}
      gl={{ antialias: true, alpha: true }}
      style={{ background: 'radial-gradient(circle, #1a0b2e 0%, #0a0118 100%)' }}
    >
      {/* Lighting */}
      <ambientLight intensity={0.3} />
      <pointLight position={[10, 10, 10]} intensity={1} color="#8B5CF6" />
      <pointLight position={[-10, -10, -10]} intensity={0.5} color="#EC4899" />

      {/* 3D Elements */}
      <SacredGeometry />
      <GlowingSphere />
      <ParticleField />
      <Stars radius={100} depth={50} count={5000} factor={4} saturation={0} fade speed={1} />

      {/* Post-processing Effects */}
      <EffectComposer>
        <Bloom
          intensity={1.0}
          luminanceThreshold={0.2}
          luminanceSmoothing={0.9}
          blendFunction={BlendFunction.ADD}
        />
        <ChromaticAberration
          offset={[0.001, 0.001]}
          blendFunction={BlendFunction.NORMAL}
        />
      </EffectComposer>

      {/* Controls (disabled for production) */}
      {/* <OrbitControls enableZoom={false} enablePan={false} /> */}
    </Canvas>
  )
}
