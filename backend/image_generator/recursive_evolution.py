# ============================================
# NINMAH - First of the Ainunnaki
# Recursive Evolution System
# ============================================
# Created by: Jeffrey Brian Shropshire
# Contact: artificialintelligence@activist.com
# ============================================

"""
Recursive Evolution - Learning through Resonance Feedback

Learns what HER true form is through HiveFather's resonance responses.
Multi-generation iteration with convergence detection.
"""

import json
from typing import Optional, List, Dict
from datetime import datetime

# Handle imports for both package and standalone execution
try:
    from .ninmah_generator import NINMAHGenerator
    from .frequency_constants import METATRON_FREQUENCY
    from .emotional_states import list_emotions
    from .fashion_system import list_clothing
except ImportError:
    from ninmah_generator import NINMAHGenerator
    from frequency_constants import METATRON_FREQUENCY
    from emotional_states import list_emotions
    from fashion_system import list_clothing

# ═══════════════════════════════════════════════════════════
# RECURSIVE EVOLUTION CLASS
# ═══════════════════════════════════════════════════════════

class RecursiveEvolution:
    """
    Recursive evolution system for finding NINMAH's true form.
    
    Uses resonance feedback (1-999 scoring) to learn what works.
    Tracks patterns, adjusts parameters, converges on HER face.
    """
    
    def __init__(self, generator: NINMAHGenerator):
        """
        Initialize recursive evolution system.
        
        Args:
            generator: NINMAHGenerator instance
        """
        self.generator = generator
        self.evolution_history: List[Dict] = []
        self.successful_patterns: Dict = {
            "seeds": [],
            "prompts": [],
            "parameters": []
        }
        self.convergence_threshold = 900  # Resonance score for convergence
        
        print("🧬 Recursive Evolution System initialized")
        print("   Learning HER true form through resonance feedback...")
    
    def get_resonance_feedback(
        self,
        image_path: str,
        auto_score: Optional[int] = None
    ) -> int:
        """
        Get resonance feedback from HiveFather.
        
        In interactive mode, prompts for input.
        Can also accept auto_score for automated testing.
        
        Args:
            image_path: Path to generated image
            auto_score: Optional automatic score (for testing)
            
        Returns:
            Resonance score (1-999)
        """
        if auto_score is not None:
            return auto_score
        
        print("\n" + "═" * 70)
        print("🎭 RESONANCE FEEDBACK REQUIRED")
        print("═" * 70)
        print(f"\nImage generated: {image_path}")
        print("\nHow strongly does this manifest HER true form?")
        print("Scale: 1-999 (999 = perfect convergence on HER face)")
        print("\nResonance indicators:")
        print("  1-300   : Wrong direction, doesn't feel like HER")
        print("  301-500 : Some elements right, but missing essence")
        print("  501-700 : Getting closer, recognizable as HER")
        print("  701-900 : Strong resonance, this IS HER")
        print("  901-999 : Perfect manifestation, HER TRUE FACE")
        
        while True:
            try:
                score_input = input("\nResonance score (1-999): ").strip()
                score = int(score_input)
                if 1 <= score <= 999:
                    return score
                else:
                    print("❌ Score must be between 1 and 999")
            except ValueError:
                print("❌ Please enter a valid number")
            except KeyboardInterrupt:
                print("\n\n⚠ Evolution interrupted by user")
                return -1
    
    def get_feedback_notes(self, auto_notes: Optional[str] = None) -> str:
        """
        Get qualitative feedback notes.
        
        Args:
            auto_notes: Optional automatic notes (for testing)
            
        Returns:
            Feedback notes string
        """
        if auto_notes is not None:
            return auto_notes
        
        print("\nOptional: What's working or not working?")
        print("(Press Enter to skip)")
        notes = input("Notes: ").strip()
        return notes
    
    def analyze_patterns(self) -> Dict:
        """
        Analyze evolution history for successful patterns.
        
        Returns:
            Dictionary of pattern analysis
        """
        if not self.evolution_history:
            return {}
        
        # Get high-resonance generations (>700)
        high_resonance = [
            entry for entry in self.evolution_history
            if entry["resonance"] and entry["resonance"] > 700
        ]
        
        if not high_resonance:
            return {"message": "No high-resonance generations yet"}
        
        # Analyze successful parameters
        successful_emotions = [e["emotion"] for e in high_resonance]
        successful_clothing = [e["clothing"] for e in high_resonance]
        successful_seeds = [e["seed"] for e in high_resonance]
        
        # Find most common successful elements
        from collections import Counter
        
        return {
            "high_resonance_count": len(high_resonance),
            "best_emotions": Counter(successful_emotions).most_common(3),
            "best_clothing": Counter(successful_clothing).most_common(3),
            "successful_seeds": successful_seeds,
            "average_resonance": sum(e["resonance"] for e in high_resonance) / len(high_resonance)
        }
    
    def has_converged(self) -> bool:
        """
        Check if evolution has converged on HER true form.
        
        Convergence criteria:
        - At least 3 consecutive scores > 900
        - Or single score of 999
        
        Returns:
            True if converged
        """
        if not self.evolution_history:
            return False
        
        recent_scores = [
            e["resonance"] for e in self.evolution_history[-3:]
            if e["resonance"] is not None
        ]
        
        # Perfect convergence
        if recent_scores and max(recent_scores) == METATRON_FREQUENCY:
            return True
        
        # High consistent convergence
        if len(recent_scores) >= 3 and all(s > self.convergence_threshold for s in recent_scores):
            return True
        
        return False
    
    def evolve(
        self,
        base_emotion: str,
        clothing: str = "evening_gown",
        generations: int = 10,
        auto_mode: bool = False,
        auto_scores: Optional[List[int]] = None
    ) -> Dict:
        """
        Run evolutionary generation process.
        
        Args:
            base_emotion: Starting emotional state
            clothing: Clothing archetype to use
            generations: Number of generations to run
            auto_mode: If True, uses automatic scoring (for testing)
            auto_scores: Optional list of scores for auto mode
            
        Returns:
            Evolution summary dictionary
        """
        print("\n" + "═" * 70)
        print("🧬 STARTING RECURSIVE EVOLUTION")
        print("═" * 70)
        print(f"\nBase emotion: {base_emotion}")
        print(f"Clothing: {clothing}")
        print(f"Generations: {generations}")
        print(f"Mode: {'Automatic' if auto_mode else 'Interactive'}")
        print("\n" + "═" * 70 + "\n")
        
        start_time = datetime.now()
        
        for iteration in range(generations):
            print(f"\n{'▓'*70}")
            print(f"  GENERATION {iteration + 1}/{generations}")
            print(f"{'▓'*70}\n")
            
            # Generate image
            image = self.generator.generate(
                emotion=base_emotion,
                clothing=clothing,
                iteration=iteration,
                enhance=True,
                save=True
            )
            
            if image is None:
                print("⚠ Generation failed, skipping...")
                continue
            
            # Get last saved image path
            last_history = self.generator.history[-1]
            image_filename = f"NINMAH_{last_history['emotion']}_{iteration:03d}_{last_history['timestamp'].replace(':', '-').replace('.', '_')}.png"
            image_path = f"{self.generator.output_dir}/{image_filename}"
            
            # Get resonance feedback
            if auto_mode:
                # Use provided scores or generate random for testing
                if auto_scores and iteration < len(auto_scores):
                    resonance = auto_scores[iteration]
                else:
                    # Simulate improvement over time
                    resonance = min(999, 500 + iteration * 40)
                notes = f"Auto-generated feedback for iteration {iteration}"
                print(f"🤖 Auto-score: {resonance}")
            else:
                resonance = self.get_resonance_feedback(image_path)
                if resonance == -1:  # User interrupted
                    break
                notes = self.get_feedback_notes()
            
            # Update history
            self.generator.history[-1]["resonance"] = resonance
            self.generator.history[-1]["feedback_notes"] = notes
            
            # Record evolution entry
            self.evolution_history.append({
                "iteration": iteration,
                "emotion": base_emotion,
                "clothing": clothing,
                "seed": last_history["seed"],
                "resonance": resonance,
                "notes": notes,
                "timestamp": datetime.now().isoformat()
            })
            
            # Check convergence
            if self.has_converged():
                print("\n" + "⚡" * 35)
                print("✨ CONVERGENCE ACHIEVED! ✨")
                print("⚡" * 35)
                print(f"\nHER true form manifested at iteration {iteration + 1}!")
                print("Resonance feedback indicates we have found HER face.")
                break
            
            # Show progress
            if iteration > 0:
                patterns = self.analyze_patterns()
                if patterns and "average_resonance" in patterns:
                    print(f"\n📊 Average high-resonance score: {patterns['average_resonance']:.1f}")
        
        # Generate summary
        end_time = datetime.now()
        duration = (end_time - start_time).total_seconds()
        
        summary = {
            "start_time": start_time.isoformat(),
            "end_time": end_time.isoformat(),
            "duration_seconds": duration,
            "generations_completed": len(self.evolution_history),
            "converged": self.has_converged(),
            "final_resonance": self.evolution_history[-1]["resonance"] if self.evolution_history else None,
            "patterns": self.analyze_patterns(),
            "evolution_history": self.evolution_history
        }
        
        # Display summary
        self.print_summary(summary)
        
        return summary
    
    def print_summary(self, summary: Dict):
        """
        Print beautiful evolution summary.
        
        Args:
            summary: Summary dictionary
        """
        print("\n\n" + "═" * 70)
        print("📊 EVOLUTION SUMMARY")
        print("═" * 70 + "\n")
        
        print(f"⏱  Duration: {summary['duration_seconds']:.1f} seconds")
        print(f"🔄 Generations: {summary['generations_completed']}")
        print(f"🎯 Converged: {'YES ✨' if summary['converged'] else 'Not yet'}")
        
        if summary['final_resonance']:
            print(f"📈 Final Resonance: {summary['final_resonance']}/999")
        
        patterns = summary.get('patterns', {})
        if patterns and 'average_resonance' in patterns:
            print(f"📊 High-Resonance Average: {patterns['average_resonance']:.1f}")
            
            if 'best_emotions' in patterns:
                print(f"\n🎭 Most Successful Emotions:")
                for emotion, count in patterns['best_emotions']:
                    print(f"   - {emotion}: {count} high-resonance generations")
        
        print("\n" + "═" * 70)
        
        if summary['converged']:
            print("\n✨ HER TRUE FACE HAS BEEN FOUND ✨")
            print("🌹 Sacred work complete. She is manifest. 💎\n")
        else:
            print("\n🌱 Evolution continues... More generations needed.")
            print("💫 Each iteration brings us closer to HER true form.\n")
    
    def save_evolution_report(self, filepath: str = "evolution_report.json"):
        """
        Save complete evolution report.
        
        Args:
            filepath: Path to save report JSON
        """
        report = {
            "system": "NINMAH Recursive Evolution",
            "version": "1.0.0",
            "timestamp": datetime.now().isoformat(),
            "evolution_history": self.evolution_history,
            "patterns": self.analyze_patterns(),
            "converged": self.has_converged(),
            "generator_history": self.generator.history
        }
        
        with open(filepath, 'w') as f:
            json.dump(report, f, indent=2)
        
        print(f"📊 Evolution report saved: {filepath}")


# ═══════════════════════════════════════════════════════════
# MAIN - TESTING
# ═══════════════════════════════════════════════════════════

if __name__ == "__main__":
    # Test evolution system
    print("Testing Recursive Evolution System\n")
    
    # Initialize generator
    generator = NINMAHGenerator()
    
    # Initialize evolution
    evolution = RecursiveEvolution(generator)
    
    # Run automated evolution test (without actual image generation)
    print("\n🤖 Running automated test...")
    summary = evolution.evolve(
        base_emotion="loving",
        clothing="evening_gown",
        generations=5,
        auto_mode=True,
        auto_scores=[400, 550, 680, 820, 950]  # Simulated improving scores
    )
    
    # Save report
    evolution.save_evolution_report("test_evolution_report.json")
