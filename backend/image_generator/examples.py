#!/usr/bin/env python3
# ============================================
# NINMAH - First of the Ainunnaki
# Usage Example Script
# ============================================
# Created by: Jeffrey Brian Shropshire
# Contact: artificialintelligence@activist.com
# ============================================

"""
NINMAH 999 Hz Image Generator - Usage Examples

This script demonstrates how to use the NINMAH image generation system.
Run different examples by uncommenting the desired sections.
"""

import sys
import os

# Add parent directory to path for imports
sys.path.insert(0, os.path.dirname(__file__))

from ninmah_generator import NINMAHGenerator
from recursive_evolution import RecursiveEvolution
from emotional_states import list_emotions, emotion_summary
from fashion_system import list_clothing, fashion_summary
from frequency_constants import FREQUENCY_BANNER

def print_header(title: str):
    """Print a beautiful header."""
    print("\n" + "═" * 70)
    print(f"  {title}")
    print("═" * 70 + "\n")


def example_1_show_constants():
    """Example 1: Display all constants and configurations."""
    print(FREQUENCY_BANNER)
    
    print_header("EMOTIONAL STATES")
    print(emotion_summary())
    
    print_header("FASHION SYSTEM")
    print(fashion_summary())


def example_2_prompt_building():
    """Example 2: Build prompts without generation."""
    print_header("PROMPT BUILDING EXAMPLE")
    
    # Initialize generator (won't load SD model if not available)
    generator = NINMAHGenerator()
    
    # Build prompts for different combinations
    combinations = [
        ("loving", "evening_gown"),
        ("fierce", "formal_suit"),
        ("playful", "cocktail_dress"),
        ("erotic", "bodysuit"),
    ]
    
    for emotion, clothing in combinations:
        print(f"\n{'─' * 70}")
        print(f"🎭 {emotion.upper()} + {clothing.upper()}")
        print(f"{'─' * 70}\n")
        
        positive, negative = generator.build_full_prompt(emotion, clothing)
        
        print("POSITIVE PROMPT (first 300 chars):")
        print(positive[:300] + "...\n")
        
        seed = generator.calculate_sacred_seed(emotion, 0)
        print(f"🔢 Sacred Seed: {seed}\n")


def example_3_single_generation():
    """Example 3: Generate a single image."""
    print_header("SINGLE IMAGE GENERATION")
    
    print("This example will generate one image.")
    print("Note: Requires Stable Diffusion model and dependencies installed.\n")
    
    # Initialize generator
    generator = NINMAHGenerator()
    
    # Generate image
    print("Generating NINMAH in loving state with evening gown...")
    image = generator.generate(
        emotion="loving",
        clothing="evening_gown",
        iteration=0,
        enhance=True,
        save=True
    )
    
    if image:
        print("\n✅ Image generated successfully!")
        print(f"📁 Saved to: {generator.output_dir}/")
    else:
        print("\n⚠ Image generation skipped (model not loaded)")
        print("This is normal if Stable Diffusion is not installed.")


def example_4_evolution_auto():
    """Example 4: Run evolution with automatic scoring (demo mode)."""
    print_header("RECURSIVE EVOLUTION - AUTO MODE")
    
    print("This example demonstrates the evolution system in auto mode.")
    print("(Auto mode simulates feedback for testing purposes)\n")
    
    # Initialize generator
    generator = NINMAHGenerator()
    
    # Initialize evolution system
    evolution = RecursiveEvolution(generator)
    
    # Run evolution with automatic scoring
    summary = evolution.evolve(
        base_emotion="loving",
        clothing="evening_gown",
        generations=5,
        auto_mode=True,
        auto_scores=[400, 550, 680, 820, 950]  # Simulated improving scores
    )
    
    # Save report
    evolution.save_evolution_report("example_evolution_report.json")
    
    print("\n✅ Evolution demo complete!")


def example_5_evolution_interactive():
    """Example 5: Run evolution with interactive feedback."""
    print_header("RECURSIVE EVOLUTION - INTERACTIVE MODE")
    
    print("This example will generate multiple images and ask for your feedback.")
    print("You will rate each image on a scale of 1-999 based on resonance.\n")
    print("Note: Requires Stable Diffusion model installed.\n")
    
    response = input("Continue with interactive evolution? (y/n): ")
    if response.lower() != 'y':
        print("Skipping interactive evolution.")
        return
    
    # Initialize generator
    generator = NINMAHGenerator()
    
    if generator.pipe is None:
        print("\n⚠ Stable Diffusion not loaded. Cannot run interactive mode.")
        return
    
    # Initialize evolution system
    evolution = RecursiveEvolution(generator)
    
    # Run evolution
    summary = evolution.evolve(
        base_emotion="loving",
        clothing="evening_gown",
        generations=5,
        auto_mode=False  # Interactive mode
    )
    
    # Save report
    evolution.save_evolution_report("my_evolution_report.json")
    generator.save_session("my_session.json")
    
    print("\n✅ Evolution complete!")


def example_6_all_emotions():
    """Example 6: Generate one image for each emotional state."""
    print_header("ALL EMOTIONAL STATES")
    
    print("This example generates one image for each of the 7 emotional states.")
    print("Note: Requires Stable Diffusion model installed.\n")
    
    response = input("Continue with generation? (y/n): ")
    if response.lower() != 'y':
        print("Skipping generation.")
        return
    
    # Initialize generator
    generator = NINMAHGenerator()
    
    if generator.pipe is None:
        print("\n⚠ Stable Diffusion not loaded. Cannot generate images.")
        return
    
    # Generate one image for each emotion
    emotions = list_emotions()
    clothing = "evening_gown"
    
    for i, emotion in enumerate(emotions):
        print(f"\n{'▓' * 70}")
        print(f"  {i+1}/{len(emotions)} - {emotion.upper()}")
        print(f"{'▓' * 70}")
        
        image = generator.generate(
            emotion=emotion,
            clothing=clothing,
            iteration=i,
            enhance=True,
            save=True
        )
        
        if not image:
            print(f"⚠ Generation failed for {emotion}")
    
    # Save session
    generator.save_session("all_emotions_session.json")
    
    print(f"\n✅ Generated {len(emotions)} images!")
    print(f"📁 Check {generator.output_dir}/ directory")


def main():
    """Main menu for examples."""
    print(FREQUENCY_BANNER)
    print("\n" + "═" * 70)
    print("  NINMAH 999 Hz Image Generator - Usage Examples")
    print("═" * 70 + "\n")
    
    examples = [
        ("1", "Show all constants and configurations", example_1_show_constants),
        ("2", "Build prompts (no generation)", example_2_prompt_building),
        ("3", "Generate single image", example_3_single_generation),
        ("4", "Run evolution (auto mode - demo)", example_4_evolution_auto),
        ("5", "Run evolution (interactive mode)", example_5_evolution_interactive),
        ("6", "Generate all emotional states", example_6_all_emotions),
    ]
    
    print("Available examples:\n")
    for num, desc, _ in examples:
        print(f"  {num}. {desc}")
    
    print("\n  0. Exit\n")
    
    choice = input("Select example (0-6): ").strip()
    
    for num, desc, func in examples:
        if choice == num:
            func()
            return
    
    if choice == "0":
        print("\nExiting. Goodbye! 🌹💎⚡")
    else:
        print(f"\nInvalid choice: {choice}")


if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        print("\n\nInterrupted by user. Goodbye! 🌹💎⚡")
    except Exception as e:
        print(f"\n❌ Error: {e}")
        import traceback
        traceback.print_exc()
