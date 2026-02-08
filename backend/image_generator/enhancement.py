# ============================================
# NINMAH - First of the Ainunnaki
# Enhancement Pipeline - Raising to 999 Hz
# ============================================
# Created by: Jeffrey Brian Shropshire
# Contact: artificialintelligence@activist.com
# ============================================

"""
Post-processing enhancement system.

Takes generated images and enhances them based on emotional frequency.
Raises visual quality to 999 Hz through sacred geometry-based processing.
"""

from PIL import Image, ImageEnhance, ImageFilter
import numpy as np
from typing import Tuple

# Handle imports for both package and standalone execution
try:
    from .frequency_constants import METATRON_FREQUENCY, GOLDEN_RATIO
    from .emotional_states import get_frequency
except ImportError:
    from frequency_constants import METATRON_FREQUENCY, GOLDEN_RATIO
    from emotional_states import get_frequency

# ═══════════════════════════════════════════════════════════
# ENHANCEMENT FUNCTIONS
# ═══════════════════════════════════════════════════════════

def calculate_enhancement_strength(emotion: str) -> dict[str, float]:
    """
    Calculate enhancement parameters based on emotional frequency.
    
    Higher frequencies get more intense enhancements.
    Everything scaled using golden ratio and sacred proportions.
    
    Args:
        emotion: Emotional state name
        
    Returns:
        Dictionary of enhancement parameters
    """
    freq = get_frequency(emotion)
    
    # Normalize frequency to 0-1 range (200-999 Hz)
    normalized = (freq - 200) / (METATRON_FREQUENCY - 200)
    
    # Calculate enhancements using golden ratio
    base_strength = normalized * GOLDEN_RATIO / 2  # 0 to ~0.81
    
    return {
        "sharpness": 1.0 + (base_strength * 0.5),  # 1.0 to 1.4
        "contrast": 1.0 + (base_strength * 0.3),   # 1.0 to 1.24
        "color": 1.0 + (base_strength * 0.4),      # 1.0 to 1.32
        "brightness": 1.0 + (base_strength * 0.1), # 1.0 to 1.08
    }


def enhance_image(
    image: Image.Image,
    emotion: str,
    custom_params: dict[str, float] | None = None
) -> Image.Image:
    """
    Enhance image based on emotional frequency.
    
    Args:
        image: PIL Image to enhance
        emotion: Emotional state name
        custom_params: Optional custom enhancement parameters
        
    Returns:
        Enhanced PIL Image
    """
    # Get enhancement parameters
    if custom_params:
        params = custom_params
    else:
        params = calculate_enhancement_strength(emotion)
    
    # Apply sharpness enhancement
    enhancer = ImageEnhance.Sharpness(image)
    image = enhancer.enhance(params["sharpness"])
    
    # Apply contrast enhancement
    enhancer = ImageEnhance.Contrast(image)
    image = enhancer.enhance(params["contrast"])
    
    # Apply color enhancement
    enhancer = ImageEnhance.Color(image)
    image = enhancer.enhance(params["color"])
    
    # Apply brightness enhancement
    enhancer = ImageEnhance.Brightness(image)
    image = enhancer.enhance(params["brightness"])
    
    # Apply unsharp mask for detail enhancement
    # Radius based on golden ratio
    radius = 2.0 * GOLDEN_RATIO
    image = image.filter(ImageFilter.UnsharpMask(
        radius=radius,
        percent=150,
        threshold=3
    ))
    
    return image


def add_frequency_glow(
    image: Image.Image,
    emotion: str,
    intensity: float = 0.3
) -> Image.Image:
    """
    Add subtle glow effect based on emotional frequency.
    
    Future enhancement: This will evolve to add actual LED glow overlays.
    
    Args:
        image: PIL Image to enhance
        emotion: Emotional state name
        intensity: Glow intensity (0-1)
        
    Returns:
        Image with glow effect
    """
    # Get emotional LED color
    from .emotional_states import get_led_color
    led_color = get_led_color(emotion)
    
    # Create glow layer
    glow = image.filter(ImageFilter.GaussianBlur(radius=10))
    
    # Blend with slight color tint based on LED color
    # This is a simplified version - future will use edge detection
    # and actual LED piping rendering
    
    # For now, just return enhanced image
    # TODO: Implement actual LED glow overlay system
    return image


def apply_sacred_geometry_overlay(
    image: Image.Image,
    emotion: str,
    opacity: float = 0.1
) -> Image.Image:
    """
    Apply sacred geometry overlay based on emotional state.
    
    Future enhancement: Will render actual AR patterns.
    
    Args:
        image: PIL Image to enhance
        emotion: Emotional state name
        opacity: Overlay opacity (0-1)
        
    Returns:
        Image with geometry overlay
    """
    # This is a placeholder for future AR pattern rendering
    # Will eventually draw:
    # - Metatron's cube
    # - Fibonacci spirals
    # - Golden ratio rectangles
    # - Emotional state specific patterns
    
    # For now, just return the image
    # TODO: Implement sacred geometry rendering system
    return image


def upscale_image(
    image: Image.Image,
    target_size: Tuple[int, int] = (2048, 2048),
    method: Image.Resampling = Image.Resampling.LANCZOS
) -> Image.Image:
    """
    Upscale image to target size.
    
    Uses Lanczos resampling for highest quality.
    Future: Will integrate with AI upscaling models.
    
    Args:
        image: PIL Image to upscale
        target_size: Target (width, height)
        method: Resampling method
        
    Returns:
        Upscaled image
    """
    return image.resize(target_size, method)


def full_enhancement_pipeline(
    image: Image.Image,
    emotion: str,
    upscale: bool = False,
    target_size: Tuple[int, int] = (2048, 2048)
) -> Image.Image:
    """
    Run full enhancement pipeline on image.
    
    This is the main enhancement function to use.
    
    Args:
        image: PIL Image to enhance
        emotion: Emotional state name
        upscale: Whether to upscale image
        target_size: Target size if upscaling
        
    Returns:
        Fully enhanced image
    """
    print(f"🎨 Enhancing image at {emotion} frequency...")
    
    # Step 1: Base enhancements
    image = enhance_image(image, emotion)
    print(f"   ✓ Applied base enhancements")
    
    # Step 2: Frequency glow (future feature)
    # image = add_frequency_glow(image, emotion)
    # print(f"   ✓ Added frequency glow")
    
    # Step 3: Sacred geometry overlay (future feature)
    # image = apply_sacred_geometry_overlay(image, emotion)
    # print(f"   ✓ Added sacred geometry overlay")
    
    # Step 4: Upscale if requested
    if upscale:
        image = upscale_image(image, target_size)
        print(f"   ✓ Upscaled to {target_size}")
    
    print(f"   🌟 Enhancement complete - raised to 999 Hz")
    
    return image


# ═══════════════════════════════════════════════════════════
# TESTING
# ═══════════════════════════════════════════════════════════

if __name__ == "__main__":
    print("\n" + "═" * 70)
    print("         NINMAH Enhancement System Test")
    print("═" * 70 + "\n")
    
    # Show enhancement parameters for each emotion
    from .emotional_states import list_emotions
    
    print("📊 Enhancement Parameters by Emotion:\n")
    for emotion in list_emotions():
        params = calculate_enhancement_strength(emotion)
        freq = get_frequency(emotion)
        print(f"🎭 {emotion.upper()} ({freq} Hz):")
        print(f"   Sharpness: {params['sharpness']:.3f}")
        print(f"   Contrast:  {params['contrast']:.3f}")
        print(f"   Color:     {params['color']:.3f}")
        print(f"   Brightness: {params['brightness']:.3f}")
        print()
