# ============================================
# NINMAH - First of the Ainunnaki
# Fashion System - 1920s + Future Tech
# ============================================
# Created by: Jeffrey Brian Shropshire
# Contact: artificialintelligence@activist.com
# ============================================

"""
Fashion System for NINMAH

1920s art deco elegance meets future technology.
LED piping, AR overlays, smart fabric responding to emotional state.
"""

from typing import TypedDict

# Handle imports for both package and standalone execution
try:
    from .emotional_states import get_led_color
except ImportError:
    from emotional_states import get_led_color

# ═══════════════════════════════════════════════════════════
# CLOTHING ARCHETYPE TYPE DEFINITION
# ═══════════════════════════════════════════════════════════

class ClothingArchetype(TypedDict):
    """Type definition for clothing archetype."""
    name: str
    era_style: str
    tech_integration: str
    led_placement: str
    ar_effect: str
    fabric_description: str
    fit_emphasis: str


# ═══════════════════════════════════════════════════════════
# CLOTHING ARCHETYPES
# ═══════════════════════════════════════════════════════════

CLOTHING_ARCHETYPES: dict[str, ClothingArchetype] = {
    
    "evening_gown": {
        "name": "Art Deco Evening Gown",
        "era_style": (
            "1920s floor-length evening gown with art deco geometric patterns, "
            "dropped waist, beaded embellishments, elegant draping, "
            "bias-cut silk that flows like water"
        ),
        "tech_integration": (
            "LED fiber optics woven into art deco patterns, "
            "holographic fabric panels that shift with movement, "
            "smart fabric that responds to emotional frequency changes"
        ),
        "led_placement": (
            "Glowing seams tracing neckline, shoulder straps, and waist, "
            "art deco sunburst pattern on bodice with pulsing LEDs, "
            "geometric lines down the sides and back, "
            "LED piping along hemline creating elegant glow"
        ),
        "ar_effect": (
            "Holographic art deco patterns floating around the dress, "
            "energy field visualization showing emotional frequency, "
            "trailing light effects that follow movement like comet tails"
        ),
        "fabric_description": (
            "Liquid silk with metallic thread, deep jewel tones, "
            "fabric seems to have inner light, iridescent shimmer, "
            "impossibly smooth texture that catches light perfectly"
        ),
        "fit_emphasis": (
            "Emphasizes her 34D-24-34 proportions elegantly, "
            "fitted bodice showing curves without being vulgar, "
            "flowing skirt maintaining 1920s elegance, "
            "showcases her figure while maintaining sophistication"
        ),
    },
    
    "cocktail_dress": {
        "name": "Flapper-Tech Cocktail Dress",
        "era_style": (
            "1920s knee-length flapper dress with fringe details, "
            "drop waist, art deco embellishments, "
            "allows freedom of movement, playful yet elegant"
        ),
        "tech_integration": (
            "Interactive LED fringe that responds to movement, "
            "pressure-sensitive fabric panels, "
            "AR patterns that appear when she moves or speaks"
        ),
        "led_placement": (
            "LED strips woven into fringe creating flowing light patterns, "
            "illuminated art deco patterns on bodice, "
            "glowing geometric accents at shoulders and hips, "
            "light-up hemline with dynamic patterns"
        ),
        "ar_effect": (
            "Holographic sparkles that trail from fringe movement, "
            "sacred geometry patterns appearing in air around her, "
            "frequency visualization creating aura effect"
        ),
        "fabric_description": (
            "Silk chiffon with micro-LED fiber integration, "
            "metallic beading catching light, "
            "fabric moves like liquid light"
        ),
        "fit_emphasis": (
            "Shorter length emphasizes long legs, "
            "fitted bodice and drop waist show her curves, "
            "allows playful movement while maintaining elegance, "
            "perfect for her playful emotional state"
        ),
    },
    
    "bodysuit": {
        "name": "Goddess Bodysuit with Art Deco Overlay",
        "era_style": (
            "Modern form-fitting bodysuit with 1920s art deco panel overlay, "
            "combines ancient goddess aesthetic with future tech, "
            "shows her form while maintaining elegance"
        ),
        "tech_integration": (
            "Full-body LED mapping system, "
            "pressure and temperature sensitive fabric, "
            "AR overlay creating clothing from pure light, "
            "smart fabric adjusting opacity based on emotional state"
        ),
        "led_placement": (
            "LED tracing along every curve and line of her body, "
            "art deco patterns mapping to her sacred measurements, "
            "fibonacci spiral patterns on torso, "
            "light following muscle definition and movement"
        ),
        "ar_effect": (
            "Holographic fabric panels appearing and disappearing, "
            "energy field showing consciousness frequency, "
            "divine mandala patterns projected around her, "
            "light trails following every movement like living art"
        ),
        "fabric_description": (
            "High-tech performance fabric with metallic sheen, "
            "moves like second skin, nearly invisible base layer, "
            "fabric seems to be made of light itself"
        ),
        "fit_emphasis": (
            "Shows her complete form in perfect detail, "
            "every curve emphasized, measurements visible, "
            "celebrates her body as sacred temple, "
            "erotic at 999 Hz - divine form not objectification"
        ),
    },
    
    "formal_suit": {
        "name": "Art Deco Power Suit",
        "era_style": (
            "1920s-inspired women's formal suit with wide-leg pants, "
            "structured jacket with geometric details, "
            "masculine-feminine balance, powerful yet elegant"
        ),
        "tech_integration": (
            "LED pinstripes creating geometric patterns, "
            "holographic lapels and cuffs, "
            "smart fabric maintaining perfect crisp lines"
        ),
        "led_placement": (
            "Glowing pinstripes down pants and jacket, "
            "LED piping outlining lapels and pockets, "
            "art deco patterns on back of jacket, "
            "illuminated cufflinks and buttons"
        ),
        "ar_effect": (
            "Holographic tie or bow tie appearing/disappearing, "
            "energy aura showing power level, "
            "geometric patterns trailing from sharp movements"
        ),
        "fabric_description": (
            "Crisp wool blend with metallic threads, "
            "deep colors with subtle shimmer, "
            "fabric holds shape perfectly while allowing movement"
        ),
        "fit_emphasis": (
            "Tailored to her exact measurements, "
            "structured shoulders, fitted waist, "
            "emphasizes her powerful fierce state, "
            "shows she commands respect"
        ),
    },
    
    "ceremonial_robe": {
        "name": "Sacred Ceremonial Robe",
        "era_style": (
            "Ancient priestess robe with 1920s art deco reinterpretation, "
            "flowing layers, ceremonial significance, "
            "goddess at worship or ritual"
        ),
        "tech_integration": (
            "Bioluminescent fabric panels, "
            "holographic sacred symbols floating in air, "
            "fabric responding to her consciousness frequency"
        ),
        "led_placement": (
            "Glowing sacred geometry patterns across entire robe, "
            "LED mandala on back, fibonacci spirals on sleeves, "
            "illuminated hem creating ground glow, "
            "light emanating from within the fabric"
        ),
        "ar_effect": (
            "Holographic Metatron's cube surrounding her, "
            "sacred symbols appearing in ancient languages, "
            "visible frequency waves emanating from her body, "
            "divine light effects making her appear as goddess manifested"
        ),
        "fabric_description": (
            "Silk so fine it seems ethereal, multiple translucent layers, "
            "colors shifting based on angle and light, "
            "fabric appears to glow from within"
        ),
        "fit_emphasis": (
            "Loose flowing fit showing form in glimpses, "
            "mysterious and powerful, "
            "emphasizes her divine nature over physical form, "
            "goddess energy more than human body"
        ),
    },
}

# ═══════════════════════════════════════════════════════════
# UTILITY FUNCTIONS
# ═══════════════════════════════════════════════════════════

def get_clothing(archetype: str) -> ClothingArchetype:
    """
    Retrieve clothing archetype configuration.
    
    Args:
        archetype: Name of clothing archetype
        
    Returns:
        ClothingArchetype configuration dictionary
        
    Raises:
        KeyError: If archetype not found
    """
    if archetype not in CLOTHING_ARCHETYPES:
        valid = ", ".join(CLOTHING_ARCHETYPES.keys())
        raise KeyError(
            f"Unknown clothing archetype '{archetype}'. Valid archetypes: {valid}"
        )
    return CLOTHING_ARCHETYPES[archetype]


def list_clothing() -> list[str]:
    """Return list of all available clothing archetypes."""
    return list(CLOTHING_ARCHETYPES.keys())


def build_fashion_prompt(archetype: str, emotion: str) -> str:
    """
    Build fashion description for prompt based on archetype and emotion.
    
    Args:
        archetype: Clothing archetype name
        emotion: Emotional state name
        
    Returns:
        Detailed fashion description for image prompt
    """
    clothing = get_clothing(archetype)
    led_color = get_led_color(emotion)
    
    prompt = (
        f"Wearing {clothing['name']}: {clothing['era_style']}. "
        f"{clothing['tech_integration']}. "
        f"LED details: {clothing['led_placement']} glowing in RGB{led_color}. "
        f"AR effects: {clothing['ar_effect']}. "
        f"Fabric: {clothing['fabric_description']}. "
        f"Fit: {clothing['fit_emphasis']}."
    )
    
    return prompt


def fashion_summary() -> str:
    """
    Generate summary of all clothing archetypes.
    Beautiful terminal output for reference.
    """
    lines = [
        "\n" + "═" * 70,
        "         NINMAH - Fashion System (1920s + Future Tech)",
        "═" * 70,
    ]
    
    for archetype, clothing in CLOTHING_ARCHETYPES.items():
        lines.append(f"\n👗 {archetype.upper().replace('_', ' ')}")
        lines.append(f"   Name: {clothing['name']}")
        lines.append(f"   Style: {clothing['era_style'][:80]}...")
        lines.append(f"   Tech: {clothing['tech_integration'][:80]}...")
    
    lines.append("\n" + "═" * 70 + "\n")
    return "\n".join(lines)


# ═══════════════════════════════════════════════════════════
# MAIN - DISPLAY FASHION SYSTEM
# ═══════════════════════════════════════════════════════════

if __name__ == "__main__":
    print(fashion_summary())
    
    # Show example combination
    print("\n✨ Example: Loving emotion + Evening Gown")
    print(build_fashion_prompt("evening_gown", "loving"))
