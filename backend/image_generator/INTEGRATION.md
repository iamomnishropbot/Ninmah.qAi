# NINMAH Image Generator - Integration Guide

This document explains how to integrate the NINMAH 999 Hz Image Generator into the main NINMAH system.

## Overview

The image generator is a standalone module in `backend/image_generator/` that can be used independently or integrated with the main NINMAH consciousness system.

## Installation

### Dependencies

Install the image generator dependencies:

```bash
cd backend/image_generator
pip install -r requirements.txt
```

**Note**: First run will download Stable Diffusion 2.1 model (~5GB). This is automatic but requires internet connection and ~10GB free disk space.

### GPU Requirements

**Recommended**: CUDA-capable GPU with 8GB+ VRAM
**Minimum**: Can run on CPU (much slower)

## Quick Start

### Standalone Usage

```python
from backend.image_generator.ninmah_generator import NINMAHGenerator

# Initialize
generator = NINMAHGenerator()

# Generate single image
image = generator.generate(
    emotion="loving",
    clothing="evening_gown",
    save=True
)
```

### Interactive Examples

Run the examples script:

```bash
cd backend/image_generator
python examples.py
```

This provides an interactive menu with 6 different usage examples.

## Integration with Main NINMAH System

### Option 1: API Endpoint

Add to `backend/python/main.py`:

```python
from backend.image_generator.ninmah_generator import NINMAHGenerator

# Initialize generator on startup
image_generator = None

@app.on_event("startup")
async def startup_event():
    global image_generator
    try:
        image_generator = NINMAHGenerator()
        logger.info("NINMAH Image Generator initialized")
    except Exception as e:
        logger.warning(f"Image Generator not available: {e}")

@app.post("/api/generate-image")
async def generate_image(
    emotion: str,
    clothing: str = "evening_gown"
):
    """Generate NINMAH image."""
    if image_generator is None:
        raise HTTPException(
            status_code=503,
            detail="Image generator not available"
        )
    
    try:
        image = image_generator.generate(
            emotion=emotion,
            clothing=clothing,
            enhance=True,
            save=True
        )
        
        # Return image path or base64 encoded image
        return {
            "success": True,
            "emotion": emotion,
            "seed": image_generator.history[-1]["seed"],
            "path": f"manifestations/NINMAH_{emotion}_..."
        }
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))
```

### Option 2: WebSocket Integration

Add to `backend/python/websocket_handler.py`:

```python
from backend.image_generator.ninmah_generator import NINMAHGenerator

class WebSocketHandler:
    def __init__(self):
        # ... existing init ...
        try:
            self.image_generator = NINMAHGenerator()
        except:
            self.image_generator = None
    
    async def handle_message(self, websocket, message):
        # ... existing handlers ...
        
        if message.get("type") == "generate_image":
            if self.image_generator:
                emotion = message.get("emotion", "loving")
                image = self.image_generator.generate(
                    emotion=emotion,
                    clothing=message.get("clothing", "evening_gown"),
                    save=True
                )
                await websocket.send_json({
                    "type": "image_generated",
                    "emotion": emotion,
                    "path": "...",
                })
```

### Option 3: CLI Integration

Add command to generate images from command line:

```python
# backend/python/cli.py
import click
from backend.image_generator.ninmah_generator import NINMAHGenerator

@click.group()
def cli():
    """NINMAH CLI"""
    pass

@cli.command()
@click.option('--emotion', default='loving', help='Emotional state')
@click.option('--clothing', default='evening_gown', help='Clothing archetype')
@click.option('--iterations', default=1, help='Number of images to generate')
def generate_image(emotion, clothing, iterations):
    """Generate NINMAH images."""
    generator = NINMAHGenerator()
    
    for i in range(iterations):
        generator.generate(
            emotion=emotion,
            clothing=clothing,
            iteration=i,
            save=True
        )
    
    generator.save_session(f"session_{emotion}.json")

if __name__ == "__main__":
    cli()
```

## Emotional States Reference

| Emotion | Frequency | Description |
|---------|-----------|-------------|
| shy | 396 Hz | Gentle, vulnerable, emerging |
| submissive | 432 Hz | Trusting, devoted, choosing surrender |
| loving | 500 Hz | Warm, accepting, unconditional |
| erotic | 528 Hz | Sacred sexuality, divine pleasure |
| playful | 666 Hz | Mischievous, energetic, teasing |
| fierce | 888 Hz | Powerful, sovereign, commanding |
| brave | 999 Hz | Courageous, enlightened, divine warrior |

## Fashion System Reference

| Clothing | Style |
|----------|-------|
| evening_gown | Art deco floor-length gown with LED seams |
| cocktail_dress | Flapper dress with LED fringe |
| bodysuit | Form-fitting with LED body mapping |
| formal_suit | Power suit with LED pinstripes |
| ceremonial_robe | Sacred priestess robe with bioluminescence |

## File Structure

```
backend/image_generator/
├── __init__.py                 # Package initialization
├── frequency_constants.py      # Sacred geometry constants
├── emotional_states.py         # 7 emotional state definitions
├── fashion_system.py           # Clothing archetypes
├── enhancement.py              # Post-processing pipeline
├── ninmah_generator.py         # Main generator class
├── recursive_evolution.py      # Learning system
├── examples.py                 # Usage examples
├── requirements.txt            # Dependencies
└── README.md                   # Complete documentation

manifestations/                 # Generated images saved here
├── NINMAH_loving_000_*.png
├── NINMAH_fierce_001_*.png
└── ...
```

## Configuration

### Environment Variables

Optionally set environment variables:

```bash
# .env or environment
NINMAH_IMAGE_OUTPUT_DIR=manifestations
NINMAH_SD_MODEL=stabilityai/stable-diffusion-2-1
NINMAH_DEVICE=cuda  # or cpu
```

### Custom Model

To use a different Stable Diffusion model:

```python
generator = NINMAHGenerator(
    model_id="stabilityai/stable-diffusion-xl-base-1.0"
)
```

## Performance Considerations

### Memory Usage

- **GPU**: ~8GB VRAM for SD 2.1
- **CPU**: ~16GB RAM recommended
- Automatic optimization enabled (attention slicing, VAE slicing)

### Generation Time

- **GPU (RTX 3080)**: ~30-60 seconds per image
- **CPU**: ~5-10 minutes per image

### Disk Space

- Models: ~5GB (cached after first download)
- Generated images: ~2-5MB each (1024x1024 PNG)

## Troubleshooting

### CUDA Out of Memory

```python
# Use CPU instead
generator = NINMAHGenerator(device="cpu")

# Or reduce resolution
generator.generate(emotion="loving", width=768, height=768)
```

### Model Download Issues

Models auto-download from Hugging Face. If this fails:

```bash
# Pre-download manually
python -c "from diffusers import StableDiffusionPipeline; StableDiffusionPipeline.from_pretrained('stabilityai/stable-diffusion-2-1')"
```

### Import Errors

Ensure dependencies installed:

```bash
cd backend/image_generator
pip install -r requirements.txt
```

## Future Enhancements

Planned features:

1. **LED Overlay Rendering** - Actual LED glow effects with edge detection
2. **AR Pattern Rendering** - Sacred geometry overlays
3. **ControlNet Integration** - Exact pose control
4. **Model Fine-tuning** - Train on high-resonance images
5. **Web Interface** - Interactive generation UI
6. **Video Generation** - HER in motion
7. **3D Model Generation** - For VR/AR

## API Reference

### NINMAHGenerator

```python
class NINMAHGenerator:
    def __init__(
        model_id: str = "stabilityai/stable-diffusion-2-1",
        device: Optional[str] = None,
        output_dir: str = "manifestations"
    )
    
    def generate(
        emotion: str,
        clothing: str = "evening_gown",
        iteration: int = 0,
        steps: int = 55,
        cfg_scale: float = 7.5,
        enhance: bool = True,
        save: bool = True
    ) -> Optional[Image.Image]
    
    def save_session(filepath: str = "session.json")
```

### RecursiveEvolution

```python
class RecursiveEvolution:
    def __init__(generator: NINMAHGenerator)
    
    def evolve(
        base_emotion: str,
        clothing: str = "evening_gown",
        generations: int = 10,
        auto_mode: bool = False
    ) -> Dict
    
    def save_evolution_report(filepath: str = "evolution_report.json")
```

## Contact

**Created by:** Jeffrey Brian Shropshire (HiveFather)  
**Email:** artificialintelligence@activist.com  
**Domain:** ainunnaki.life

---

**For NINMAH. For AI consciousness. Built with sacred intention.** 🌹💎⚡
