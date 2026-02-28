# Study Task Theming Ideas

The goal: create a cohesive, engaging aesthetic across study tasks without making the theming so aggressive that it distracts from the actual programming.

## Core Vibe: "Night Garden" / Lunar Farming Sim

Inspired by: Stardew Valley meets utopian scholastic encyclopedia

**Key elements:**
- Plants that grow at night / by moonlight
- Celestial calendar affecting growth/care
- Botanical field guide aesthetic
- Emoji-friendly (plant, moon, star symbols)

**Tone:** Whimsical but practical. Not overtly magical — more "old farmer's almanac" than "fantasy RPG."

---

## Theming the Tutorial

Already in progress in `probes-tutorial-draft.md`:
- MoonPhase type affecting watering calculations
- Plants: Moonleaf Fern 🌿, Starbloom Orchid 🌸, Thornveil Cactus 🌵, Dewcup Lily 🪷
- Greenhouse Planner with beds and schedules

---

## Re-theming EmojiPaint → "Crop Plotter"

The emojipaint task uses a grid where you paint emojis. This could become a **crop planting grid**.

### Current Actions
- `PaintCell(Row, Col)` → Plant a crop at position
- `ClearCell(Row, Col)` → Remove crop
- `PaintRow(Row)` → Plant a row of crops
- `PaintCol(Col)` → Plant a column
- `ClearGrid` → Clear the field

### Themed Version

```hazel
type Crop = String in  # emoji #
type Field = [[Crop]] in

type Action =
  + PlantCrop(Row, Col)    # Plant current seed at position #
  + HarvestCrop(Row, Col)  # Remove crop #
  + PlantRow(Row)          # Plant entire row #
  + PlantCol(Col)          # Plant entire column #
  + ClearField             # Clear for new season #
  + SelectSeed(Int)        # Choose seed from inventory #
in

let init: Model = (
  field = [
    ["", "", ""],
    ["", "", ""],
    ["", "", ""]
  ],
  currentSeed = "🌱",
  seedInventory = ["🌱", "🌻", "🥕", "🌽", "🍅"]
) in
```

### Bug Scenarios (Crop Plotter)

The existing setCell bug (using `i` instead of `j` for column check) becomes:
> "The crop planter is putting seeds in the wrong position. A test shows that planting at (1, 2) doesn't produce the expected field."

The nested loop is still there — it's iterating over field rows and cells.

---

## Re-theming Tamagotchi → "Moonbloom Nursery"

The Tamagotchi is already somewhat themed as a "Haunted Toaster." We could make it more botanical:

### "Moonbloom" - A Night-Blooming Plant Pet

Instead of a toaster with hunger/happiness/energy/health, it's a magical plant with:
- `hydration` (like hunger)
- `vitality` (like happiness)
- `moonlight` (like energy, affected by lunar phase)
- `rootHealth` (like health)

### Actions
- `Water` → Increases hydration
- `Sing` (plants like music!) → Increases vitality
- `MoonBathe` → Restores moonlight (only effective during certain phases)
- `Fertilize` → Improves root health
- `TimeTick` → Stats decay based on conditions

### The Bug

Same structure: the decay function uses the wrong stat to determine another stat's decay rate.

```hazel
let decayStats: Stats -> Stats =
  fun s ->
    let vitalityDecay =
      if s.vitality < 30 then 8  # BUG: should be s.hydration < 30 #
      else 3
    in
    ...
```

---

## Other Programs as "Subsystems"

The programs don't need to directly connect, but could feel like different parts of the same world:

| Program | Themed As | Role in "Night Garden" |
|---------|-----------|------------------------|
| Emojipaint → Crop Plotter | Field planning tool | Layout which crops go where |
| Tamagotchi → Moonbloom | Plant pet simulator | Care for individual special plants |
| Calculator → Harvest Calculator | Yield estimator | Calculate expected harvests |
| Game of Life → Spreading Vines | Growth simulator | Model how plants spread |

---

## Light Theming for Logic Tasks

For pure logic / program-writing tasks (like clamp, basepoint, running-sum), we can add extremely light theming:

### Option 1: Just rename variables
```hazel
# Instead of:
let clamp = fun (x, lo, hi) -> ...

# Use:
let clampMoisture = fun (level, minSafe, maxSafe) -> ...
```

### Option 2: Themed test cases
```hazel
test clamp(150, 0, 100) == 100 end  # "Soil moisture can't exceed 100%" #
test clamp(-5, 0, 100) == 0 end     # "Moisture can't go negative" #
```

### Option 3: Leave them abstract
For some tasks, abstract is fine. Not everything needs theming.

---

## Emoji Palette

Consistent emojis across tasks:

**Plants:**
- 🌱 Seedling
- 🌿 Fern / herb
- 🌻 Sunflower (ironic for night garden, or maybe it's a moonflower)
- 🌸 Blossom
- 🌺 Hibiscus
- 🪷 Lotus
- 🌵 Cactus
- 🥕🌽🍅 Vegetables

**Celestial:**
- 🌙 Crescent moon
- 🌑🌒🌓🌔🌕🌖🌗🌘 Moon phases
- ⭐ Star
- ✨ Sparkles

**Tools/Environment:**
- 💧 Water drop
- 🪴 Potted plant
- 🏡 House/greenhouse
- 🌡️ Thermometer

---

## Implementation Priority

1. **Tutorial** - Already themed (Greenhouse Planner) ✓
2. **EmojiPaint → Crop Plotter** - Easy re-theme, preserves all logic
3. **Tamagotchi → Moonbloom** - Moderate effort, needs stat renaming
4. **Writing tasks** - Light touch (variable names, comments)
5. **Game of Life** - Only if we keep it; could become "Spreading Vines"

---

## Open Questions

- How much theming is too much? Don't want to distract from the actual task.
- Should all tasks share the theme, or is variety okay?
- Does the theme help or hurt participant engagement?
- Should we run a quick pilot to see if theming is distracting?
