# Growth Stages Plotter

A moonlit field simulation where crops advance through growth stages when watered. Part of the Night Garden debugging study series.

## Concept

You tend a magical garden where crops grow through stages under moonlight. Watering advances crops through their lifecycle:

**Growth Lifecycle:**
- **Seed** - Just planted, dormant
- **Sprout** - First leaves emerge
- **Growing** - Building strength
- **Mature** - Fully developed
- **ReadyToHarvest** - Glowing with lunar energy, ready to pick

Each watering advances all planted crops by one stage. Only crops that have reached `ReadyToHarvest` can be harvested.

### Crops

- **Moonmelon** - Silvery melon that ripens under starlight
- **Nightshade** - Purple bloom with magical properties
- **Starwheat** - Golden grain that sparkles at dusk
- **Glowpumpkin** - Orange and luminescent

### Actions

- `PlantCrop(row, col)` - Plant the currently selected seed
- `HarvestCrop(row, col)` - Harvest a crop (only works on ReadyToHarvest)
- `WaterField` - Advance all planted crops by one growth stage
- `PassDay` - Increment the day counter
- `SelectSeed(crop)` - Choose which crop type to plant next

## The Bug

### Location
In the `advanceStage` function, around line 53-60 in the bug files.

### What's Wrong
The pattern match for the `Growing` stage returns `Growing` instead of `Mature`:

```hazel
let advanceStage = fun stage ->
  case stage
  | Seed => Sprout
  | Sprout => Growing
  | Growing => Growing   # BUG: should be Mature
  | Mature => ReadyToHarvest
  | ReadyToHarvest => ReadyToHarvest
  end
in
```

### Why It's Wrong
The buggy code causes crops to get stuck at the `Growing` stage forever. They can never advance to `Mature` or `ReadyToHarvest`, which means they can never be harvested.

### Effect
- Crops advance normally from Seed to Sprout to Growing
- Crops get stuck at Growing stage no matter how many times you water
- No crops ever reach ReadyToHarvest
- Harvesting always fails since no crops are harvestable

### Failing Tests
1. `"advanceStage: Growing becomes Mature"` - Direct test of the function
2. `"multiple waterings progress through stages"` - Expects ReadyToHarvest after 4 waterings
3. `"can harvest ReadyToHarvest crops"` - Can't harvest because nothing reaches ReadyToHarvest
4. `"harvesting clears the cell"` - Same issue

## Files

- `growth-plotter.hz` - Working version with all tests passing
- `growth-plotter-bug.hz` - Buggy version with minimal comments
- `growth-plotter-bug-scaffold.hz` - Buggy version with helpful comments

## Why Probes Help

This bug is subtle because:
1. The code type-checks correctly
2. Most of the growth lifecycle works (Seed->Sprout->Growing)
3. The bug only manifests after the 3rd watering
4. The pattern match looks reasonable at a glance

Using probes, a debugger can:

1. **Track stage progression**: Add `^^probe(advanceStage(stage))` to see what stage is returned
2. **Watch the watering effect**: Add `^^probe(getStage(cell))` after each watering to see stages accumulate
3. **Spot the stuck stage**: After multiple waterings, probe output will show crops stuck at Growing

### Probe Strategy

Place a probe on the `advanceStage` function result:

```hazel
let advanceStage = fun stage ->
  ^^probe(case stage
    | Seed => Sprout
    | Sprout => Growing
    | Growing => Growing   # BUG
    | Mature => ReadyToHarvest
    | ReadyToHarvest => ReadyToHarvest
    end)
in
```

Running `./hazel probe --many` on the demo at the end will show:
- The function being called multiple times per watering (once per crop)
- The progression: Sprout, Growing, Growing, Growing, Growing...
- The telltale pattern of `Growing` appearing repeatedly when `Mature` and `ReadyToHarvest` should appear

## Running Tests

```bash
# Check for type errors
./hazel analyze growth-plotter-bug.hz

# Run tests and see failures
./hazel test growth-plotter-bug.hz

# Debug with probes
./hazel probe growth-plotter-bug.hz
./hazel probe --many growth-plotter-bug.hz
```

## Test Summary

The task includes 18 tests covering:
- Basic planting (4 tests)
- Growth stage progression (5 tests)
- Watering mechanics (3 tests)
- Harvesting behavior (3 tests)
- Day counting (1 test)
- Multiple crop management (2 tests)
