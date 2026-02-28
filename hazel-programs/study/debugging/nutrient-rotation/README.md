# Night Garden Nutrient Tracker

A soil nutrient management system for debugging practice with Live Probes.

## Concept

The Night Garden is a mystical sustainable farming system where crop rotation is key to healthy soil. Different crops deplete or restore different nutrients (N/P/K):

- **Nightbeans**: Nitrogen fixer - restores N when harvested
- **Mooncorn**: Heavy feeder - depletes N significantly
- **Startomatoes**: Balanced - moderate use of all nutrients
- **Twilight Clover**: Cover crop - restores N when turned under (nitrogen fixer)

The core insight is that nitrogen-fixing plants (legumes like beans and clover) have symbiotic bacteria in their roots that convert atmospheric nitrogen into soil nitrogen. When these plants are harvested or turned under, they restore nitrogen to depleted soil.

## The Bug

In the buggy versions, when Twilight Clover is harvested, it incorrectly restores **Phosphorus** instead of **Nitrogen**.

### Buggy Code (line ~74)
```
| TwilightClover =>
    (n = s.n, p = clamp(s.p + 20), k = clamp(s.k - 5))
```

### Correct Code
```
| TwilightClover =>
    (n = clamp(s.n + 20), p = clamp(s.p - 5), k = clamp(s.k - 5))
```

## Symptoms

The failing test is "mooncorn then clover rotation restores nitrogen":

1. Start with balanced soil (N=60)
2. Plant and harvest Mooncorn (depletes N to ~25)
3. Plant and harvest Twilight Clover (should restore N to ~45)
4. **Expected**: N is restored above 40
5. **Actual**: N stays at 25, but P mysteriously increases to 62

This manifests as a crop rotation that should work (heavy feeder followed by nitrogen fixer) failing to restore soil health.

## Why Probes Help

With Live Probes, you can:

1. **Probe the harvestEffect function** to see what nutrients change for each crop:
   ```
   ^^probe(harvestEffect(s, TwilightClover))
   ```
   This reveals that N stays the same while P increases.

2. **Probe the rotation sequence** to track soil state through the season:
   ```
   let afterMooncorn = ^^probe(farm(start, [Plant(Mooncorn), Harvest])) in
   let afterClover = ^^probe(farm(afterMooncorn, [Plant(TwilightClover), Harvest])) in
   ```
   This shows N not recovering after the clover harvest.

3. **Compare with Nightbeans** (the other nitrogen fixer) to see the expected pattern:
   ```
   ^^probe(harvestEffect(s, Nightbeans))
   ```
   This correctly shows N increasing, giving a reference for what Clover should do.

## Files

- `nutrient-rotation.hz` - Working version (all tests pass)
- `nutrient-rotation-bug.hz` - Bug planted, minimal comments
- `nutrient-rotation-bug-scaffold.hz` - Bug planted, with explanatory comments

## Domain Knowledge

Real nitrogen-fixing plants include:
- Legumes (beans, peas, clover, alfalfa)
- Some non-legumes with bacterial symbionts

These plants work with Rhizobium bacteria in root nodules to convert N2 from air into ammonia (NH3), which enriches the soil. This is why traditional farming rotates nitrogen-depleting crops (like corn) with nitrogen-fixing cover crops (like clover).
