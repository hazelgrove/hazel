# Hazel Core Scripts

This folder contains utility scripts for maintaining the Hazel codebase.

## DocSlideMigration

Migrates doc slides to use proper indentation. Old slides have linebreaks but no indentation spaces after them. This script applies the Format action to add correct indentation.

### Why This Exists

The Format action (Cmd+S) adds proper indentation to code. Old doc slides were created before this feature existed, so they lack indentation. This migration adds indentation to make the slides display correctly.

### Idempotency

The migration is **safe to run multiple times**. Running Format on already-formatted content produces identical output. This means:
- You can re-run on slides that have already been migrated
- New slides can be migrated regardless of their current indentation state
- The migration won't corrupt existing proper indentation

### How to Run

#### Step 1: Build the project
```bash
make
```

#### Step 2: Run the migration tests to verify everything works
```bash
node _build/default/test/haz3ltest.bc.js test 'DocSlideMigration'
```

You should see all tests pass:
```
[OK] DocSlideMigration  0  BasicReference round-trip after migration
[OK] DocSlideMigration  1  Format is idempotent
[OK] DocSlideMigration  2  Generate ML file
[OK] DocSlideMigration  3  All web docs slides migrate successfully
[OK] DocSlideMigration  4  Output all web docs migrations
```

#### Step 3: Generate migrated ML files

To generate migrated content for all `src/web/init/docs/` slides:
```bash
node _build/default/test/haz3ltest.bc.js test 'DocSlideMigration' '4' 2>&1 | \
  grep -v "^\[" | grep -v "^qcheck" | grep -v "^Testing" | grep -v "^This run"
```

This outputs the migrated ML content with markers:
```
===FILE:src/web/init/docs/BasicReference.ml===
let out : string * Haz3lcore.PersistentSegment.t =
  ...
===END:src/web/init/docs/BasicReference.ml===
```

#### Step 4: Extract and write individual files

You can use a script to extract and write the files. Here's an example bash approach:

```bash
# Run the migration and save output
node _build/default/test/haz3ltest.bc.js test 'DocSlideMigration' '4' 2>&1 > /tmp/migration_output.txt

# Extract each file (example for BasicReference.ml)
sed -n '/===FILE:src\/web\/init\/docs\/BasicReference.ml===/,/===END:src\/web\/init\/docs\/BasicReference.ml===/p' \
  /tmp/migration_output.txt | \
  grep -v "^===" > src/web/init/docs/BasicReference.ml
```

Or process all files with a loop:

```bash
# Extract all files from migration output
for file in BasicReference Projectors ADTs Tuples Tables Polymorphism Cards Probes Livelits; do
  sed -n "/===FILE:src\/web\/init\/docs\/${file}.ml===/,/===END:src\/web\/init\/docs\/${file}.ml===/p" \
    /tmp/migration_output.txt | \
    grep -v "^===" > "src/web/init/docs/${file}.ml"
done
```

### Verifying the Migration

After writing the files, verify with:

```bash
# Rebuild
make

# Run the round-trip tests
node _build/default/test/haz3ltest.bc.js test 'DocSlides.ReparseBackuptext'
```

All tests should pass.

### Adding New Slides

If you add new slides that need migration:

1. Add the slide to `web_docs_slides` in `test/Test_DocSlideMigration.re`
2. Run the migration as described above
3. Verify with round-trip tests

### B2T2 Slides

The B2T2 slides (`src/b2t2/slides/`) can also be migrated using the same approach. To add them:

1. Import them in `Test_DocSlideMigration.re`
2. Add them to a `b2t2_slides` list similar to `web_docs_slides`
3. Create corresponding test cases

### Troubleshooting

**Test fails with "Failed to parse migrated backup_text"**
- The segment may have structural issues. Check the original backup_text manually.

**Build errors after migration**
- Check that the ML file escaping is correct. Strings should use `\n\` for line continuation.

**Editor shows wrong indentation**
- Rebuild completely: `make clean && make`
- Verify the ML file has the correct format by inspection
