#!/usr/bin/env bash
set -euo pipefail
REPO_DIR="/Users/eneas/cobol-data-app"
APP="$REPO_DIR/bin/cobol-data-app"

# Build
make -C "$REPO_DIR" build

# Show LIST (CSV) before import
echo "== CSV LIST (before import) =="
"$APP" LIST || true

# Import sample data into CSV
echo "== CSV IMPORT sample-import.csv =="
IMPORT_FILE="$REPO_DIR/data/sample-import.csv" "$APP" IMPORT

# Show LIST (CSV) after import
echo "== CSV LIST (after import) =="
"$APP" LIST || true

# Import sample updates into CSV
echo "== CSV IMPORT sample-update.csv (updates existing IDs) =="
IMPORT_FILE="$REPO_DIR/data/sample-update.csv" "$APP" IMPORT || true

# Show LIST (CSV) after updates
echo "== CSV LIST (after updates) =="
"$APP" LIST || true

# Demonstrate INDEXED storage: import and list
echo "== INDEXED IMPORT sample-import.csv =="
STORAGE=INDEXED IMPORT_FILE="$REPO_DIR/data/sample-import.csv" "$APP" IMPORT

echo "== INDEXED LIST =="
STORAGE=INDEXED THEME=BLUE "$APP" LIST || true
