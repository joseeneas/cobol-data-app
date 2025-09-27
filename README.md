# COBOL Data App

Quick start

- Build: make build
- Home menu: bin/cobol-data-app
- Themed + indexed list: THEME=BLUE STORAGE=INDEXED bin/cobol-data-app LIST
- Import sample data (CSV): IMPORT_FILE=data/sample-import.csv bin/cobol-data-app IMPORT

A small terminal (SCREEN SECTION) COBOL application demonstrating:
- Interactive data entry with validation and status feedback
- Batch import with per-row validation and summary
- Search/Edit flow (lookup by ID or partial name)
- List mode with filters and pagination
- Audit logging on save/update
- Configurable themes (colors) and pluggable storage backends (CSV or Indexed)

The app uses GnuCOBOL (cobc) and is known to work on macOS.

---

## Prerequisites

- GnuCOBOL and GMP (math library used by generated C)
  - brew install gnu-cobol gmp

If you see a compile error like `fatal error: 'gmp.h' file not found`, install gmp as above. The Makefile uses pkg-config to locate headers and libs.

---

## Build

- Build the executable:

  make build

- Clean build artifacts:

  make clean

---

## Run (entry points)

You can run the unified Home menu or jump directly into a specific mode.

- Home menu (default):

  bin/cobol-data-app

  or explicitly:

  bin/cobol-data-app HOME

- Data entry (Add new):

  bin/cobol-data-app SCREEN

- Search/Edit existing record:

  bin/cobol-data-app EDIT

- List with filters and pagination:

  bin/cobol-data-app LIST

- Batch import (from CSV file):

  bin/cobol-data-app IMPORT

---

## Environment configuration

- Theme (colors):
  - THEME can be DEFAULT (empty), BLUE, or GREEN
  - Examples:

        THEME=BLUE bin/cobol-data-app
        THEME=GREEN bin/cobol-data-app LIST

- Storage backend:
  - STORAGE can be CSV (default) or INDEXED
  - CSV: uses data/customers.csv (LINE SEQUENTIAL)
  - INDEXED: uses data/customers.idx (ORGANIZATION IS INDEXED, ACCESS MODE DYNAMIC)
  - Examples:

        STORAGE=INDEXED bin/cobol-data-app HOME
        STORAGE=CSV     bin/cobol-data-app EDIT

- Import file path:
  - IMPORT mode reads the path from IMPORT_FILE (if set), otherwise defaults to data/import.csv
  - Example:

        IMPORT_FILE=/path/to/file.csv bin/cobol-data-app IMPORT

---

## Modes and features

- Home menu
  - Choose: 1) Add 2) Edit 3) List 4) Import X) Exit
  - Displays current Storage and Theme in the header

- Add (SCREEN)
  - Fields: ID (digits, >0), Name (no commas or quotes), Email (contains @ and . and . after @), Age (1-120)
  - Field-level error highlighting (the specific field turns red on validation errors)
  - Keys: Enter/F5 = Save, F1 = Help, ESC/F10 = Exit
  - Duplicate ID check blocks saving (both CSV and Indexed storage)
  - Audit log: data/audit.log appends entries on successful saves

- Search/Edit (EDIT)
  - Search by exact ID or case-insensitive "Name contains"
  - Loads record into the same entry form; on save:
    - CSV: rewrites the CSV via a temp file
    - Indexed: REWRITEs the record by key
  - Duplicate-check logic allows keeping the same ID; changing ID triggers duplicate validation
  - Audit log entry for "update"

- List (LIST)
  - Filters:
    - Name contains (case-insensitive)
    - Email domain (part after @)
    - Age range (min/max)
  - Pagination: 10 rows per page; keys N (next), P (prev), F (change filter), ESC/F10 (exit)
  - Displays current Storage and Theme in the header

- Import (IMPORT)
  - Reads CSV rows (id,name,email,age). First line may be a header (skipped if it starts with ID)
  - For each valid row:
    - If ID exists: update
    - Else: add
  - Logs a final summary: added=X updated=Y failed=Z and appends audit entries for saves/updates

---

## Storage backends

- CSV (default)
  - File: data/customers.csv (created if missing, header written once)
  - Appends for write; reads when needed for duplicate checks, search, and list

- Indexed (optional)
  - File: data/customers.idx (created if missing)
  - Layout:
    - id:     PIC X(9)
    - name:   PIC X(50)
    - email:  PIC X(60)
    - age:    PIC 9(3)
  - Exact ID reads are by key; name "contains" and list filtering use a sequential scan over the index

Switch storage with STORAGE=CSV or STORAGE=INDEXED. The UI, validation, and logging behave the same across backends.

---

## Keys (applies to screens)

- Enter or F5: Save
- F1: Help
- ESC or F10: Exit/Cancel current screen

Note: Function key codes can differ by terminal. The program shows unknown key codes in the Status line if unmapped.

---

## Files and directories

- src/main.cob         # COBOL program (free source format)
- bin/cobol-data-app   # Built executable
- data/customers.csv   # CSV storage (default)
- data/customers.idx   # Indexed storage (when STORAGE=INDEXED)
- data/audit.log       # Simple audit log (append-only)
- Makefile             # Uses pkg-config to include/link gmp
- README.md

---

## Troubleshooting

- Build error: `fatal error: 'gmp.h' file not found`
  - brew install gmp
  - If still failing, try: brew reinstall gnu-cobol

- Function keys not working as expected
  - Try the alternatives: Enter for Save, ESC for Exit, '?' may open help depending on terminal
  - Let us know the key code shown in Status to add a mapping

- CSV contains duplicates already
  - The app will block new duplicates on save and will update existing IDs via EDIT or IMPORT

---

## Project Structure

- src/main.cob
- data/
  - customers.csv (created at runtime)
  - customers.idx (created when STORAGE=INDEXED)
  - audit.log (created on first save/update)
- bin/
- Makefile
- README.md
- docs/ (optional)

---

## License

This is a learning/demo project; use at your own discretion.
