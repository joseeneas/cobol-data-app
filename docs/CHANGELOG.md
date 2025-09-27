# Changelog

All notable changes to this project will be documented in this file.

## 2025-09-27

- Add Home menu with choices for Add, Edit, List, Import (ESC/F10 to exit)
- Add key bindings: Enter/F5 = Save, F1 = Help, ESC/F10 = Exit
- Field-level error highlighting for invalid inputs
- Duplicate ID check on save (CSV and Indexed storage)
- Add audit logging to data/audit.log for save/update operations
- Implement Search/Edit screen:
  - Lookup by ID or case-insensitive Name contains
  - Save updates (CSV rewrite via temp; Indexed REWRITE)
- Implement List mode with filters and pagination:
  - Filters for Name contains, Email domain, Age range
  - N/P navigation, F to re-open filters
- Implement Batch Import mode (IMPORT):
  - Validate each row, add or update by ID, summary printed
- Configurable theme via THEME=DEFAULT|BLUE|GREEN
- Pluggable storage backend via STORAGE=CSV|INDEXED
  - CSV: data/customers.csv
  - Indexed: data/customers.idx
- Display current Storage and Theme on Home and List screens
- Build improvements: use pkg-config to include/link gmp
