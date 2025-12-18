# Secret Santa v2

Secret Santa web app for running anonymous gift exchanges. Admins manage a workshop (game instance), invite players, capture wishlists, and randomly pair givers with recipients so each participant sees only their assigned receiver's list.

## Current state
The repository currently contains the core domain library and unit tests. It models workshops, players, wishlists, and mnemonic-style workshop IDs; pairing logic and service/API layers are planned to follow.

## Project layout
- `src/backend/secret-santa.sln` — solution including the domain library and tests
- `src/backend/SecretSanta.Domain` — domain types for workshops, players, wishlists, and Pneumonic IDs
- `src/backend/tests/SecretSanta.Tests.Domain` — Expecto tests covering the domain behaviors

## Domain highlights
- **Workshop**: holds a mnemonic ID, name, spending cap, player list, and eventual giver/receiver pairs.
- **Player**: identified by nickname; supports tags (deduplicated) and wishlist management.
- **Wishlist items**: validated HTTP/HTTPS URLs to keep entries shareable.
- **Pneumonic IDs**: human-friendly, winter-themed slugs (e.g., `frosty-elf`) with basic validation.

## Getting started
1) Install a .NET SDK that supports `net10.0` (or retarget the projects to your installed SDK).  
2) From `src/backend`, run tests:  
   ```bash
   dotnet test
   ```  
3) Open `secret-santa.sln` in your editor/IDE to explore or extend the domain.

## License
MIT — see `LICENSE`.
