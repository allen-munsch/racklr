# Autonomous vigils for racklr

racklr is driven by two dirge vigils: a timer that picks up the next issue and fixes it, and a filesystem watcher that verifies and commits uncommitted work. This file documents the loop and how to write vigils in general, so adding or editing one does not require reading the dirge source.

## The loop

- `racklr-drive` (toll, every 15 minutes) picks one actionable issue from the board, implements the smallest fix, recompiles with `raco setup racklr`, runs the targeted tests and the full harness, commits on the feature branch when green, and updates the board. It never pushes, never commits to main, and never runs `git add -A`.
- `racklr-verify` (watcher on `racklr/`) is a commit safety net. Its rite gate wakes the agent only when the working tree has uncommitted changes; the turn re-verifies and commits if the harness is green.

Both live in `.dirge/vigils/`. Start the keeper from the racklr root:

```bash
dirge --vigil --auto-confirm
```

Vigils load from `.dirge/vigils/` at startup. A running keeper picks up new files only after a restart, or you register one live with `/vigil add`. Check state with `/vigil status`, pause with `/vigil pause <name>`.

## What a vigil is

A vigil is a wake-and-sleep loop. A trigger queues events into a bounded per-vigil channel. A reaper drains that channel on a cadence, runs an optional rite gate, and on a pass runs one agent turn with the prompt, then the keeper returns to sleep. An optional procession chains to the next vigil.

## Writing a vigil

One JSON file in `.dirge/vigils/`, named for the vigil. The fields:

- `name` — unique id, used by `/vigil` commands and `vigil/emit`
- `trigger` — what produces events: toll, watcher, or harbinger
- `reap_interval_secs` — how often the reaper drains the queue (default 30)
- `rite` — optional gate; see the rite rule below
- `prompt` — the template sent to the agent on wake
- `procession` — optional name of the next vigil to fire after this one

Triggers:

- toll: `{ "type": "toll", "interval_secs": 300 }` fires on a fixed timer
- watcher: `{ "type": "watcher", "path": "src" }` fires on filesystem changes under the path, relative to the dirge working directory
- harbinger: `{ "type": "harbinger", "address": "127.0.0.1:9090" }` fires on TCP connections; payloads arrive as `{harbinger_data}`

## The rite rule

This is the part people get backwards. A rite is a gate that must exit 0 to wake the agent; a non-zero exit skips the observance. Write it inverted: exit 0 means "there is work, wake me".

To wake only when the harness has failures, the rite is a command that exits 0 when a FAIL line exists, not the harness itself (which exits non-zero on failure):

```json
{
  "rite": {
    "cmd": "cd racklr-test && racket examples-roundtrip-test.rkt 2>&1 | grep -q '^FAIL'"
  }
}
```

To wake only when the tree has uncommitted work:

```json
{
  "rite": {
    "cmd": "test -n \"$(git status --porcelain)\""
  }
}
```

On a passing rite, stdout becomes `{rite_output}`; on a failing rite nothing is captured.

## Template variables

`{name}`, `{files}`, `{events}`, `{event_count}`, `{timestamp}`, `{rite_output}`, `{rite_exit_code}`, `{harbinger_data}`, plus any string field merged into the event context by a plugin.

## Minimal example

```json
{
  "name": "hello",
  "trigger": { "type": "toll", "interval_secs": 60 },
  "prompt": "Tick {name}. Rite said: {rite_output}"
}
```

## Full reference

The dirge source tree documents vigils in `docs/vigils/`: `README.md` (overview), `usage.md` (config reference), `use-cases.md` (worked examples), `vs-loop-mcp.md` (when to choose vigil over loop or MCP).
