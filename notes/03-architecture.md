# 03 · Architecture

## Delivery to an agent

Options, not decisions:

1. **CLI** (`racket-steer check file.rkt --json`): simplest; works with any agent that can run shell commands.
2. **MCP server** exposing each tool with a JSON schema: the natural fit for Claude-style agents; lets us describe
   arguments precisely and return structured content. Implement in Racket (JSON-RPC over stdio) or as a thin wrapper
   that shells out to the CLI.
3. **Hooks** (agent-harness hooks run after each edit/write): run A1/A2 automatically on any `.rkt` file the agent
   touches and inject the result, so the model gets feedback without choosing to ask. Probably the highest leverage
   because models under-use optional tools. [hyp]
4. **Library API** (`(require steering)`) for building custom agent loops in Racket.

Recommendation: core library + CLI first, MCP and hooks as thin layers on top.

## Shared feedback protocol

Every tool returns:

```
{ "tool": "binding-check", "ok": false, "elapsed_ms": 412,
  "findings": [ { "severity": "error", "kind": "unbound-identifier",
                  "file": "a.rkt", "line": 12, "col": 8, "span": 6,
                  "message": "`string-contains` is not bound; did you mean `string-contains?`",
                  "suggestions": [ {"replace": "string-contains?", "confidence": "high"} ],
                  "doc": "racket/string: (string-contains? s contained) → boolean?" } ],
  "truncated": false, "next": ["run raco test", ...] }
```

Rules: stable field names; findings ordered by likely root cause, not by position; cap the count (default 5) and say
if truncated; always include a suggestion when one exists; never dump more than ~2 KB unless asked; no ANSI colour.

## Isolation and safety

- Expansion and execution run inside a **sandbox evaluator** with time/memory limits, no network, filesystem
  restricted to the project (read) and a temp dir (write).
- Sandbox is a guard against accidents. For untrusted code add OS-level isolation (container or macOS sandbox
  profile). Never rely on `racket/sandbox` alone against a hostile program.
- Tools are read-only by default; edit-applying tools (`apply-edit`) write only inside the given project root and
  produce a diff first.
- Determinism: fixed random seeds reported in output; no clock/network in checks unless declared.

## Performance budget

Racket startup is 250-450 ms with racket/base; loading the expander with big libraries is more. To keep feedback
interactive: keep a **warm server process** (one long-lived Racket with libraries preloaded, requests over a socket)
instead of spawning per call; cache doc indexes on disk; compile our own code with `raco make`. Target: A1 < 50 ms
warm, A2 < 1 s, A3 depends on tests.

## Packaging

- Racket package `steering` (library) + `steering-cli`; single-file distributable via `raco exe` / `raco distribute`
  for machines without Racket (tens of MB).
- Versioned tool schema so agent prompts can pin a version.

## Related to skiaracket

No dependency. The only overlap is the private-fork/patch discipline and the measurement discipline (interleaved
runs, medians, controls); reuse that method for note 04.
