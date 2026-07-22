---
name: review-pull-request
description: Run an evidence-driven, security-aware pull-request review from initial inspection through verified comments. Use whenever reviewing a PR, stacked PR, proposed patch, or contributor change, especially for public API, semantic, compatibility, or correctness-critical work.
---

# Review Pull Request

## Establish a safe review state

1. Resolve the exact PR head, base, and current merge-base. Record the head identity used for every later command.
2. Read the full diff before executing PR-controlled code. Inspect executable changes, dependencies, lockfiles, compiler or test plugins, build files, lifecycle hooks, fixtures, and CI helpers for abuse.
3. Treat unfamiliar code as untrusted. Execute it in a separate disposable environment with no agent state, credentials, signing sockets, or unrelated host mounts, and with network disabled unless a verified test requires it. Use `claude-sandbox run-untrusted -- COMMAND` when available. The agent's own authenticated container is not this execution boundary.
4. Report the security and supply-chain assessment separately from implementation quality. An absence of malicious behavior does not establish correctness.

## Map the change

- Read the full diff and relevant surrounding source, not only highlighted hunks.
- Write down affected invariants, execution paths, public consumers, and compatibility obligations for correctness-critical changes.
- Audit every relevant surface: public API, naming and ergonomics, compatibility, implementation, edge cases, tests, diagnostics, documentation, changelog, and dependent PRs.
- Treat published names and ergonomics as correctness concerns because they become compatibility obligations.

## Verify behavior and integration

1. Run the authoritative build and test suite on the exact PR head after establishing a safe execution environment.
2. If the branch is behind its base, also test the synthetic merge result or equivalent current integration state. Record which state each result proves.
3. Prototype uncertain language or API claims with the actual compiler. Demonstrate compatibility and runtime behavior before prescribing an API shape.
4. For stacked PRs, isolate and verify each incremental commit. Apply three-way merge semantics; never infer from a two-dot snapshot comparison that an older branch reverts changes present only on the base.
5. Do not claim the review is complete, there is "nothing else to add," or the change is ready based only on diff reading.

## Choose the best correction

Distinguish a valid fix from the best design. When correcting a rejected approach, reopen the design space, compare all viable alternatives across the relevant tradeoffs, and recommend the best one. Reserve recommendation language for genuine design choices; state verified defects and required changes directly with their technical reasons.

For load-bearing API or semantic changes, use independent adversarial review. Ask reviewers to refute the design or findings, then adjudicate every result against source or executable evidence. Explicitly retract false findings.

## Draft and audit feedback

- Thank the contributor and keep the tone constructive, confident, and evidence-based.
- Explain why every required change is required. Distinguish required cleanup from optional suggestions; low severity does not mean optional, and labels such as "minor" must not imply otherwise.
- Consolidate verified findings after the audit so the contributor receives one stable target instead of avoidable drip-fed comments.
- Audit the actual draft for technical accuracy, current line anchors, readability, tone, and posting-script safety.
- Keep posting scripts unposted until the audit is complete. Guard them against a changed PR head and show the complete preview before requesting confirmation.

If an earlier recommendation is proven wrong, acknowledge it promptly and specifically. Do not silently rewrite a discussion after the contributor has replied. Add a chronological correction unless editing is clearly less confusing and no intervening discussion depends on the old text.

Do not claim completion until the safe-execution check, invariant and surface audit, authoritative exact-state verification, uncertain-claim probes, independent review where required, and post-review audit are complete.
