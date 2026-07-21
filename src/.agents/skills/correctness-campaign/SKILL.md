---
name: correctness-campaign
description: Run a decision-first, adversarially reviewed correctness workflow for large or high-risk changes. Use for formal designs, compilers, ownership or concurrency work, semantic migrations, parsers, extract-and-replace refactors, protocol changes, or any change where a plausible local mistake could violate a load-bearing invariant.
---

# Correctness Campaign

Treat process as the quality floor. Strong models improve judgment, but durable decisions, independent attacks, executable gates, and post-change review preserve quality when later work is performed by weaker models.

## Allocate judgment deliberately

- Use the strongest available model for the irreducible design decision, invariant discovery, and adjudication of conflicting review findings.
- Record each settled decision and its reasoning in the working paper before handing execution to another model.
- Delegate mechanical folds, implementation steps, grep audits, and verification only after the judgment is durable.
- When weaker models perform review or implementation, narrow each brief, provide exact invariants and gates, and widen the independent review panel. Never compensate by asking one weak reviewer to bless a larger surface.
- Keep reviewers independent. Give them the artifact and attack surface, not another reviewer's conclusion.

## Establish the oracle

Before implementation:

1. Identify the trusted specification, implementation, corpus, or observable behavior.
2. Define the exact invariant and failure modes.
3. Choose evidence that can falsify the change: regression cases, differential outputs, sanitizers, fixed points, fuzzing, or performance budgets.
4. Record the baseline results and known exceptions.

Do not use the candidate implementation as its own oracle.

## Decide on separate paper

Write a working paper that:

- Makes decisions rather than presenting an option menu.
- Maps every affected execution path and authority boundary.
- States invariants, non-goals, compatibility stance, and consumer impact.
- Names suspected weak points and includes a numbered attack list.
- Defines a commit-sequenced fold with a gate after each step.
- Defines the terminal proof of completion.

Keep the load-bearing artifact unchanged until the paper survives review.

## Attack the paper

Run at least two independent adversarial reviews for core-invariant work:

- Soundness lens: construct counterexamples, invalid states, miscompiles, leaks, races, and boundary violations.
- Coherence lens: find contradictions, stale definitions, incomplete propagation, and consumer mismatches.

Tell reviewers to refute the paper, cite quoted content, and distinguish verified defects from hypotheses. Amend until the paper is one clean, decision-complete body.

## Implement through gates

1. Write the regression corpus red first where practical.
2. Apply the smallest coherent fold step.
3. Run its narrow gate in the foreground and wait for completion.
4. Compare the candidate with the trusted baseline.
5. Commit only a coherent green unit.
6. Repeat without weakening tests or silently changing the oracle.

Recheck repository state immediately before every writer dispatch and commit.

## Select domain gates

Use every gate relevant to the claim:

- Compiler or self-hosting: stage-to-stage fixed-point identity.
- Ownership or runtime safety: ASan, LSan, UBSan, and exactly-once resource accounting across every terminal path.
- Parser or grammar: malformed-input fuzzing with time, depth, iteration, and input-size bounds.
- Semantic unification: differential fuzzing with the richest observable comparison and an absolute-zero unexplained-disagreement gate across multiple seeds.
- Extraction or refactor: old-vs-new corpus comparison, consumer switchover tests, and byte identity where behavior must be unchanged.
- Concurrency or timing: instrument actual event order and state; include deterministic stress or schedule variation.
- Performance claim: measure at the earliest architecture gate and preserve a repeatable budget.
- Cross-target support: compile a real artifact for every claimed target.

Classify every disagreement. Do not hide a semantic bucket behind filtering, routing, retries, or a weakened comparison.

## Review the applied result

After the full suite is green, review the actual diff adversarially. Attack paper-to-code correspondence, invariant propagation, new edge cases, test adequacy, and defects introduced by review fixes. Fix findings and rerun the relevant narrow and full gates.

Do not declare completion from confidence, a green narrow suite, or a clean pre-change review. The post-change review is mandatory because it repeatedly finds real memory-safety and miscompile defects after apparently green folds.

## Finish durably

- Rewrite durable documentation as one current body.
- Record exact verification evidence and known exceptions.
- Produce a `session-handoff` if any work, decision, or risk remains in flight.
- Keep outward-facing actions behind fresh explicit consent.
