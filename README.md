# JudgeRep

Arbitrator impartiality tracking smart contract built on the Stacks blockchain. JudgeRep records arbitration cases, measures ruling consistency, aggregates party satisfaction scores, and exposes a composite reputation score for each registered arbitrator.

---

## Features

- **Arbitrator Registry** — Any principal can self-register. The contract owner can forcefully deactivate misbehaving arbitrators.
- **Case Lifecycle** — Open, rule, close, and reopen arbitration cases with on-chain audit trails and off-chain document hashes.
- **Ruling Consistency Tracking** — Counts rulings by type (Party A, Party B, Split, Dismissed) and derives an impartiality score from the distribution.
- **Party Satisfaction Scoring** — Each party submits one score (1–100) per closed case. Scores aggregate into a running average on the arbitrator profile.
- **Composite Reputation Score** — Weighted blend of satisfaction average (60%) and impartiality score (40%).
- **Challenge System** — Parties may challenge a ruling within a 144-block window (~24 hours). Peer reviewers resolve challenges; upheld challenges are recorded against the arbitrator.
- **Peer Reviewer Management** — Owner-managed set of trusted principals authorized to resolve challenges.

---

## Contract Functions

### Read-Only

| Function | Parameters | Returns | Description |
|---|---|---|---|
| `get-arbitrator` | `arb: principal` | `optional arbitrator-data` | Returns the full arbitrator profile. |
| `get-arbitrator-satisfaction-avg` | `arb: principal` | `(ok uint)` | Average satisfaction score across all votes received. |
| `get-impartiality-score` | `arb: principal` | `(ok uint)` | Score 0–100 measuring ruling distribution balance. Returns `0` if fewer than 5 closed cases exist. |
| `get-reputation-score` | `arb: principal` | `(ok uint)` | Composite score: `(satisfaction_avg * 0.6) + (impartiality * 0.4)`. |
| `get-challenge-rate` | `arb: principal` | `(ok uint)` | Percentage of challenges that were upheld against the arbitrator. |
| `get-total-arbitrators` | — | `(ok uint)` | Total number of registered arbitrators. |
| `get-case` | `case-id: string-ascii 64` | `optional case-data` | Returns full case record. |
| `get-satisfaction-score` | `case-id: string-ascii 64`, `scorer: principal` | `optional score-data` | Returns a specific party's satisfaction submission for a case. |
| `get-challenge` | `case-id: string-ascii 64` | `optional challenge-data` | Returns the challenge record for a case. |
| `get-global-stats` | — | `(ok stats)` | Returns total arbitrators, cases opened, and cases closed. |
| `is-peer-reviewer` | `reviewer: principal` | `bool` | Returns `true` if the principal is an authorized peer reviewer. |

---

### Public — Arbitrator Management

#### `register-arbitrator`