;; JudgeRep - Arbitrator Impartiality Tracking Contract
;; Tracks arbitrator reputation, consistency metrics, and party satisfaction
;; across arbitration cases on the Stacks blockchain.

;; ============================================================
;;  CONSTANTS
;; ============================================================

(define-constant CONTRACT-OWNER tx-sender)

(define-constant ERR-NOT-OWNER             (err u100))
(define-constant ERR-NOT-REGISTERED       (err u101))
(define-constant ERR-ALREADY-REGISTERED   (err u102))
(define-constant ERR-CASE-NOT-FOUND       (err u103))
(define-constant ERR-CASE-ALREADY-EXISTS  (err u104))
(define-constant ERR-CASE-CLOSED          (err u105))
(define-constant ERR-CASE-NOT-CLOSED      (err u106))
(define-constant ERR-UNAUTHORIZED         (err u107))
(define-constant ERR-ALREADY-SCORED       (err u108))
(define-constant ERR-INVALID-SCORE        (err u109))
(define-constant ERR-INVALID-RULING       (err u110))
(define-constant ERR-ARBITRATOR-INACTIVE  (err u111))
(define-constant ERR-ZERO-ADDRESS         (err u112))
(define-constant ERR-SELF-RULING          (err u113))
(define-constant ERR-CHALLENGE-WINDOW     (err u114))
(define-constant ERR-ALREADY-CHALLENGED   (err u115))

;; Scoring bounds (1-100)
(define-constant MIN-SCORE u1)
(define-constant MAX-SCORE u100)

;; Ruling options
(define-constant RULING-PARTY-A   u1)
(define-constant RULING-PARTY-B   u2)
(define-constant RULING-SPLIT     u3)    ;; e.g. 50/50 or negotiated split
(define-constant RULING-DISMISSED u4)

;; Challenge window in blocks (~24 h at ~10 min/block)
(define-constant CHALLENGE-WINDOW-BLOCKS u144)

;; Minimum cases before consistency score is considered reliable
(define-constant MIN-CASES-FOR-RELIABILITY u5)

;; ============================================================
;;  DATA MAPS & VARS
;; ============================================================

;; -- Arbitrators --

(define-map arbitrators
  { arbitrator: principal }
  {
    display-name:        (string-utf8 64),
    registered-at:       uint,            ;; block height
    is-active:           bool,
    total-cases:         uint,
    closed-cases:        uint,
    total-satisfaction:  uint,            ;; sum of all satisfaction scores received
    satisfaction-votes:  uint,            ;; number of satisfaction scores received
    ;; Consistency: tracks ruling-type distribution for impartiality
    rulings-for-a:       uint,
    rulings-for-b:       uint,
    rulings-split:       uint,
    rulings-dismissed:   uint,
    ;; Challenge tracking
    total-challenges:    uint,
    upheld-challenges:   uint
  }
)

;; -- Cases --

(define-map cases
  { case-id: (string-ascii 64) }
  {
    arbitrator:            principal,
    party-a:               principal,
    party-b:               principal,
    description-hash:      (buff 32),    ;; SHA-256 of case description (off-chain)
    opened-at:             uint,         ;; block height
    closed-at:             uint,         ;; 0 = still open
    ruling:                uint,         ;; 0 = no ruling yet
    ruling-rationale-hash: (buff 32),    ;; SHA-256 of rationale document (off-chain)
    is-closed:             bool,
    is-challenged:         bool,
    challenge-resolved:    bool
  }
)

;; -- Party satisfaction scores (one per party per case) --
;; key: {case-id, scorer}  value: satisfaction score 1-100

(define-map satisfaction-scores
  { case-id: (string-ascii 64), scorer: principal }
  { score: uint, submitted-at: uint }
)

;; -- Challenges --

(define-map challenges
  { case-id: (string-ascii 64) }
  {
    challenger:    principal,
    reason-hash:   (buff 32),
    challenged-at: uint,
    resolved:      bool,
    upheld:        bool,
    resolver:      principal
  }
)

;; -- Peer reviewers (trusted principals that can resolve challenges) --

(define-map peer-reviewers
  { reviewer: principal }
  { added-at: uint }
)

;; -- Global stats --

(define-data-var total-arbitrators  uint u0)
(define-data-var total-cases-opened uint u0)
(define-data-var total-cases-closed uint u0)

;; ============================================================
;;  PRIVATE HELPERS
;; ============================================================

(define-private (is-owner)
  (is-eq tx-sender CONTRACT-OWNER)
)

(define-private (arbitrator-exists (arb principal))
  (is-some (map-get? arbitrators { arbitrator: arb }))
)

(define-private (case-exists (case-id (string-ascii 64)))
  (is-some (map-get? cases { case-id: case-id }))
)

;; Safe integer division returning 0 on divide-by-zero
(define-private (safe-div (num uint) (den uint))
  (if (is-eq den u0)
    u0
    (/ num den)
  )
)

;; ============================================================
;;  READ-ONLY: ARBITRATOR QUERIES
;; ============================================================

(define-read-only (get-arbitrator (arb principal))
  (map-get? arbitrators { arbitrator: arb })
)

(define-read-only (get-arbitrator-satisfaction-avg (arb principal))
  (match (map-get? arbitrators { arbitrator: arb })
    data (ok (safe-div (get total-satisfaction data) (get satisfaction-votes data)))
    (err ERR-NOT-REGISTERED)
  )
)

;; Impartiality score: measures how balanced the arbitrator's rulings are.
;; Returns a value 0-100 where 100 = perfectly balanced (equal split between A/B/split/dismissed).
;; Formula: 100 - normalised_max_deviation_across_buckets
(define-read-only (get-impartiality-score (arb principal))
  (match (map-get? arbitrators { arbitrator: arb })
    data
      (let (
        (total (get closed-cases data))
      )
        (if (< total MIN-CASES-FOR-RELIABILITY)
          (ok u0)   ;; not enough data yet
          (let (
            (a (get rulings-for-a    data))
            (b (get rulings-for-b    data))
            (s (get rulings-split    data))
            (d (get rulings-dismissed data))
            ;; expected count per bucket if perfectly balanced
            (expected (/ total u4))
            ;; absolute deviations
            (dev-a (if (>= a expected) (- a expected) (- expected a)))
            (dev-b (if (>= b expected) (- b expected) (- expected b)))
            (dev-s (if (>= s expected) (- s expected) (- expected s)))
            (dev-d (if (>= d expected) (- d expected) (- expected d)))
            ;; max deviation
            (max-dev
              (let (
                (m1 (if (>= dev-a dev-b) dev-a dev-b))
                (m2 (if (>= dev-s dev-d) dev-s dev-d))
              )
                (if (>= m1 m2) m1 m2)
              )
            )
            ;; impartiality = (1 - max_dev/total) * 100
            (impartiality (- u100 (safe-div (* max-dev u100) total)))
          )
            (ok impartiality)
          )
        )
      )
    (err ERR-NOT-REGISTERED)
  )
)

;; Composite reputation score: weighted blend of satisfaction (60%) + impartiality (40%)
(define-read-only (get-reputation-score (arb principal))
  (match (map-get? arbitrators { arbitrator: arb })
    data
      (let (
        (sat-avg (safe-div (get total-satisfaction data) (get satisfaction-votes data)))
      )
        (match (get-impartiality-score arb)
          impartiality
            (ok (+
              (/ (* sat-avg u60) u100)
              (/ (* impartiality u40) u100)
            ))
          e (err e)
        )
      )
    (err ERR-NOT-REGISTERED)
  )
)

;; Challenge upheld rate expressed as a percentage (0-100)
(define-read-only (get-challenge-rate (arb principal))
  (match (map-get? arbitrators { arbitrator: arb })
    data (ok (safe-div (* (get upheld-challenges data) u100) (get total-challenges data)))
    (err ERR-NOT-REGISTERED)
  )
)

(define-read-only (get-total-arbitrators)
  (ok (var-get total-arbitrators))
)

;; ============================================================
;;  READ-ONLY: CASE QUERIES
;; ============================================================

(define-read-only (get-case (case-id (string-ascii 64)))
  (map-get? cases { case-id: case-id })
)

(define-read-only (get-satisfaction-score (case-id (string-ascii 64)) (scorer principal))
  (map-get? satisfaction-scores { case-id: case-id, scorer: scorer })
)

(define-read-only (get-challenge (case-id (string-ascii 64)))
  (map-get? challenges { case-id: case-id })
)

(define-read-only (get-global-stats)
  (ok {
    total-arbitrators:  (var-get total-arbitrators),
    total-cases-opened: (var-get total-cases-opened),
    total-cases-closed: (var-get total-cases-closed)
  })
)

(define-read-only (is-peer-reviewer (reviewer principal))
  (is-some (map-get? peer-reviewers { reviewer: reviewer }))
)

;; ============================================================
;;  PUBLIC: ARBITRATOR MANAGEMENT
;; ============================================================

;; Any principal can self-register as an arbitrator
(define-public (register-arbitrator (display-name (string-utf8 64)))
  (begin
    (asserts! (not (arbitrator-exists tx-sender)) ERR-ALREADY-REGISTERED)
    (asserts! (> (len display-name) u0) ERR-ZERO-ADDRESS)
    (map-set arbitrators
      { arbitrator: tx-sender }
      {
        display-name:       display-name,
        registered-at:      stacks-block-height,
        is-active:          true,
        total-cases:        u0,
        closed-cases:       u0,
        total-satisfaction: u0,
        satisfaction-votes: u0,
        rulings-for-a:      u0,
        rulings-for-b:      u0,
        rulings-split:      u0,
        rulings-dismissed:  u0,
        total-challenges:   u0,
        upheld-challenges:  u0
      }
    )
    (var-set total-arbitrators (+ (var-get total-arbitrators) u1))
    (ok true)
  )
)

;; Arbitrator can toggle their own active status
(define-public (set-arbitrator-active (active bool))
  (match (map-get? arbitrators { arbitrator: tx-sender })
    data
      (begin
        (map-set arbitrators
          { arbitrator: tx-sender }
          (merge data { is-active: active })
        )
        (ok true)
      )
    ERR-NOT-REGISTERED
  )
)

;; Owner can forcefully deactivate a misbehaving arbitrator
(define-public (deactivate-arbitrator (arb principal))
  (begin
    (asserts! (is-owner) ERR-NOT-OWNER)
    (match (map-get? arbitrators { arbitrator: arb })
      data
        (begin
          (map-set arbitrators
            { arbitrator: arb }
            (merge data { is-active: false })
          )
          (ok true)
        )
      ERR-NOT-REGISTERED
    )
  )
)

;; ============================================================
;;  PUBLIC: PEER REVIEWER MANAGEMENT
;; ============================================================

(define-public (add-peer-reviewer (reviewer principal))
  (begin
    (asserts! (is-owner) ERR-NOT-OWNER)
    (map-set peer-reviewers { reviewer: reviewer } { added-at: stacks-block-height })
    (ok true)
  )
)

(define-public (remove-peer-reviewer (reviewer principal))
  (begin
    (asserts! (is-owner) ERR-NOT-OWNER)
    (map-delete peer-reviewers { reviewer: reviewer })
    (ok true)
  )
)

;; ============================================================
;;  PUBLIC: CASE LIFECYCLE
;; ============================================================

;; Open a new arbitration case.
;; Either party-a, party-b, or the arbitrator themselves may open a case.
(define-public (open-case
    (case-id          (string-ascii 64))
    (arbitrator       principal)
    (party-a          principal)
    (party-b          principal)
    (description-hash (buff 32)))
  (begin
    (asserts! (not (case-exists case-id))  ERR-CASE-ALREADY-EXISTS)
    (asserts! (arbitrator-exists arbitrator) ERR-NOT-REGISTERED)
    (asserts!
      (match (map-get? arbitrators { arbitrator: arbitrator })
        d (get is-active d)
        false
      )
      ERR-ARBITRATOR-INACTIVE
    )
    ;; Opener must be the arbitrator, party-a, or party-b
    (asserts!
      (or (is-eq tx-sender arbitrator)
          (is-eq tx-sender party-a)
          (is-eq tx-sender party-b))
      ERR-UNAUTHORIZED
    )
    ;; Arbitrator must not also be a party (prevents bias)
    (asserts! (not (is-eq arbitrator party-a)) ERR-SELF-RULING)
    (asserts! (not (is-eq arbitrator party-b)) ERR-SELF-RULING)
    ;; Parties must be distinct principals
    (asserts! (not (is-eq party-a party-b)) ERR-ZERO-ADDRESS)

    (map-set cases
      { case-id: case-id }
      {
        arbitrator:            arbitrator,
        party-a:               party-a,
        party-b:               party-b,
        description-hash:      description-hash,
        opened-at:             stacks-block-height,
        closed-at:             u0,
        ruling:                u0,
        ruling-rationale-hash: 0x0000000000000000000000000000000000000000000000000000000000000000,
        is-closed:             false,
        is-challenged:         false,
        challenge-resolved:    false
      }
    )

    ;; Increment arbitrator's total-cases counter
    (match (map-get? arbitrators { arbitrator: arbitrator })
      arb-data
        (map-set arbitrators
          { arbitrator: arbitrator }
          (merge arb-data { total-cases: (+ (get total-cases arb-data) u1) })
        )
      false
    )

    (var-set total-cases-opened (+ (var-get total-cases-opened) u1))
    (ok true)
  )
)

;; Arbitrator submits a ruling to close the case.
(define-public (submit-ruling
    (case-id               (string-ascii 64))
    (ruling                uint)
    (ruling-rationale-hash (buff 32)))
  (match (map-get? cases { case-id: case-id })
    case-data
      (begin
        (asserts! (is-eq tx-sender (get arbitrator case-data)) ERR-UNAUTHORIZED)
        (asserts! (not (get is-closed case-data))              ERR-CASE-CLOSED)
        (asserts!
          (or (is-eq ruling RULING-PARTY-A)
              (is-eq ruling RULING-PARTY-B)
              (is-eq ruling RULING-SPLIT)
              (is-eq ruling RULING-DISMISSED))
          ERR-INVALID-RULING
        )

        ;; Close the case with the ruling
        (map-set cases
          { case-id: case-id }
          (merge case-data {
            ruling:                ruling,
            ruling-rationale-hash: ruling-rationale-hash,
            closed-at:             block-height,
            is-closed:             true
          })
        )

        ;; Update arbitrator ruling counts & closed-cases
        (match (map-get? arbitrators { arbitrator: (get arbitrator case-data) })
          arb-data
            (map-set arbitrators
              { arbitrator: (get arbitrator case-data) }
              (merge arb-data {
                closed-cases:      (+ (get closed-cases arb-data) u1),
                rulings-for-a:     (if (is-eq ruling RULING-PARTY-A)
                                     (+ (get rulings-for-a arb-data) u1)
                                     (get rulings-for-a arb-data)),
                rulings-for-b:     (if (is-eq ruling RULING-PARTY-B)
                                     (+ (get rulings-for-b arb-data) u1)
                                     (get rulings-for-b arb-data)),
                rulings-split:     (if (is-eq ruling RULING-SPLIT)
                                     (+ (get rulings-split arb-data) u1)
                                     (get rulings-split arb-data)),
                rulings-dismissed: (if (is-eq ruling RULING-DISMISSED)
                                     (+ (get rulings-dismissed arb-data) u1)
                                     (get rulings-dismissed arb-data))
              })
            )
          false
        )

        (var-set total-cases-closed (+ (var-get total-cases-closed) u1))
        (ok true)
      )
    ERR-CASE-NOT-FOUND
  )
)

;; ============================================================
;;  PUBLIC: SATISFACTION SCORING
;; ============================================================

;; Either party may submit a satisfaction score (1-100) once the case is closed.
;; Each party gets exactly one vote per case.
(define-public (submit-satisfaction-score
    (case-id (string-ascii 64))
    (score   uint))
  (match (map-get? cases { case-id: case-id })
    case-data
      (begin
        (asserts! (get is-closed case-data) ERR-CASE-NOT-CLOSED)
        ;; Only party-a or party-b may score
        (asserts!
          (or (is-eq tx-sender (get party-a case-data))
              (is-eq tx-sender (get party-b case-data)))
          ERR-UNAUTHORIZED
        )
        ;; One score per party per case
        (asserts!
          (is-none (map-get? satisfaction-scores { case-id: case-id, scorer: tx-sender }))
          ERR-ALREADY-SCORED
        )
        ;; Validate score range 1-100
        (asserts! (and (>= score MIN-SCORE) (<= score MAX-SCORE)) ERR-INVALID-SCORE)

        (map-set satisfaction-scores
          { case-id: case-id, scorer: tx-sender }
          { score: score, submitted-at: block-height }
        )

        ;; Aggregate onto arbitrator profile
        (match (map-get? arbitrators { arbitrator: (get arbitrator case-data) })
          arb-data
            (begin
              (map-set arbitrators
                { arbitrator: (get arbitrator case-data) }
                (merge arb-data {
                  total-satisfaction: (+ (get total-satisfaction arb-data) score),
                  satisfaction-votes: (+ (get satisfaction-votes arb-data) u1)
                })
              )
              (ok true)
            )
          ERR-NOT-REGISTERED
        )
      )
    ERR-CASE-NOT-FOUND
  )
)

;; ============================================================
;;  PUBLIC: CHALLENGE SYSTEM
;; ============================================================

;; Either party can challenge a ruling within the challenge window.
(define-public (challenge-ruling
    (case-id     (string-ascii 64))
    (reason-hash (buff 32)))
  (match (map-get? cases { case-id: case-id })
    case-data
      (begin
        (asserts! (get is-closed case-data)           ERR-CASE-NOT-CLOSED)
        (asserts! (not (get is-challenged case-data)) ERR-ALREADY-CHALLENGED)
        ;; Only a party to the case may challenge
        (asserts!
          (or (is-eq tx-sender (get party-a case-data))
              (is-eq tx-sender (get party-b case-data)))
          ERR-UNAUTHORIZED
        )
        ;; Must be within the post-ruling challenge window
        (asserts!
          (<= block-height (+ (get closed-at case-data) CHALLENGE-WINDOW-BLOCKS))
          ERR-CHALLENGE-WINDOW
        )

        (map-set challenges
          { case-id: case-id }
          {
            challenger:    tx-sender,
            reason-hash:   reason-hash,
            challenged-at: block-height,
            resolved:      false,
            upheld:        false,
            resolver:      CONTRACT-OWNER
          }
        )

        (map-set cases
          { case-id: case-id }
          (merge case-data { is-challenged: true })
        )

        ;; Increment arbitrator challenge counter
        (match (map-get? arbitrators { arbitrator: (get arbitrator case-data) })
          arb-data
            (map-set arbitrators
              { arbitrator: (get arbitrator case-data) }
              (merge arb-data {
                total-challenges: (+ (get total-challenges arb-data) u1)
              })
            )
          false
        )

        (ok true)
      )
    ERR-CASE-NOT-FOUND
  )
)

;; A peer reviewer resolves a pending challenge.
;; upheld = true  -> the challenge was valid (arbitrator erred)
;; upheld = false -> the challenge was dismissed (ruling stands)
(define-public (resolve-challenge
    (case-id (string-ascii 64))
    (upheld  bool))
  (begin
    (asserts! (is-peer-reviewer tx-sender) ERR-UNAUTHORIZED)
    (match (map-get? cases { case-id: case-id })
      case-data
        (begin
          (asserts! (get is-challenged case-data)            ERR-UNAUTHORIZED)
          (asserts! (not (get challenge-resolved case-data)) ERR-CASE-CLOSED)

          ;; Update challenge record
          (match (map-get? challenges { case-id: case-id })
            ch-data
              (map-set challenges
                { case-id: case-id }
                (merge ch-data {
                  resolved: true,
                  upheld:   upheld,
                  resolver: tx-sender
                })
              )
            false
          )

          ;; Mark challenge as resolved on the case
          (map-set cases
            { case-id: case-id }
            (merge case-data { challenge-resolved: true })
          )

          ;; If upheld, increment arbitrator's upheld-challenges counter
          (if upheld
            (match (map-get? arbitrators { arbitrator: (get arbitrator case-data) })
              arb-data
                (map-set arbitrators
                  { arbitrator: (get arbitrator case-data) }
                  (merge arb-data {
                    upheld-challenges: (+ (get upheld-challenges arb-data) u1)
                  })
                )
              false
            )
            false
          )

          (ok true)
        )
      ERR-CASE-NOT-FOUND
    )
  )
)

;; ============================================================
;;  PUBLIC: OWNER UTILITIES
;; ============================================================

;; Emergency: owner can reopen a case (e.g. after a successful challenge)
;; This reverses the ruling counts on the arbitrator's record.
(define-public (reopen-case (case-id (string-ascii 64)))
  (begin
    (asserts! (is-owner) ERR-NOT-OWNER)
    (match (map-get? cases { case-id: case-id })
      case-data
        (let (
          (prior-ruling (get ruling case-data))
        )
          (map-set cases
            { case-id: case-id }
            (merge case-data {
              is-closed:          false,
              ruling:             u0,
              closed-at:          u0,
              challenge-resolved: false
            })
          )

          ;; Roll back arbitrator counters if the case was previously closed
          (if (get is-closed case-data)
            (match (map-get? arbitrators { arbitrator: (get arbitrator case-data) })
              arb-data
                (map-set arbitrators
                  { arbitrator: (get arbitrator case-data) }
                  (merge arb-data {
                    closed-cases:      (if (> (get closed-cases arb-data) u0)
                                         (- (get closed-cases arb-data) u1)
                                         u0),
                    rulings-for-a:     (if (and (is-eq prior-ruling RULING-PARTY-A)
                                               (> (get rulings-for-a arb-data) u0))
                                         (- (get rulings-for-a arb-data) u1)
                                         (get rulings-for-a arb-data)),
                    rulings-for-b:     (if (and (is-eq prior-ruling RULING-PARTY-B)
                                               (> (get rulings-for-b arb-data) u0))
                                         (- (get rulings-for-b arb-data) u1)
                                         (get rulings-for-b arb-data)),
                    rulings-split:     (if (and (is-eq prior-ruling RULING-SPLIT)
                                               (> (get rulings-split arb-data) u0))
                                         (- (get rulings-split arb-data) u1)
                                         (get rulings-split arb-data)),
                    rulings-dismissed: (if (and (is-eq prior-ruling RULING-DISMISSED)
                                               (> (get rulings-dismissed arb-data) u0))
                                         (- (get rulings-dismissed arb-data) u1)
                                         (get rulings-dismissed arb-data))
                  })
                )
              false
            )
            false
          )

          (var-set total-cases-closed
            (if (and (get is-closed case-data) (> (var-get total-cases-closed) u0))
              (- (var-get total-cases-closed) u1)
              (var-get total-cases-closed)
            )
          )
          (ok true)
        )
      ERR-CASE-NOT-FOUND
    )
  )
)
