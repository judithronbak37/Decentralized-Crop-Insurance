(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u300))
(define-constant err-not-found (err u301))
(define-constant err-insufficient-reputation (err u302))
(define-constant err-reward-already-claimed (err u303))
(define-constant err-invalid-points (err u304))

(define-data-var total-reputation-points uint u0)
(define-data-var total-rewards-distributed uint u0)

(define-map farmer-reputation
  { farmer: principal }
  {
    reputation-score: uint,
    policies-completed: uint,
    claim-free-periods: uint,
    total-premiums-paid: uint,
    last-activity-block: uint,
    tier: (string-ascii 20)
  }
)

(define-map reputation-milestones
  { farmer: principal, milestone-id: uint }
  {
    points-earned: uint,
    reward-claimed: bool,
    achieved-block: uint,
    milestone-type: (string-ascii 30)
  }
)

(define-map tier-benefits
  { tier: (string-ascii 20) }
  {
    premium-discount: uint,
    priority-level: uint,
    bonus-multiplier: uint
  }
)

(define-public (initialize-tiers)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set tier-benefits { tier: "bronze" } { premium-discount: u5, priority-level: u1, bonus-multiplier: u100 })
    (map-set tier-benefits { tier: "silver" } { premium-discount: u10, priority-level: u2, bonus-multiplier: u120 })
    (map-set tier-benefits { tier: "gold" } { premium-discount: u15, priority-level: u3, bonus-multiplier: u150 })
    (map-set tier-benefits { tier: "platinum" } { premium-discount: u20, priority-level: u5, bonus-multiplier: u200 })
    (ok true)
  )
)

(define-public (award-reputation-points 
  (farmer principal)
  (points uint)
  (reason (string-ascii 30)))
  (let
    (
      (current-rep (default-to 
        { reputation-score: u0, policies-completed: u0, claim-free-periods: u0, 
          total-premiums-paid: u0, last-activity-block: u0, tier: "bronze" }
        (map-get? farmer-reputation { farmer: farmer })))
      (new-score (+ (get reputation-score current-rep) points))
      (new-tier (calculate-tier new-score))
    )
    (asserts! (> points u0) err-invalid-points)
    (map-set farmer-reputation
      { farmer: farmer }
      (merge current-rep {
        reputation-score: new-score,
        last-activity-block: stacks-block-height,
        tier: new-tier
      }))
    (var-set total-reputation-points (+ (var-get total-reputation-points) points))
    (ok new-score)
  )
)

(define-private (calculate-tier (score uint))
  (if (>= score u1000)
    "platinum"
    (if (>= score u500)
      "gold"
      (if (>= score u200)
        "silver"
        "bronze"
      )
    )
  )
)

(define-public (claim-milestone-reward (milestone-id uint))
  (let
    (
      (milestone (unwrap! (map-get? reputation-milestones 
        { farmer: tx-sender, milestone-id: milestone-id }) err-not-found))
    )
    (asserts! (not (get reward-claimed milestone)) err-reward-already-claimed)
    (map-set reputation-milestones
      { farmer: tx-sender, milestone-id: milestone-id }
      (merge milestone { reward-claimed: true }))
    (ok true)
  )
)

(define-read-only (get-farmer-reputation (farmer principal))
  (map-get? farmer-reputation { farmer: farmer })
)

(define-read-only (get-tier-benefits (tier (string-ascii 20)))
  (map-get? tier-benefits { tier: tier })
)

(define-read-only (calculate-discount (farmer principal) (base-premium uint))
  (let
    (
      (rep-data (map-get? farmer-reputation { farmer: farmer }))
    )
    (match rep-data
      data
      (let
        (
          (tier-data (unwrap! (get-tier-benefits (get tier data)) (ok base-premium)))
          (discount-rate (get premium-discount tier-data))
          (discount-amount (/ (* base-premium discount-rate) u100))
        )
        (ok (- base-premium discount-amount))
      )
      (ok base-premium)
    )
  )
)

(define-read-only (get-reputation-stats)
  {
    total-points: (var-get total-reputation-points),
    total-rewards: (var-get total-rewards-distributed)
  }
)
