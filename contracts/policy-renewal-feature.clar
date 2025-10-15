(define-constant err-policy-not-renewable (err u112))
(define-constant err-renewal-window-closed (err u113))
(define-constant err-renewal-already-processed (err u114))
(define-constant err-not-found (err u101))
(define-constant err-owner-only (err u102))
(define-constant err-invalid-amount (err u103))

(define-data-var next-policy-id uint u1)
(define-data-var insurance-pool uint u0)
(define-data-var total-premiums-collected uint u0)

(define-map policies
  { policy-id: uint }
  {
    farmer: principal,
    premium-amount: uint,
    coverage-amount: uint,
    crop-type: (string-ascii 50),
    location: (string-ascii 100),
    start-block: uint,
    end-block: uint,
    is-active: bool,
    claim-processed: bool
  }
)

(define-map location-risk-multipliers
  { location: (string-ascii 100) }
  { multiplier: uint }
)

(define-map crop-base-rates
  { crop-type: (string-ascii 50) }
  { base-rate: uint }
)

(define-map policy-renewals
  { policy-id: uint }
  {
    renewal-count: uint,
    last-renewal-block: uint,
    original-policy-id: uint,
    is-renewable: bool
  }
)

(define-map renewal-history
  { farmer: principal }
  { total-renewals: uint, active-renewals: uint }
)

(define-read-only (calculate-premium 
  (coverage-amount uint)
  (duration-blocks uint)
  (crop-type (string-ascii 50))
  (location (string-ascii 100)))
  (let
    (
      (location-risk (default-to { multiplier: u100 } 
        (map-get? location-risk-multipliers { location: location })))
      (crop-rate (default-to { base-rate: u5 } 
        (map-get? crop-base-rates { crop-type: crop-type })))
      (duration-years (/ duration-blocks u52560))
      (adjusted-duration (if (is-eq duration-years u0) u1 duration-years))
      (base-premium (/ (* coverage-amount (get base-rate crop-rate)) u100))
      (risk-adjusted-premium (/ (* base-premium (get multiplier location-risk)) u100))
      (final-premium (/ (* risk-adjusted-premium adjusted-duration) u1))
    )
    (if (< final-premium u1000) u1000 final-premium)
  )
)

(define-public (renew-policy 
  (policy-id uint)
  (new-coverage-amount uint)
  (new-duration-blocks uint))
  (let
    (
      (policy (unwrap! (map-get? policies { policy-id: policy-id }) err-not-found))
      (current-block stacks-block-height)
      (renewal-data (default-to 
        { renewal-count: u0, last-renewal-block: u0, original-policy-id: policy-id, is-renewable: true }
        (map-get? policy-renewals { policy-id: policy-id })))
      (farmer-history (default-to 
        { total-renewals: u0, active-renewals: u0 }
        (map-get? renewal-history { farmer: (get farmer policy) })))
      (renewal-window-start (- (get end-block policy) u1440))
      (renewal-window-end (+ (get end-block policy) u720))
      (new-premium (calculate-premium 
        new-coverage-amount 
        new-duration-blocks 
        (get crop-type policy) 
        (get location policy)))
      (new-policy-id (var-get next-policy-id))
    )
    (begin      (asserts! (is-eq tx-sender (get farmer policy)) err-owner-only)
      (asserts! (get is-renewable renewal-data) err-policy-not-renewable)
      (asserts! (and (>= current-block renewal-window-start) 
                     (<= current-block renewal-window-end)) err-renewal-window-closed)
      (asserts! (> new-coverage-amount u0) err-invalid-amount)
      (asserts! (> new-duration-blocks u0) err-invalid-amount)
      (try! (stx-transfer? new-premium tx-sender (as-contract tx-sender)))
      (map-set policies
        { policy-id: policy-id }
        (merge policy { is-active: false }))
      (map-set policies
        { policy-id: new-policy-id }
        {
          farmer: (get farmer policy),
          premium-amount: new-premium,
          coverage-amount: new-coverage-amount,
          crop-type: (get crop-type policy),
          location: (get location policy),
          start-block: current-block,
          end-block: (+ current-block new-duration-blocks),
          is-active: true,
          claim-processed: false
        })
      (map-set policy-renewals
        { policy-id: new-policy-id }
        {
          renewal-count: (+ (get renewal-count renewal-data) u1),
          last-renewal-block: current-block,
          original-policy-id: (get original-policy-id renewal-data),
          is-renewable: true
        })
      (map-set renewal-history
        { farmer: (get farmer policy) }
        {
          total-renewals: (+ (get total-renewals farmer-history) u1),
          active-renewals: (+ (get active-renewals farmer-history) u1)
        })
      (var-set next-policy-id (+ new-policy-id u1))
      (var-set insurance-pool (+ (var-get insurance-pool) new-premium))
      (var-set total-premiums-collected (+ (var-get total-premiums-collected) new-premium))
      (ok new-policy-id)
    )
  )
)

(define-read-only (get-renewal-info (policy-id uint))
  (map-get? policy-renewals { policy-id: policy-id })
)

(define-read-only (get-farmer-renewal-history (farmer principal))
  (map-get? renewal-history { farmer: farmer })
)

(define-read-only (check-renewal-eligibility (policy-id uint))
  (let
    (
      (policy (map-get? policies { policy-id: policy-id }))
      (current-block stacks-block-height)
    )
    (match policy
      policy-data
      (let
        (
          (renewal-window-start (- (get end-block policy-data) u1440))
          (renewal-window-end (+ (get end-block policy-data) u720))
          (renewal-data (map-get? policy-renewals { policy-id: policy-id }))
        )
        {
          eligible: (and 
            (get is-active policy-data)
            (>= current-block renewal-window-start)
            (<= current-block renewal-window-end)
            (match renewal-data
              data (get is-renewable data)
              true)),
          window-start: renewal-window-start,
          window-end: renewal-window-end,
          current-block: current-block
        }
      )
      { eligible: false, window-start: u0, window-end: u0, current-block: current-block }
    )
  )
)