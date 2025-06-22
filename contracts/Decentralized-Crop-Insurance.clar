(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-insufficient-funds (err u102))
(define-constant err-already-exists (err u103))
(define-constant err-invalid-amount (err u104))
(define-constant err-policy-expired (err u105))
(define-constant err-policy-not-active (err u106))
(define-constant err-unauthorized-oracle (err u107))
(define-constant err-claim-already-processed (err u108))
(define-constant err-invalid-weather-data (err u109))

(define-data-var next-policy-id uint u1)
(define-data-var insurance-pool uint u0)
(define-data-var total-premiums-collected uint u0)
(define-data-var total-claims-paid uint u0)

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

(define-map farmer-policies
  { farmer: principal }
  { policy-ids: (list 50 uint) }
)

(define-map authorized-oracles
  { oracle: principal }
  { is-authorized: bool }
)

(define-map weather-reports
  {
    location: (string-ascii 100),
    report-block: uint
  }
  {
    oracle: principal,
    temperature: int,
    rainfall: uint,
    flood-level: uint,
    drought-indicator: uint,
    timestamp: uint
  }
)

(define-map claim-triggers
  { policy-id: uint }
  {
    triggered: bool,
    trigger-type: (string-ascii 20),
    weather-data: (string-ascii 200),
    trigger-block: uint
  }
)

(define-public (add-oracle (oracle principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (ok (map-set authorized-oracles { oracle: oracle } { is-authorized: true }))
  )
)

(define-public (remove-oracle (oracle principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (ok (map-delete authorized-oracles { oracle: oracle }))
  )
)

(define-public (create-policy 
  (premium-amount uint)
  (coverage-amount uint)
  (crop-type (string-ascii 50))
  (location (string-ascii 100))
  (duration-blocks uint))
  (let
    (
      (policy-id (var-get next-policy-id))
      (current-block stacks-block-height)
      (end-block (+ current-block duration-blocks))
      (current-policies (default-to { policy-ids: (list) } 
        (map-get? farmer-policies { farmer: tx-sender })))
    )
    (asserts! (> premium-amount u0) err-invalid-amount)
    (asserts! (> coverage-amount u0) err-invalid-amount)
    (asserts! (> duration-blocks u0) err-invalid-amount)
    (try! (stx-transfer? premium-amount tx-sender (as-contract tx-sender)))
    (map-set policies
      { policy-id: policy-id }
      {
        farmer: tx-sender,
        premium-amount: premium-amount,
        coverage-amount: coverage-amount,
        crop-type: crop-type,
        location: location,
        start-block: current-block,
        end-block: end-block,
        is-active: true,
        claim-processed: false
      }
    )
    (map-set farmer-policies
      { farmer: tx-sender }
      { policy-ids: (unwrap! (as-max-len? 
        (append (get policy-ids current-policies) policy-id) u50) err-invalid-amount) }
    )
    (var-set next-policy-id (+ policy-id u1))
    (var-set insurance-pool (+ (var-get insurance-pool) premium-amount))
    (var-set total-premiums-collected (+ (var-get total-premiums-collected) premium-amount))
    (ok policy-id)
  )
)

(define-public (submit-weather-report
  (location (string-ascii 100))
  (temperature int)
  (rainfall uint)
  (flood-level uint)
  (drought-indicator uint))
  (let
    (
      (current-block stacks-block-height)
      (oracle-auth (default-to { is-authorized: false } 
        (map-get? authorized-oracles { oracle: tx-sender })))
    )
    (asserts! (get is-authorized oracle-auth) err-unauthorized-oracle)
    (asserts! (and (>= temperature -50) (<= temperature 60)) err-invalid-weather-data)
    (asserts! (<= rainfall u1000) err-invalid-weather-data)
    (asserts! (<= flood-level u100) err-invalid-weather-data)
    (asserts! (<= drought-indicator u100) err-invalid-weather-data)
    (map-set weather-reports
      { location: location, report-block: current-block }
      {
        oracle: tx-sender,
        temperature: temperature,
        rainfall: rainfall,
        flood-level: flood-level,
        drought-indicator: drought-indicator,
        timestamp: current-block
      }
    )
    (unwrap-panic (check-and-trigger-claims location current-block))
    (ok true)
  )
)
(define-private (check-and-trigger-claims (location (string-ascii 100)) (current-block uint))
  (let
    (
      (weather-data (map-get? weather-reports { location: location, report-block: current-block }))
    )
    (match weather-data
      report
      (begin
        (unwrap-panic (process-flood-claims location report current-block))
        (unwrap-panic (process-drought-claims location report current-block))
        (unwrap-panic (process-extreme-weather-claims location report current-block))
        (ok true)
      )
      (ok false)
    )
  )
)

(define-private (process-flood-claims 
  (location (string-ascii 100)) 
  (weather-report { oracle: principal, temperature: int, rainfall: uint, flood-level: uint, drought-indicator: uint, timestamp: uint })
  (current-block uint))
  (if (>= (get flood-level weather-report) u70)
    (trigger-claims-for-location location "flood" current-block)
    (ok false)
  )
)

(define-private (process-drought-claims 
  (location (string-ascii 100)) 
  (weather-report { oracle: principal, temperature: int, rainfall: uint, flood-level: uint, drought-indicator: uint, timestamp: uint })
  (current-block uint))
  (if (>= (get drought-indicator weather-report) u80)
    (trigger-claims-for-location location "drought" current-block)
    (ok false)
  )
)

(define-private (process-extreme-weather-claims 
  (location (string-ascii 100)) 
  (weather-report { oracle: principal, temperature: int, rainfall: uint, flood-level: uint, drought-indicator: uint, timestamp: uint })
  (current-block uint))
  (if (or 
      (and (>= (get rainfall weather-report) u300) (>= (get flood-level weather-report) u50))
      (and (<= (get rainfall weather-report) u10) (>= (get drought-indicator weather-report) u60))
      (or (<= (get temperature weather-report) -10) (>= (get temperature weather-report) 45)))
    (trigger-claims-for-location location "extreme-weather" current-block)
    (ok false)
  )
)

(define-private (trigger-claims-for-location (location (string-ascii 100)) (trigger-type (string-ascii 20)) (current-block uint))
  (ok true)
)

(define-public (process-claim (policy-id uint))
  (let
    (
      (policy (unwrap! (map-get? policies { policy-id: policy-id }) err-not-found))
      (current-block stacks-block-height)
      (claim-trigger (map-get? claim-triggers { policy-id: policy-id }))
    )
    (asserts! (get is-active policy) err-policy-not-active)
    (asserts! (<= current-block (get end-block policy)) err-policy-expired)
    (asserts! (not (get claim-processed policy)) err-claim-already-processed)
    (asserts! (is-some claim-trigger) err-not-found)
    (let
      (
        (trigger-data (unwrap-panic claim-trigger))
        (payout-amount (get coverage-amount policy))
      )
      (asserts! (get triggered trigger-data) err-not-found)
      (asserts! (>= (var-get insurance-pool) payout-amount) err-insufficient-funds)
      (try! (as-contract (stx-transfer? payout-amount tx-sender (get farmer policy))))
      (map-set policies
        { policy-id: policy-id }
        (merge policy { claim-processed: true, is-active: false })
      )
      (var-set insurance-pool (- (var-get insurance-pool) payout-amount))
      (var-set total-claims-paid (+ (var-get total-claims-paid) payout-amount))
      (ok payout-amount)
    )
  )
)

(define-public (manual-trigger-claim 
  (policy-id uint) 
  (trigger-type (string-ascii 20)) 
  (weather-data (string-ascii 200)))
  (let
    (
      (policy (unwrap! (map-get? policies { policy-id: policy-id }) err-not-found))
      (current-block stacks-block-height)
      (oracle-auth (default-to { is-authorized: false } 
        (map-get? authorized-oracles { oracle: tx-sender })))
    )
    (asserts! (get is-authorized oracle-auth) err-unauthorized-oracle)
    (asserts! (get is-active policy) err-policy-not-active)
    (asserts! (<= current-block (get end-block policy)) err-policy-expired)
    (map-set claim-triggers
      { policy-id: policy-id }
      {
        triggered: true,
        trigger-type: trigger-type,
        weather-data: weather-data,
        trigger-block: current-block
      }
    )
    (ok true)
  )
)

(define-public (fund-insurance-pool (amount uint))
  (begin
    (asserts! (> amount u0) err-invalid-amount)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (var-set insurance-pool (+ (var-get insurance-pool) amount))
    (ok true)
  )
)

(define-public (withdraw-excess-funds (amount uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (>= (var-get insurance-pool) amount) err-insufficient-funds)
    (try! (as-contract (stx-transfer? amount tx-sender contract-owner)))
    (var-set insurance-pool (- (var-get insurance-pool) amount))
    (ok true)
  )
)

(define-read-only (get-policy (policy-id uint))
  (map-get? policies { policy-id: policy-id })
)

(define-read-only (get-farmer-policies (farmer principal))
  (map-get? farmer-policies { farmer: farmer })
)

(define-read-only (get-weather-report (location (string-ascii 100)) (report-block uint))
  (map-get? weather-reports { location: location, report-block: report-block })
)

(define-read-only (get-claim-trigger (policy-id uint))
  (map-get? claim-triggers { policy-id: policy-id })
)

(define-read-only (get-insurance-pool-balance)
  (var-get insurance-pool)
)

(define-read-only (get-contract-stats)
  {
    total-premiums: (var-get total-premiums-collected),
    total-claims: (var-get total-claims-paid),
    pool-balance: (var-get insurance-pool),
    next-policy-id: (var-get next-policy-id)
  }
)

(define-read-only (is-oracle-authorized (oracle principal))
  (default-to { is-authorized: false } (map-get? authorized-oracles { oracle: oracle }))
)
