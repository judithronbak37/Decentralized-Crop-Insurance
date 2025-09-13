(define-constant contract-owner tx-sender)
(define-constant err-not-found (err u200))
(define-constant err-owner-only (err u201))
(define-constant err-unauthorized-oracle (err u202))
(define-constant err-invalid-severity (err u203))
(define-constant err-duplicate-alert (err u204))

(define-data-var next-alert-id uint u1)
(define-data-var total-alerts-issued uint u0)

(define-map authorized-oracles
  { oracle: principal }
  { is-authorized: bool }
)

(define-map weather-alerts
  { alert-id: uint }
  {
    oracle: principal,
    location: (string-ascii 100),
    alert-type: (string-ascii 30),
    severity: uint,
    message: (string-ascii 200),
    issued-block: uint,
    expires-block: uint,
    is-active: bool
  }
)

(define-map farmer-subscriptions
  { farmer: principal }
  { locations: (list 10 (string-ascii 100)), notification-count: uint }
)

(define-map location-active-alerts
  { location: (string-ascii 100) }
  { alert-ids: (list 20 uint), active-count: uint }
)

(define-public (add-oracle (oracle principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (ok (map-set authorized-oracles { oracle: oracle } { is-authorized: true }))
  )
)

(define-public (issue-weather-alert
  (location (string-ascii 100))
  (alert-type (string-ascii 30))
  (severity uint)
  (message (string-ascii 200))
  (duration-blocks uint))
  (let
    (
      (oracle-auth (default-to { is-authorized: false } 
        (map-get? authorized-oracles { oracle: tx-sender })))
      (alert-id (var-get next-alert-id))
      (current-block stacks-block-height)
      (expires-block (+ current-block duration-blocks))
      (location-alerts (default-to { alert-ids: (list), active-count: u0 }
        (map-get? location-active-alerts { location: location })))
    )
    (asserts! (get is-authorized oracle-auth) err-unauthorized-oracle)
    (asserts! (and (>= severity u1) (<= severity u5)) err-invalid-severity)
    (map-set weather-alerts
      { alert-id: alert-id }
      {
        oracle: tx-sender,
        location: location,
        alert-type: alert-type,
        severity: severity,
        message: message,
        issued-block: current-block,
        expires-block: expires-block,
        is-active: true
      })
    (map-set location-active-alerts
      { location: location }
      {
        alert-ids: (unwrap! (as-max-len? 
          (append (get alert-ids location-alerts) alert-id) u20) err-duplicate-alert),
        active-count: (+ (get active-count location-alerts) u1)
      })
    (var-set next-alert-id (+ alert-id u1))
    (var-set total-alerts-issued (+ (var-get total-alerts-issued) u1))
    (ok alert-id)
  )
)

(define-public (subscribe-to-location (location (string-ascii 100)))
  (let
    (
      (current-sub (default-to { locations: (list), notification-count: u0 }
        (map-get? farmer-subscriptions { farmer: tx-sender })))
    )
    (map-set farmer-subscriptions
      { farmer: tx-sender }
      {
        locations: (unwrap! (as-max-len? 
          (append (get locations current-sub) location) u10) err-duplicate-alert),
        notification-count: (get notification-count current-sub)
      })
    (ok true)
  )
)

(define-read-only (get-active-alerts-for-location (location (string-ascii 100)))
  (map-get? location-active-alerts { location: location })
)

(define-read-only (get-alert-details (alert-id uint))
  (map-get? weather-alerts { alert-id: alert-id })
)

(define-read-only (get-farmer-subscriptions (farmer principal))
  (map-get? farmer-subscriptions { farmer: farmer })
)

(define-read-only (get-alert-stats)
  {
    total-alerts: (var-get total-alerts-issued),
    next-alert-id: (var-get next-alert-id)
  }
)
