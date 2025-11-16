(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u400))
(define-constant err-not-found (err u401))
(define-constant err-already-voted (err u402))
(define-constant err-proposal-expired (err u403))
(define-constant err-proposal-not-passed (err u404))
(define-constant err-proposal-already-executed (err u405))
(define-constant err-invalid-amount (err u406))
(define-constant err-insufficient-voting-power (err u407))

(define-data-var next-proposal-id uint u1)
(define-data-var total-proposals uint u0)
(define-data-var min-voting-power uint u100)
(define-data-var quorum-percentage uint u30)
(define-data-var proposal-duration uint u1440)

(define-map proposals
  { proposal-id: uint }
  {
    proposer: principal,
    proposal-type: (string-ascii 30),
    amount: uint,
    recipient: principal,
    description: (string-ascii 200),
    created-block: uint,
    expires-block: uint,
    votes-for: uint,
    votes-against: uint,
    executed: bool,
    passed: bool
  }
)

(define-map voter-records
  { proposal-id: uint, voter: principal }
  { voting-power: uint, vote-for: bool, voted-block: uint }
)

(define-map voter-power
  { voter: principal }
  { power: uint, last-updated-block: uint }
)

(define-public (initialize-governance-params)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set min-voting-power u100)
    (var-set quorum-percentage u30)
    (var-set proposal-duration u1440)
    (ok true)
  )
)

(define-public (create-proposal
  (proposal-type (string-ascii 30))
  (amount uint)
  (recipient principal)
  (description (string-ascii 200)))
  (let
    (
      (proposal-id (var-get next-proposal-id))
      (current-block stacks-block-height)
      (expires-block (+ current-block (var-get proposal-duration)))
      (proposer-power (default-to { power: u0, last-updated-block: u0 }
        (map-get? voter-power { voter: tx-sender })))
    )
    (asserts! (>= (get power proposer-power) (var-get min-voting-power)) 
      err-insufficient-voting-power)
    (asserts! (> amount u0) err-invalid-amount)
    (map-set proposals
      { proposal-id: proposal-id }
      {
        proposer: tx-sender,
        proposal-type: proposal-type,
        amount: amount,
        recipient: recipient,
        description: description,
        created-block: current-block,
        expires-block: expires-block,
        votes-for: u0,
        votes-against: u0,
        executed: false,
        passed: false
      })
    (var-set next-proposal-id (+ proposal-id u1))
    (var-set total-proposals (+ (var-get total-proposals) u1))
    (ok proposal-id)
  )
)

(define-public (vote-on-proposal (proposal-id uint) (vote-for bool))
  (let
    (
      (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) err-not-found))
      (current-block stacks-block-height)
      (voter-voting-power (default-to { power: u0, last-updated-block: u0 }
        (map-get? voter-power { voter: tx-sender })))
      (voting-power (get power voter-voting-power))
      (existing-vote (map-get? voter-records { proposal-id: proposal-id, voter: tx-sender }))
    )
    (asserts! (is-none existing-vote) err-already-voted)
    (asserts! (<= current-block (get expires-block proposal)) err-proposal-expired)
    (asserts! (> voting-power u0) err-insufficient-voting-power)
    (map-set voter-records
      { proposal-id: proposal-id, voter: tx-sender }
      { voting-power: voting-power, vote-for: vote-for, voted-block: current-block })
    (map-set proposals
      { proposal-id: proposal-id }
      (merge proposal {
        votes-for: (if vote-for (+ (get votes-for proposal) voting-power) (get votes-for proposal)),
        votes-against: (if vote-for (get votes-against proposal) (+ (get votes-against proposal) voting-power))
      }))
    (ok true)
  )
)

(define-public (update-voting-power (voter principal) (power uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (ok (map-set voter-power
      { voter: voter }
      { power: power, last-updated-block: stacks-block-height }))
  )
)

(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals { proposal-id: proposal-id })
)

(define-read-only (get-vote-record (proposal-id uint) (voter principal))
  (map-get? voter-records { proposal-id: proposal-id, voter: voter })
)

(define-read-only (get-voting-power (voter principal))
  (map-get? voter-power { voter: voter })
)

(define-read-only (check-proposal-status (proposal-id uint))
  (let
    (
      (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) 
        (ok { passed: false, expired: false, executed: false })))
      (current-block stacks-block-height)
      (total-votes (+ (get votes-for proposal) (get votes-against proposal)))
      (passed (and 
        (> (get votes-for proposal) (get votes-against proposal))
        (>= (* (get votes-for proposal) u100) (* total-votes (var-get quorum-percentage)))))
    )
    (ok {
      passed: passed,
      expired: (> current-block (get expires-block proposal)),
      executed: (get executed proposal)
    })
  )
)

(define-read-only (get-governance-params)
  {
    min-voting-power: (var-get min-voting-power),
    quorum-percentage: (var-get quorum-percentage),
    proposal-duration: (var-get proposal-duration),
    total-proposals: (var-get total-proposals)
  }
)
