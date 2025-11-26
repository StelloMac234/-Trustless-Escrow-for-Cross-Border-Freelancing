(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-invalid-state (err u103))
(define-constant err-insufficient-funds (err u104))
(define-constant err-expired (err u105))
(define-constant err-not-expired (err u106))
(define-constant err-already-exists (err u107))
(define-constant err-unsupported-currency (err u108))
(define-constant err-invalid-exchange-rate (err u109))
(define-constant err-currency-mismatch (err u110))
(define-constant err-stale-exchange-rate (err u111))
(define-constant err-subscription-not-found (err u112))
(define-constant err-subscription-inactive (err u113))
(define-constant err-payment-not-due (err u114))
(define-constant err-insufficient-balance (err u115))
(define-constant err-subscription-paused (err u116))

(define-data-var next-escrow-id uint u1)
(define-data-var dispute-fee uint u1000000)
(define-data-var contract-fee-rate uint u250)
(define-data-var next-template-id uint u1)
(define-data-var next-milestone-escrow-id uint u1)
(define-data-var next-currency-id uint u1)
(define-data-var next-subscription-id uint u1)

(define-map escrows
  { escrow-id: uint }
  {
    client: principal,
    freelancer: principal,
    amount: uint,
    deadline: uint,
    status: (string-ascii 20),
    work-description: (string-ascii 500),
    created-at: uint,
    completed-at: (optional uint),
    disputed-at: (optional uint),
    arbiter: (optional principal)
  }
)

(define-map disputes
  { escrow-id: uint }
  {
    reason: (string-ascii 500),
    evidence-client: (string-ascii 500),
    evidence-freelancer: (string-ascii 500),
    resolution: (optional (string-ascii 20)),
    resolved-at: (optional uint)
  }
)

(define-map user-ratings
  { user: principal }
  {
    total-score: uint,
    review-count: uint,
    completed-escrows: uint
  }
)

(define-map escrow-templates
  { template-id: uint }
  {
    name: (string-ascii 50),
    description: (string-ascii 200),
    category: (string-ascii 30),
    default-duration: uint,
    min-amount: uint,
    max-amount: uint,
    terms: (string-ascii 300),
    creator: principal,
    created-at: uint,
    is-active: bool
  }
)

(define-map milestone-escrows
  { milestone-escrow-id: uint }
  {
    client: principal,
    freelancer: principal,
    total-amount: uint,
    deadline: uint,
    work-description: (string-ascii 500),
    created-at: uint,
    milestone-count: uint,
    completed-milestones: uint,
    status: (string-ascii 20)
  }
)

(define-map milestones
  { milestone-escrow-id: uint, milestone-index: uint }
  {
    description: (string-ascii 200),
    amount: uint,
    status: (string-ascii 20),
    completed-at: (optional uint),
    approved-at: (optional uint)
  }
)

(define-map supported-currencies
  { currency-id: uint }
  {
    symbol: (string-ascii 10),
    name: (string-ascii 50),
    contract-address: (optional principal),
    decimals: uint,
    is-active: bool,
    added-at: uint
  }
)

(define-map currency-exchange-rates
  { base-currency: uint, quote-currency: uint }
  {
    rate: uint,
    last-updated: uint,
    oracle: principal
  }
)

(define-map multi-currency-escrows
  { escrow-id: uint }
  {
    currency-id: uint,
    original-amount: uint,
    stx-equivalent: uint
  }
)

(define-map subscriptions
  { subscription-id: uint }
  {
    client: principal,
    freelancer: principal,
    payment-amount: uint,
    billing-cycle: uint,
    total-deposited: uint,
    total-paid: uint,
    last-payment-block: uint,
    next-payment-block: uint,
    subscription-start: uint,
    status: (string-ascii 20),
    service-description: (string-ascii 300),
    payments-made: uint,
    auto-renew: bool
  }
)

(define-map subscription-payments
  { subscription-id: uint, payment-index: uint }
  {
    amount: uint,
    paid-at: uint,
    period-start: uint,
    period-end: uint
  }
)

(define-public (create-escrow (freelancer principal) (amount uint) (deadline uint) (work-description (string-ascii 500)))
  (let
    (
      (escrow-id (var-get next-escrow-id))
      (current-height stacks-block-height)
    )
    (asserts! (> amount u0) err-insufficient-funds)
    (asserts! (> deadline current-height) err-invalid-state)
    (asserts! (not (is-eq tx-sender freelancer)) err-invalid-state)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (map-set escrows
      { escrow-id: escrow-id }
      {
        client: tx-sender,
        freelancer: freelancer,
        amount: amount,
        deadline: deadline,
        status: "active",
        work-description: work-description,
        created-at: current-height,
        completed-at: none,
        disputed-at: none,
        arbiter: none
      }
    )
    (var-set next-escrow-id (+ escrow-id u1))
    (ok escrow-id)
  )
)

(define-public (complete-work (escrow-id uint))
  (let
    (
      (escrow (unwrap! (map-get? escrows { escrow-id: escrow-id }) err-not-found))
      (current-height stacks-block-height)
    )
    (asserts! (is-eq tx-sender (get freelancer escrow)) err-unauthorized)
    (asserts! (is-eq (get status escrow) "active") err-invalid-state)
    (asserts! (< current-height (get deadline escrow)) err-expired)
    
    (map-set escrows
      { escrow-id: escrow-id }
      (merge escrow {
        status: "completed",
        completed-at: (some current-height)
      })
    )
    (ok true)
  )
)

(define-public (approve-work (escrow-id uint))
  (let
    (
      (escrow (unwrap! (map-get? escrows { escrow-id: escrow-id }) err-not-found))
      (contract-fee (/ (* (get amount escrow) (var-get contract-fee-rate)) u10000))
      (freelancer-amount (- (get amount escrow) contract-fee))
    )
    (asserts! (is-eq tx-sender (get client escrow)) err-unauthorized)
    (asserts! (is-eq (get status escrow) "completed") err-invalid-state)
    
    (try! (as-contract (stx-transfer? freelancer-amount tx-sender (get freelancer escrow))))
    (try! (as-contract (stx-transfer? contract-fee tx-sender contract-owner)))
    
    (map-set escrows
      { escrow-id: escrow-id }
      (merge escrow { status: "paid" })
    )
    
    (update-user-rating (get freelancer escrow) u5)
    (update-user-rating (get client escrow) u5)
    (ok true)
  )
)

(define-public (dispute-escrow (escrow-id uint) (reason (string-ascii 500)))
  (let
    (
      (escrow (unwrap! (map-get? escrows { escrow-id: escrow-id }) err-not-found))
      (current-height stacks-block-height)
    )
    (asserts! (or (is-eq tx-sender (get client escrow)) (is-eq tx-sender (get freelancer escrow))) err-unauthorized)
    (asserts! (or (is-eq (get status escrow) "active") (is-eq (get status escrow) "completed")) err-invalid-state)
    
    (try! (stx-transfer? (var-get dispute-fee) tx-sender (as-contract tx-sender)))
    
    (map-set escrows
      { escrow-id: escrow-id }
      (merge escrow {
        status: "disputed",
        disputed-at: (some current-height)
      })
    )
    
    (map-set disputes
      { escrow-id: escrow-id }
      {
        reason: reason,
        evidence-client: "",
        evidence-freelancer: "",
        resolution: none,
        resolved-at: none
      }
    )
    (ok true)
  )
)

(define-public (submit-evidence (escrow-id uint) (evidence (string-ascii 500)))
  (let
    (
      (escrow (unwrap! (map-get? escrows { escrow-id: escrow-id }) err-not-found))
      (dispute-data (unwrap! (map-get? disputes { escrow-id: escrow-id }) err-not-found))
    )
    (asserts! (is-eq (get status escrow) "disputed") err-invalid-state)
    (asserts! (or (is-eq tx-sender (get client escrow)) (is-eq tx-sender (get freelancer escrow))) err-unauthorized)
    
    (if (is-eq tx-sender (get client escrow))
      (map-set disputes
        { escrow-id: escrow-id }
        (merge dispute-data { evidence-client: evidence })
      )
      (map-set disputes
        { escrow-id: escrow-id }
        (merge dispute-data { evidence-freelancer: evidence })
      )
    )
    (ok true)
  )
)

(define-public (resolve-dispute (escrow-id uint) (resolution (string-ascii 20)))
  (let
    (
      (escrow (unwrap! (map-get? escrows { escrow-id: escrow-id }) err-not-found))
      (dispute-data (unwrap! (map-get? disputes { escrow-id: escrow-id }) err-not-found))
      (current-height stacks-block-height)
      (contract-fee (/ (* (get amount escrow) (var-get contract-fee-rate)) u10000))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (is-eq (get status escrow) "disputed") err-invalid-state)
    
    (if (is-eq resolution "client")
      (begin
        (try! (as-contract (stx-transfer? (get amount escrow) tx-sender (get client escrow))))
        (try! (as-contract (stx-transfer? (var-get dispute-fee) tx-sender (get client escrow))))
      )
      (if (is-eq resolution "freelancer")
        (begin
          (try! (as-contract (stx-transfer? (- (get amount escrow) contract-fee) tx-sender (get freelancer escrow))))
          (try! (as-contract (stx-transfer? contract-fee tx-sender contract-owner)))
          (try! (as-contract (stx-transfer? (var-get dispute-fee) tx-sender (get freelancer escrow))))
        )
        (begin
          (try! (as-contract (stx-transfer? (/ (get amount escrow) u2) tx-sender (get client escrow))))
          (try! (as-contract (stx-transfer? (/ (get amount escrow) u2) tx-sender (get freelancer escrow))))
        )
      )
    )
    
    (map-set escrows
      { escrow-id: escrow-id }
      (merge escrow { status: "resolved" })
    )
    
    (map-set disputes
      { escrow-id: escrow-id }
      (merge dispute-data {
        resolution: (some resolution),
        resolved-at: (some current-height)
      })
    )
    (ok true)
  )
)

(define-public (cancel-escrow (escrow-id uint))
  (let
    (
      (escrow (unwrap! (map-get? escrows { escrow-id: escrow-id }) err-not-found))
      (current-height stacks-block-height)
    )
    (asserts! (is-eq tx-sender (get client escrow)) err-unauthorized)
    (asserts! (is-eq (get status escrow) "active") err-invalid-state)
    (asserts! (> current-height (get deadline escrow)) err-not-expired)
    
    (try! (as-contract (stx-transfer? (get amount escrow) tx-sender (get client escrow))))
    
    (map-set escrows
      { escrow-id: escrow-id }
      (merge escrow { status: "cancelled" })
    )
    (ok true)
  )
)

(define-public (claim-expired-escrow (escrow-id uint))
  (let
    (
      (escrow (unwrap! (map-get? escrows { escrow-id: escrow-id }) err-not-found))
      (current-height stacks-block-height)
      (contract-fee (/ (* (get amount escrow) (var-get contract-fee-rate)) u10000))
      (freelancer-amount (- (get amount escrow) contract-fee))
    )
    (asserts! (is-eq tx-sender (get freelancer escrow)) err-unauthorized)
    (asserts! (is-eq (get status escrow) "completed") err-invalid-state)
    (asserts! (> current-height (+ (unwrap! (get completed-at escrow) err-invalid-state) u1008)) err-not-expired)
    
    (try! (as-contract (stx-transfer? freelancer-amount tx-sender (get freelancer escrow))))
    (try! (as-contract (stx-transfer? contract-fee tx-sender contract-owner)))
    
    (map-set escrows
      { escrow-id: escrow-id }
      (merge escrow { status: "auto-paid" })
    )
    
    (update-user-rating (get freelancer escrow) u4)
    (ok true)
  )
)

(define-public (set-dispute-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set dispute-fee new-fee)
    (ok true)
  )
)

(define-public (set-contract-fee-rate (new-rate uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-rate u1000) err-invalid-state)
    (var-set contract-fee-rate new-rate)
    (ok true)
  )
)

(define-private (update-user-rating (user principal) (score uint))
  (let
    (
      (current-rating (default-to { total-score: u0, review-count: u0, completed-escrows: u0 } 
                                 (map-get? user-ratings { user: user })))
    )
    (map-set user-ratings
      { user: user }
      {
        total-score: (+ (get total-score current-rating) score),
        review-count: (+ (get review-count current-rating) u1),
        completed-escrows: (+ (get completed-escrows current-rating) u1)
      }
    )
  )
)

(define-read-only (get-escrow (escrow-id uint))
  (map-get? escrows { escrow-id: escrow-id })
)

(define-read-only (get-dispute (escrow-id uint))
  (map-get? disputes { escrow-id: escrow-id })
)

(define-read-only (get-user-rating (user principal))
  (map-get? user-ratings { user: user })
)

(define-read-only (get-user-average-rating (user principal))
  (let
    (
      (rating-data (map-get? user-ratings { user: user }))
    )
    (match rating-data
      data (if (> (get review-count data) u0)
             (some (/ (get total-score data) (get review-count data)))
             none)
      none
    )
  )
)

(define-read-only (get-contract-balance)
  (stx-get-balance (as-contract tx-sender))
)

(define-read-only (get-dispute-fee)
  (var-get dispute-fee)
)

(define-read-only (get-contract-fee-rate)
  (var-get contract-fee-rate)
)

(define-read-only (get-next-escrow-id)
  (var-get next-escrow-id)
)

(define-public (create-template (name (string-ascii 50)) (description (string-ascii 200)) (category (string-ascii 30)) (default-duration uint) (min-amount uint) (max-amount uint) (terms (string-ascii 300)))
  (let
    (
      (template-id (var-get next-template-id))
      (current-height stacks-block-height)
    )
    (asserts! (> (len name) u0) err-invalid-state)
    (asserts! (> default-duration u0) err-invalid-state)
    (asserts! (> max-amount min-amount) err-invalid-state)
    (asserts! (> min-amount u0) err-invalid-state)
    
    (map-set escrow-templates
      { template-id: template-id }
      {
        name: name,
        description: description,
        category: category,
        default-duration: default-duration,
        min-amount: min-amount,
        max-amount: max-amount,
        terms: terms,
        creator: tx-sender,
        created-at: current-height,
        is-active: true
      }
    )
    (var-set next-template-id (+ template-id u1))
    (ok template-id)
  )
)

(define-public (create-escrow-from-template (template-id uint) (freelancer principal) (amount uint) (custom-deadline (optional uint)) (additional-description (string-ascii 200)))
  (let
    (
      (template (unwrap! (map-get? escrow-templates { template-id: template-id }) err-not-found))
      (escrow-id (var-get next-escrow-id))
      (current-height stacks-block-height)
      (deadline (default-to (+ current-height (get default-duration template)) custom-deadline))
      (work-description (concat (get terms template) additional-description))
    )
    (asserts! (get is-active template) err-invalid-state)
    (asserts! (>= amount (get min-amount template)) err-insufficient-funds)
    (asserts! (<= amount (get max-amount template)) err-invalid-state)
    (asserts! (> deadline current-height) err-invalid-state)
    (asserts! (not (is-eq tx-sender freelancer)) err-invalid-state)
    
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (map-set escrows
      { escrow-id: escrow-id }
      {
        client: tx-sender,
        freelancer: freelancer,
        amount: amount,
        deadline: deadline,
        status: "active",
        work-description: work-description,
        created-at: current-height,
        completed-at: none,
        disputed-at: none,
        arbiter: none
      }
    )
    (var-set next-escrow-id (+ escrow-id u1))
    (ok escrow-id)
  )
)

(define-public (toggle-template-status (template-id uint))
  (let
    (
      (template (unwrap! (map-get? escrow-templates { template-id: template-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get creator template)) err-unauthorized)
    (map-set escrow-templates
      { template-id: template-id }
      (merge template { is-active: (not (get is-active template)) })
    )
    (ok true)
  )
)

(define-read-only (get-template (template-id uint))
  (map-get? escrow-templates { template-id: template-id })
)

(define-read-only (get-next-template-id)
  (var-get next-template-id)
)

(define-public (create-milestone-escrow (freelancer principal) (total-amount uint) (deadline uint) (work-description (string-ascii 500)))
  (let
    (
      (milestone-escrow-id (var-get next-milestone-escrow-id))
      (current-height stacks-block-height)
    )
    (asserts! (> total-amount u0) err-insufficient-funds)
    (asserts! (> deadline current-height) err-invalid-state)
    (asserts! (not (is-eq tx-sender freelancer)) err-invalid-state)
    
    (try! (stx-transfer? total-amount tx-sender (as-contract tx-sender)))
    
    (map-set milestone-escrows
      { milestone-escrow-id: milestone-escrow-id }
      {
        client: tx-sender,
        freelancer: freelancer,
        total-amount: total-amount,
        deadline: deadline,
        work-description: work-description,
        created-at: current-height,
        milestone-count: u0,
        completed-milestones: u0,
        status: "setup"
      }
    )
    
    (var-set next-milestone-escrow-id (+ milestone-escrow-id u1))
    (ok milestone-escrow-id)
  )
)

(define-public (add-milestone (milestone-escrow-id uint) (description (string-ascii 200)) (amount uint))
  (let
    (
      (milestone-escrow (unwrap! (map-get? milestone-escrows { milestone-escrow-id: milestone-escrow-id }) err-not-found))
      (current-milestone-count (get milestone-count milestone-escrow))
    )
    (asserts! (is-eq tx-sender (get client milestone-escrow)) err-unauthorized)
    (asserts! (is-eq (get status milestone-escrow) "setup") err-invalid-state)
    (asserts! (> amount u0) err-insufficient-funds)
    (asserts! (< current-milestone-count u10) err-invalid-state)
    
    (map-set milestones
      { milestone-escrow-id: milestone-escrow-id, milestone-index: current-milestone-count }
      {
        description: description,
        amount: amount,
        status: "pending",
        completed-at: none,
        approved-at: none
      }
    )
    
    (map-set milestone-escrows
      { milestone-escrow-id: milestone-escrow-id }
      (merge milestone-escrow { milestone-count: (+ current-milestone-count u1) })
    )
    (ok true)
  )
)

(define-public (activate-milestone-escrow (milestone-escrow-id uint))
  (let
    (
      (milestone-escrow (unwrap! (map-get? milestone-escrows { milestone-escrow-id: milestone-escrow-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get client milestone-escrow)) err-unauthorized)
    (asserts! (is-eq (get status milestone-escrow) "setup") err-invalid-state)
    (asserts! (> (get milestone-count milestone-escrow) u0) err-invalid-state)
    
    (map-set milestone-escrows
      { milestone-escrow-id: milestone-escrow-id }
      (merge milestone-escrow { status: "active" })
    )
    (ok true)
  )
)

(define-public (complete-milestone (milestone-escrow-id uint) (milestone-index uint))
  (let
    (
      (milestone-escrow (unwrap! (map-get? milestone-escrows { milestone-escrow-id: milestone-escrow-id }) err-not-found))
      (milestone (unwrap! (map-get? milestones { milestone-escrow-id: milestone-escrow-id, milestone-index: milestone-index }) err-not-found))
      (current-height stacks-block-height)
    )
    (asserts! (is-eq tx-sender (get freelancer milestone-escrow)) err-unauthorized)
    (asserts! (is-eq (get status milestone-escrow) "active") err-invalid-state)
    (asserts! (is-eq (get status milestone) "pending") err-invalid-state)
    (asserts! (< current-height (get deadline milestone-escrow)) err-expired)
    
    (map-set milestones
      { milestone-escrow-id: milestone-escrow-id, milestone-index: milestone-index }
      (merge milestone {
        status: "completed",
        completed-at: (some current-height)
      })
    )
    (ok true)
  )
)

(define-public (approve-milestone (milestone-escrow-id uint) (milestone-index uint))
  (let
    (
      (milestone-escrow (unwrap! (map-get? milestone-escrows { milestone-escrow-id: milestone-escrow-id }) err-not-found))
      (milestone (unwrap! (map-get? milestones { milestone-escrow-id: milestone-escrow-id, milestone-index: milestone-index }) err-not-found))
      (current-height stacks-block-height)
      (milestone-amount (get amount milestone))
      (contract-fee (/ (* milestone-amount (var-get contract-fee-rate)) u10000))
      (freelancer-amount (- milestone-amount contract-fee))
    )
    (asserts! (is-eq tx-sender (get client milestone-escrow)) err-unauthorized)
    (asserts! (is-eq (get status milestone) "completed") err-invalid-state)
    
    (try! (as-contract (stx-transfer? freelancer-amount tx-sender (get freelancer milestone-escrow))))
    (try! (as-contract (stx-transfer? contract-fee tx-sender contract-owner)))
    
    (map-set milestones
      { milestone-escrow-id: milestone-escrow-id, milestone-index: milestone-index }
      (merge milestone {
        status: "paid",
        approved-at: (some current-height)
      })
    )
    
    (let
      (
        (updated-completed (+ (get completed-milestones milestone-escrow) u1))
      )
      (map-set milestone-escrows
        { milestone-escrow-id: milestone-escrow-id }
        (merge milestone-escrow { completed-milestones: updated-completed })
      )
      
      (if (is-eq updated-completed (get milestone-count milestone-escrow))
        (begin
          (map-set milestone-escrows
            { milestone-escrow-id: milestone-escrow-id }
            (merge milestone-escrow { status: "completed" })
          )
          (update-user-rating (get freelancer milestone-escrow) u5)
          (update-user-rating (get client milestone-escrow) u5)
        )
        true
      )
    )
    (ok true)
  )
)

(define-public (cancel-milestone-escrow (milestone-escrow-id uint))
  (let
    (
      (milestone-escrow (unwrap! (map-get? milestone-escrows { milestone-escrow-id: milestone-escrow-id }) err-not-found))
      (current-height stacks-block-height)
    )
    (asserts! (is-eq tx-sender (get client milestone-escrow)) err-unauthorized)
    (asserts! (is-eq (get status milestone-escrow) "active") err-invalid-state)
    (asserts! (> current-height (get deadline milestone-escrow)) err-not-expired)
    
    (map-set milestone-escrows
      { milestone-escrow-id: milestone-escrow-id }
      (merge milestone-escrow { status: "cancelled" })
    )
    (ok true)
  )
)

(define-read-only (get-milestone-escrow (milestone-escrow-id uint))
  (map-get? milestone-escrows { milestone-escrow-id: milestone-escrow-id })
)

(define-read-only (get-milestone (milestone-escrow-id uint) (milestone-index uint))
  (map-get? milestones { milestone-escrow-id: milestone-escrow-id, milestone-index: milestone-index })
)

(define-read-only (get-next-milestone-escrow-id)
  (var-get next-milestone-escrow-id)
)

(define-public (add-supported-currency 
  (symbol (string-ascii 10))
  (name (string-ascii 50))
  (contract-address (optional principal))
  (decimals uint))
  (let
    (
      (currency-id (var-get next-currency-id))
      (current-height stacks-block-height)
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (> (len symbol) u0) err-invalid-state)
    (asserts! (> (len name) u0) err-invalid-state)
    (asserts! (<= decimals u18) err-invalid-state)
    
    (map-set supported-currencies
      { currency-id: currency-id }
      {
        symbol: symbol,
        name: name,
        contract-address: contract-address,
        decimals: decimals,
        is-active: true,
        added-at: current-height
      }
    )
    (var-set next-currency-id (+ currency-id u1))
    (ok currency-id)
  )
)

(define-public (toggle-currency-status (currency-id uint))
  (let
    (
      (currency (unwrap! (map-get? supported-currencies { currency-id: currency-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set supported-currencies
      { currency-id: currency-id }
      (merge currency { is-active: (not (get is-active currency)) })
    )
    (ok true)
  )
)

(define-public (set-exchange-rate 
  (base-currency uint)
  (quote-currency uint)
  (rate uint))
  (let
    (
      (current-height stacks-block-height)
      (base-curr (unwrap! (map-get? supported-currencies { currency-id: base-currency }) err-unsupported-currency))
      (quote-curr (unwrap! (map-get? supported-currencies { currency-id: quote-currency }) err-unsupported-currency))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (get is-active base-curr) err-unsupported-currency)
    (asserts! (get is-active quote-curr) err-unsupported-currency)
    (asserts! (> rate u0) err-invalid-exchange-rate)
    (asserts! (not (is-eq base-currency quote-currency)) err-invalid-state)
    
    (map-set currency-exchange-rates
      { base-currency: base-currency, quote-currency: quote-currency }
      {
        rate: rate,
        last-updated: current-height,
        oracle: tx-sender
      }
    )
    (ok true)
  )
)

(define-public (create-multi-currency-escrow 
  (freelancer principal)
  (amount uint)
  (currency-id uint)
  (deadline uint)
  (work-description (string-ascii 500)))
  (let
    (
      (escrow-id (var-get next-escrow-id))
      (current-height stacks-block-height)
      (currency (unwrap! (map-get? supported-currencies { currency-id: currency-id }) err-unsupported-currency))
      (stx-rate-data (map-get? currency-exchange-rates { base-currency: currency-id, quote-currency: u0 }))
      (stx-equivalent (match stx-rate-data
        rate-info (/ (* amount (get rate rate-info)) (pow u10 (get decimals currency)))
        amount))
    )
    (asserts! (get is-active currency) err-unsupported-currency)
    (asserts! (> amount u0) err-insufficient-funds)
    (asserts! (> deadline current-height) err-invalid-state)
    (asserts! (not (is-eq tx-sender freelancer)) err-invalid-state)
    
    (if (is-eq currency-id u0)
      (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
      (begin
        (try! (stx-transfer? stx-equivalent tx-sender (as-contract tx-sender)))
        (map-set multi-currency-escrows
          { escrow-id: escrow-id }
          {
            currency-id: currency-id,
            original-amount: amount,
            stx-equivalent: stx-equivalent
          }
        )
      )
    )
    
    (map-set escrows
      { escrow-id: escrow-id }
      {
        client: tx-sender,
        freelancer: freelancer,
        amount: stx-equivalent,
        deadline: deadline,
        status: "active",
        work-description: work-description,
        created-at: current-height,
        completed-at: none,
        disputed-at: none,
        arbiter: none
      }
    )
    (var-set next-escrow-id (+ escrow-id u1))
    (ok escrow-id)
  )
)

(define-read-only (get-supported-currency (currency-id uint))
  (map-get? supported-currencies { currency-id: currency-id })
)

(define-read-only (get-exchange-rate (base-currency uint) (quote-currency uint))
  (map-get? currency-exchange-rates { base-currency: base-currency, quote-currency: quote-currency })
)

(define-read-only (get-multi-currency-escrow-info (escrow-id uint))
  (map-get? multi-currency-escrows { escrow-id: escrow-id })
)

(define-read-only (convert-currency-amount 
  (amount uint)
  (from-currency uint)
  (to-currency uint))
  (let
    (
      (from-curr (unwrap! (map-get? supported-currencies { currency-id: from-currency }) err-unsupported-currency))
      (to-curr (unwrap! (map-get? supported-currencies { currency-id: to-currency }) err-unsupported-currency))
      (rate-data (map-get? currency-exchange-rates { base-currency: from-currency, quote-currency: to-currency }))
    )
    (match rate-data
      rate-info
        (let
          (
            (current-height stacks-block-height)
            (rate-age (- current-height (get last-updated rate-info)))
          )
          (if (< rate-age u144)
            (ok (/ (* amount (get rate rate-info)) (pow u10 (get decimals from-curr))))
            err-stale-exchange-rate
          )
        )
      err-invalid-exchange-rate
    )
  )
)

(define-read-only (get-escrow-currency-info (escrow-id uint))
  (let
    (
      (escrow (map-get? escrows { escrow-id: escrow-id }))
      (multi-currency-info (map-get? multi-currency-escrows { escrow-id: escrow-id }))
    )
    (match escrow
      escrow-data
        (match multi-currency-info
          currency-info
            (some {
              escrow-amount: (get amount escrow-data),
              currency-id: (get currency-id currency-info),
              original-amount: (get original-amount currency-info),
              stx-equivalent: (get stx-equivalent currency-info)
            })
          (some {
            escrow-amount: (get amount escrow-data),
            currency-id: u0,
            original-amount: (get amount escrow-data),
            stx-equivalent: (get amount escrow-data)
          })
        )
      none
    )
  )
)

(define-read-only (get-next-currency-id)
  (var-get next-currency-id)
)

(define-read-only (is-exchange-rate-fresh (base-currency uint) (quote-currency uint) (max-age uint))
  (let
    (
      (rate-data (map-get? currency-exchange-rates { base-currency: base-currency, quote-currency: quote-currency }))
      (current-height stacks-block-height)
    )
    (match rate-data
      rate-info (< (- current-height (get last-updated rate-info)) max-age)
      false
    )
  )
)

(define-public (create-subscription
  (freelancer principal)
  (payment-amount uint)
  (billing-cycle uint)
  (initial-deposit uint)
  (service-description (string-ascii 300)))
  (let
    (
      (subscription-id (var-get next-subscription-id))
      (current-height stacks-block-height)
      (next-payment (+ current-height billing-cycle))
    )
    (asserts! (> payment-amount u0) err-insufficient-funds)
    (asserts! (> billing-cycle u0) err-invalid-state)
    (asserts! (>= initial-deposit payment-amount) err-insufficient-balance)
    (asserts! (not (is-eq tx-sender freelancer)) err-invalid-state)
    
    (try! (stx-transfer? initial-deposit tx-sender (as-contract tx-sender)))
    
    (map-set subscriptions
      { subscription-id: subscription-id }
      {
        client: tx-sender,
        freelancer: freelancer,
        payment-amount: payment-amount,
        billing-cycle: billing-cycle,
        total-deposited: initial-deposit,
        total-paid: u0,
        last-payment-block: u0,
        next-payment-block: next-payment,
        subscription-start: current-height,
        status: "active",
        service-description: service-description,
        payments-made: u0,
        auto-renew: false
      }
    )
    
    (var-set next-subscription-id (+ subscription-id u1))
    (ok subscription-id)
  )
)

(define-public (deposit-to-subscription (subscription-id uint) (amount uint))
  (let
    (
      (subscription (unwrap! (map-get? subscriptions { subscription-id: subscription-id }) err-subscription-not-found))
    )
    (asserts! (is-eq tx-sender (get client subscription)) err-unauthorized)
    (asserts! (or (is-eq (get status subscription) "active") (is-eq (get status subscription) "paused")) err-subscription-inactive)
    (asserts! (> amount u0) err-insufficient-funds)
    
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    (map-set subscriptions
      { subscription-id: subscription-id }
      (merge subscription { total-deposited: (+ (get total-deposited subscription) amount) })
    )
    (ok true)
  )
)

(define-public (claim-subscription-payment (subscription-id uint))
  (let
    (
      (subscription (unwrap! (map-get? subscriptions { subscription-id: subscription-id }) err-subscription-not-found))
      (current-height stacks-block-height)
      (payment-amount (get payment-amount subscription))
      (contract-fee (/ (* payment-amount (var-get contract-fee-rate)) u10000))
      (freelancer-amount (- payment-amount contract-fee))
      (available-balance (- (get total-deposited subscription) (get total-paid subscription)))
      (payment-index (get payments-made subscription))
    )
    (asserts! (is-eq tx-sender (get freelancer subscription)) err-unauthorized)
    (asserts! (is-eq (get status subscription) "active") err-subscription-inactive)
    (asserts! (>= current-height (get next-payment-block subscription)) err-payment-not-due)
    (asserts! (>= available-balance payment-amount) err-insufficient-balance)
    
    (try! (as-contract (stx-transfer? freelancer-amount tx-sender (get freelancer subscription))))
    (try! (as-contract (stx-transfer? contract-fee tx-sender contract-owner)))
    
    (map-set subscription-payments
      { subscription-id: subscription-id, payment-index: payment-index }
      {
        amount: payment-amount,
        paid-at: current-height,
        period-start: (get next-payment-block subscription),
        period-end: (+ (get next-payment-block subscription) (get billing-cycle subscription))
      }
    )
    
    (map-set subscriptions
      { subscription-id: subscription-id }
      (merge subscription {
        total-paid: (+ (get total-paid subscription) payment-amount),
        last-payment-block: current-height,
        next-payment-block: (+ (get next-payment-block subscription) (get billing-cycle subscription)),
        payments-made: (+ payment-index u1)
      })
    )
    
    (update-user-rating (get freelancer subscription) u5)
    (ok true)
  )
)

(define-public (pause-subscription (subscription-id uint))
  (let
    (
      (subscription (unwrap! (map-get? subscriptions { subscription-id: subscription-id }) err-subscription-not-found))
    )
    (asserts! (is-eq tx-sender (get client subscription)) err-unauthorized)
    (asserts! (is-eq (get status subscription) "active") err-invalid-state)
    
    (map-set subscriptions
      { subscription-id: subscription-id }
      (merge subscription { status: "paused" })
    )
    (ok true)
  )
)

(define-public (resume-subscription (subscription-id uint))
  (let
    (
      (subscription (unwrap! (map-get? subscriptions { subscription-id: subscription-id }) err-subscription-not-found))
      (current-height stacks-block-height)
    )
    (asserts! (is-eq tx-sender (get client subscription)) err-unauthorized)
    (asserts! (is-eq (get status subscription) "paused") err-subscription-paused)
    
    (map-set subscriptions
      { subscription-id: subscription-id }
      (merge subscription {
        status: "active",
        next-payment-block: (+ current-height (get billing-cycle subscription))
      })
    )
    (ok true)
  )
)

(define-public (cancel-subscription (subscription-id uint))
  (let
    (
      (subscription (unwrap! (map-get? subscriptions { subscription-id: subscription-id }) err-subscription-not-found))
      (remaining-balance (- (get total-deposited subscription) (get total-paid subscription)))
    )
    (asserts! (is-eq tx-sender (get client subscription)) err-unauthorized)
    (asserts! (or (is-eq (get status subscription) "active") (is-eq (get status subscription) "paused")) err-subscription-inactive)
    
    (if (> remaining-balance u0)
      (try! (as-contract (stx-transfer? remaining-balance tx-sender (get client subscription))))
      true
    )
    
    (map-set subscriptions
      { subscription-id: subscription-id }
      (merge subscription { status: "cancelled" })
    )
    (ok true)
  )
)

(define-public (toggle-auto-renew (subscription-id uint))
  (let
    (
      (subscription (unwrap! (map-get? subscriptions { subscription-id: subscription-id }) err-subscription-not-found))
    )
    (asserts! (is-eq tx-sender (get client subscription)) err-unauthorized)
    (asserts! (or (is-eq (get status subscription) "active") (is-eq (get status subscription) "paused")) err-subscription-inactive)
    
    (map-set subscriptions
      { subscription-id: subscription-id }
      (merge subscription { auto-renew: (not (get auto-renew subscription)) })
    )
    (ok true)
  )
)

(define-read-only (get-subscription (subscription-id uint))
  (map-get? subscriptions { subscription-id: subscription-id })
)

(define-read-only (get-subscription-payment (subscription-id uint) (payment-index uint))
  (map-get? subscription-payments { subscription-id: subscription-id, payment-index: payment-index })
)

(define-read-only (get-subscription-balance (subscription-id uint))
  (match (map-get? subscriptions { subscription-id: subscription-id })
    subscription
      (ok {
        total-deposited: (get total-deposited subscription),
        total-paid: (get total-paid subscription),
        available-balance: (- (get total-deposited subscription) (get total-paid subscription)),
        payments-remaining: (/ (- (get total-deposited subscription) (get total-paid subscription)) (get payment-amount subscription))
      })
    err-subscription-not-found
  )
)

(define-read-only (is-payment-due (subscription-id uint))
  (match (map-get? subscriptions { subscription-id: subscription-id })
    subscription
      (let
        (
          (current-height stacks-block-height)
          (next-payment (get next-payment-block subscription))
          (available-balance (- (get total-deposited subscription) (get total-paid subscription)))
        )
        (and
          (is-eq (get status subscription) "active")
          (>= current-height next-payment)
          (>= available-balance (get payment-amount subscription))
        )
      )
    false
  )
)

(define-read-only (get-subscription-status (subscription-id uint))
  (match (map-get? subscriptions { subscription-id: subscription-id })
    subscription
      (let
        (
          (current-height stacks-block-height)
          (available-balance (- (get total-deposited subscription) (get total-paid subscription)))
          (blocks-until-next (if (>= current-height (get next-payment-block subscription))
                               u0
                               (- (get next-payment-block subscription) current-height)))
        )
        (some {
          status: (get status subscription),
          payments-made: (get payments-made subscription),
          available-balance: available-balance,
          next-payment-in-blocks: blocks-until-next,
          is-payment-due: (is-payment-due subscription-id),
          auto-renew: (get auto-renew subscription)
        })
      )
    none
  )
)

(define-read-only (get-next-subscription-id)
  (var-get next-subscription-id)
)

(define-public (add-funds (escrow-id uint) (amount uint))
  (let
    (
      (escrow (unwrap! (map-get? escrows { escrow-id: escrow-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get client escrow)) err-unauthorized)
    (asserts! (is-eq (get status escrow) "active") err-invalid-state)
    (asserts! (> amount u0) err-insufficient-funds)
    
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    (map-set escrows
      { escrow-id: escrow-id }
      (merge escrow { amount: (+ (get amount escrow) amount) })
    )

    (match (map-get? multi-currency-escrows { escrow-id: escrow-id })
      mc-data
      (map-set multi-currency-escrows
        { escrow-id: escrow-id }
        (merge mc-data { stx-equivalent: (+ (get stx-equivalent mc-data) amount) })
      )
      true
    )
    
    (ok true)
  )
)
