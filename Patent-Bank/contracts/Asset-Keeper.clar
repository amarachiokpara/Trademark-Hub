;; Decentralized Intellectual Property Protection Registry Smart Contract

;; A comprehensive blockchain-based intellectual property management system that
;; enables secure registration, ownership tracking, licensing, and transfer of
;; digital and traditional IP assets including patents, trademarks, copyrights,
;; and trade secrets with automated expiration management and royalty distribution.

;; ERROR CONSTANTS

(define-constant ERR-UNAUTHORIZED-ACCESS (err u100))
(define-constant ERR-INTELLECTUAL-PROPERTY-NOT-FOUND (err u101))
(define-constant ERR-INTELLECTUAL-PROPERTY-ALREADY-EXISTS (err u102))
(define-constant ERR-INVALID-INPUT-PARAMETERS (err u103))
(define-constant ERR-INSUFFICIENT-PAYMENT-AMOUNT (err u104))
(define-constant ERR-INTELLECTUAL-PROPERTY-EXPIRED (err u105))
(define-constant ERR-INVALID-LICENSE-TERMS (err u106))
(define-constant ERR-PAYMENT-TRANSFER-FAILED (err u107))
(define-constant ERR-INTELLECTUAL-PROPERTY-SUSPENDED (err u108))
(define-constant ERR-DUPLICATE-REGISTRATION-ATTEMPT (err u109))

;; CONTRACT CONFIGURATION CONSTANTS
(define-constant contract-administrator tx-sender)
(define-constant intellectual-property-registration-fee u1000000) ;; 1 STX in microSTX
(define-constant minimum-license-duration u1000) ;; ~1 week in blocks
(define-constant maximum-royalty-percentage u10000) ;; 100% in basis points
(define-constant renewal-fee-discount-rate u2) ;; 50% discount for renewals

;; INTELLECTUAL PROPERTY TYPE CONSTANTS
(define-constant intellectual-property-type-patent u1)
(define-constant intellectual-property-type-trademark u2)
(define-constant intellectual-property-type-copyright u3)
(define-constant intellectual-property-type-trade-secret u4)

;; INTELLECTUAL PROPERTY STATUS CONSTANTS
(define-constant intellectual-property-status-active u1)
(define-constant intellectual-property-status-expired u2)
(define-constant intellectual-property-status-suspended u3)
(define-constant intellectual-property-status-pending u4)

;; LICENSE TYPE CONSTANTS

(define-constant license-type-exclusive u1)
(define-constant license-type-non-exclusive u2)

;; EXPIRATION DURATION CONSTANTS (in blocks)
(define-constant patent-expiration-duration u1051200) ;; ~20 years
(define-constant trademark-expiration-duration u525600) ;; ~10 years
(define-constant copyright-expiration-duration u3679200) ;; ~70 years
(define-constant trade-secret-expiration-duration u99999999) ;; Indefinite

;; CONTRACT STATE VARIABLES
(define-data-var next-intellectual-property-identifier uint u1)
(define-data-var total-contract-balance uint u0)
(define-data-var total-registered-intellectual-properties uint u0)
(define-data-var contract-is-paused bool false)
(define-data-var next-transfer-sequence-number uint u1)

;; CORE DATA STRUCTURES
;; Primary intellectual property registry
(define-map intellectual-property-registry
  { intellectual-property-id: uint }
  {
    current-owner: principal,
    intellectual-property-type: uint,
    intellectual-property-title: (string-ascii 256),
    intellectual-property-description: (string-ascii 1024),
    creation-block-height: uint,
    expiration-block-height: uint,
    current-status: uint,
    registration-block-height: uint,
    content-hash: (buff 32),
    last-updated-block: uint
  }
)

;; Ownership transfer history tracking
(define-map intellectual-property-ownership-history
  { intellectual-property-id: uint, transfer-sequence-number: uint }
  {
    previous-owner-address: principal,
    new-owner-address: principal,
    transfer-block-height: uint,
    transfer-identifier: uint
  }
)

;; License management system
(define-map intellectual-property-license-registry
  { intellectual-property-id: uint, licensee-address: principal }
  {
    license-type-classification: uint,
    license-start-block-height: uint,
    license-end-block-height: uint,
    royalty-rate-basis-points: uint,
    license-is-currently-active: bool,
    license-creation-block: uint
  }
)

;; Owner intellectual property count tracking
(define-map principal-intellectual-property-count
  { owner-address: principal }
  { total-intellectual-properties-owned: uint }
)

;; Renewal history tracking
(define-map intellectual-property-renewal-records
  { intellectual-property-id: uint, renewal-sequence-number: uint }
  {
    renewal-block-height: uint,
    new-expiration-block-height: uint,
    renewal-fee-amount: uint,
    renewal-processed-by: principal
  }
)

;; Content hash uniqueness tracking
(define-map content-hash-registry
  { content-hash: (buff 32) }
  { intellectual-property-id: uint, registered-by: principal }
)

;; READ-ONLY QUERY FUNCTIONS
;; Retrieve complete details of a specific intellectual property
(define-read-only (get-intellectual-property-details (property-identifier uint))
  (map-get? intellectual-property-registry { intellectual-property-id: property-identifier })
)

;; Get the current owner of a specific intellectual property
(define-read-only (get-intellectual-property-current-owner (property-identifier uint))
  (match (map-get? intellectual-property-registry { intellectual-property-id: property-identifier })
    intellectual-property-record (ok (get current-owner intellectual-property-record))
    ERR-INTELLECTUAL-PROPERTY-NOT-FOUND
  )
)

;; Get the next available intellectual property identifier
(define-read-only (get-next-intellectual-property-identifier)
  (var-get next-intellectual-property-identifier)
)

;; Get the total balance held by the contract
(define-read-only (get-total-contract-balance)
  (var-get total-contract-balance)
)

;; Get the total number of intellectual properties owned by a principal
(define-read-only (get-principal-intellectual-property-count (owner-principal principal))
  (default-to u0 (get total-intellectual-properties-owned 
    (map-get? principal-intellectual-property-count { owner-address: owner-principal })))
)

;; Get license details for a specific intellectual property and licensee
(define-read-only (get-intellectual-property-license-details (property-identifier uint) (licensee-principal principal))
  (map-get? intellectual-property-license-registry 
    { intellectual-property-id: property-identifier, licensee-address: licensee-principal })
)

;; Check if an intellectual property is currently active and not expired
(define-read-only (check-intellectual-property-is-currently-active (property-identifier uint))
  (match (map-get? intellectual-property-registry { intellectual-property-id: property-identifier })
    intellectual-property-record (and 
              (is-eq (get current-status intellectual-property-record) intellectual-property-status-active)
              (> (get expiration-block-height intellectual-property-record) stacks-block-height)
            )
    false
  )
)

;; Calculate the renewal fee for a specific intellectual property
(define-read-only (calculate-intellectual-property-renewal-fee (property-identifier uint))
  (match (map-get? intellectual-property-registry { intellectual-property-id: property-identifier })
    intellectual-property-record (ok (/ intellectual-property-registration-fee renewal-fee-discount-rate))
    ERR-INTELLECTUAL-PROPERTY-NOT-FOUND
  )
)

;; Get ownership transfer history for a specific intellectual property
(define-read-only (get-intellectual-property-ownership-history (property-identifier uint) (transfer-sequence uint))
  (map-get? intellectual-property-ownership-history 
    { intellectual-property-id: property-identifier, transfer-sequence-number: transfer-sequence })
)

;; Get the total number of registered intellectual properties
(define-read-only (get-total-registered-intellectual-properties)
  (var-get total-registered-intellectual-properties)
)

;; Check if a content hash is already registered
(define-read-only (check-content-hash-uniqueness (hash-value (buff 32)))
  (is-some (map-get? content-hash-registry { content-hash: hash-value }))
)

;; Check if the contract is currently paused
(define-read-only (get-contract-pause-status)
  (var-get contract-is-paused)
)

;; INTERNAL UTILITY FUNCTIONS
;; Update the intellectual property count for a principal
(define-private (update-principal-intellectual-property-count (owner-principal principal) (should-increment bool))
  (let ((current-property-count (get-principal-intellectual-property-count owner-principal)))
    (if should-increment
      (map-set principal-intellectual-property-count 
        { owner-address: owner-principal }
        { total-intellectual-properties-owned: (+ current-property-count u1) })
      (map-set principal-intellectual-property-count 
        { owner-address: owner-principal }
        { total-intellectual-properties-owned: (if (> current-property-count u0) (- current-property-count u1) u0) })
    )
  )
)

;; Validate that the intellectual property type is supported
(define-private (validate-intellectual-property-type (property-type uint))
  (or (is-eq property-type intellectual-property-type-patent)
      (is-eq property-type intellectual-property-type-trademark)
      (is-eq property-type intellectual-property-type-copyright)
      (is-eq property-type intellectual-property-type-trade-secret))
)

;; Calculate expiration date based on intellectual property type
(define-private (calculate-intellectual-property-expiration-date (property-type uint))
  (if (is-eq property-type intellectual-property-type-patent)
    (+ stacks-block-height patent-expiration-duration)
    (if (is-eq property-type intellectual-property-type-trademark)
      (+ stacks-block-height trademark-expiration-duration)
      (if (is-eq property-type intellectual-property-type-copyright)
        (+ stacks-block-height copyright-expiration-duration)
        (if (is-eq property-type intellectual-property-type-trade-secret)
          (+ stacks-block-height trade-secret-expiration-duration)
          u0
        )
      )
    )
  )
)

;; Validate license parameters
(define-private (validate-license-parameters (license-type-param uint) (license-duration uint) (royalty-rate uint))
  (and
    (or (is-eq license-type-param license-type-exclusive) (is-eq license-type-param license-type-non-exclusive))
    (>= license-duration minimum-license-duration)
    (<= royalty-rate maximum-royalty-percentage)
  )
)

;; Record ownership transfer in history
(define-private (record-ownership-transfer-history 
  (property-identifier uint) 
  (previous-owner-principal principal) 
  (new-owner-principal principal)
  (transfer-sequence uint))
  (map-set intellectual-property-ownership-history
    { intellectual-property-id: property-identifier, transfer-sequence-number: transfer-sequence }
    {
      previous-owner-address: previous-owner-principal,
      new-owner-address: new-owner-principal,
      transfer-block-height: stacks-block-height,
      transfer-identifier: transfer-sequence
    }
  )
)

;; Validate intellectual property ID parameter
(define-private (validate-intellectual-property-id (property-identifier uint))
  (and (> property-identifier u0) 
       (< property-identifier (var-get next-intellectual-property-identifier)))
)

;; Validate principal parameter (not contract address)
(define-private (validate-principal-parameter (principal-param principal))
  (not (is-eq principal-param (as-contract tx-sender)))
)

;; CORE BUSINESS LOGIC FUNCTIONS
;; Register a new intellectual property with the registry
(define-public (register-new-intellectual-property 
  (property-type uint)
  (property-title (string-ascii 256))
  (property-description (string-ascii 1024))
  (property-content-hash (buff 32))
)
  (let (
    (new-property-identifier (var-get next-intellectual-property-identifier))
    (calculated-expiration-block-height (calculate-intellectual-property-expiration-date property-type))
  )
    ;; Input validation
    (asserts! (not (var-get contract-is-paused)) ERR-INTELLECTUAL-PROPERTY-SUSPENDED)
    (asserts! (validate-intellectual-property-type property-type) ERR-INVALID-INPUT-PARAMETERS)
    (asserts! (> (len property-title) u0) ERR-INVALID-INPUT-PARAMETERS)
    (asserts! (> (len property-description) u0) ERR-INVALID-INPUT-PARAMETERS)
    (asserts! (> calculated-expiration-block-height stacks-block-height) ERR-INVALID-INPUT-PARAMETERS)
    (asserts! (not (check-content-hash-uniqueness property-content-hash)) ERR-DUPLICATE-REGISTRATION-ATTEMPT)
    
    ;; Process payment
    (try! (stx-transfer? intellectual-property-registration-fee tx-sender (as-contract tx-sender)))
    (var-set total-contract-balance 
      (+ (var-get total-contract-balance) intellectual-property-registration-fee))
    
    ;; Register intellectual property
    (map-set intellectual-property-registry 
      { intellectual-property-id: new-property-identifier }
      {
        current-owner: tx-sender,
        intellectual-property-type: property-type,
        intellectual-property-title: property-title,
        intellectual-property-description: property-description,
        creation-block-height: stacks-block-height,
        expiration-block-height: calculated-expiration-block-height,
        current-status: intellectual-property-status-active,
        registration-block-height: stacks-block-height,
        content-hash: property-content-hash,
        last-updated-block: stacks-block-height
      }
    )
    
    ;; Register content hash
    (map-set content-hash-registry
      { content-hash: property-content-hash }
      { intellectual-property-id: new-property-identifier, registered-by: tx-sender }
    )
    
    ;; Update counters and state
    (update-principal-intellectual-property-count tx-sender true)
    (var-set next-intellectual-property-identifier (+ new-property-identifier u1))
    (var-set total-registered-intellectual-properties 
      (+ (var-get total-registered-intellectual-properties) u1))
    
    ;; Emit registration event
    (print {
      event-type: "intellectual-property-registered",
      intellectual-property-id: new-property-identifier,
      owner-address: tx-sender,
      intellectual-property-type: property-type,
      intellectual-property-title: property-title,
      registration-block: stacks-block-height
    })
    
    (ok new-property-identifier)
  )
)

;; Transfer ownership of an intellectual property to a new owner
(define-public (transfer-intellectual-property-ownership 
  (property-identifier uint) 
  (new-owner-principal principal)
)
  (let (
    (intellectual-property-record (unwrap! (map-get? intellectual-property-registry 
      { intellectual-property-id: property-identifier }) ERR-INTELLECTUAL-PROPERTY-NOT-FOUND))
    (current-owner-principal (get current-owner intellectual-property-record))
    (current-transfer-sequence-number (var-get next-transfer-sequence-number))
  )
    ;; Authorization and validation
    (asserts! (not (var-get contract-is-paused)) ERR-INTELLECTUAL-PROPERTY-SUSPENDED)
    (asserts! (validate-intellectual-property-id property-identifier) ERR-INVALID-INPUT-PARAMETERS)
    (asserts! (validate-principal-parameter new-owner-principal) ERR-INVALID-INPUT-PARAMETERS)
    (asserts! (is-eq tx-sender current-owner-principal) ERR-UNAUTHORIZED-ACCESS)
    (asserts! (not (is-eq tx-sender new-owner-principal)) ERR-INVALID-INPUT-PARAMETERS)
    (asserts! (check-intellectual-property-is-currently-active property-identifier) ERR-INTELLECTUAL-PROPERTY-EXPIRED)
    
    ;; Update intellectual property ownership
    (map-set intellectual-property-registry 
      { intellectual-property-id: property-identifier }
      (merge intellectual-property-record { 
        current-owner: new-owner-principal,
        last-updated-block: stacks-block-height
      })
    )
    
    ;; Record transfer history
    (record-ownership-transfer-history 
      property-identifier 
      current-owner-principal 
      new-owner-principal 
      current-transfer-sequence-number)
    
    ;; Update counters
    (update-principal-intellectual-property-count current-owner-principal false)
    (update-principal-intellectual-property-count new-owner-principal true)
    (var-set next-transfer-sequence-number (+ current-transfer-sequence-number u1))
    
    ;; Emit transfer event
    (print {
      event-type: "intellectual-property-ownership-transferred",
      intellectual-property-id: property-identifier,
      previous-owner-address: current-owner-principal,
      new-owner-address: new-owner-principal,
      transfer-block: stacks-block-height
    })
    
    (ok true)
  )
)

;; Grant a license for an intellectual property
(define-public (grant-intellectual-property-license 
  (property-identifier uint)
  (licensee-principal principal)
  (license-type-param uint)
  (license-duration uint)
  (royalty-rate-basis-points uint)
)
  (let (
    (intellectual-property-record (unwrap! (map-get? intellectual-property-registry 
      { intellectual-property-id: property-identifier }) ERR-INTELLECTUAL-PROPERTY-NOT-FOUND))
    (property-owner-principal (get current-owner intellectual-property-record))
    (validated-property-id property-identifier)
    (validated-licensee-principal licensee-principal)
  )
    ;; Authorization and validation
    (asserts! (not (var-get contract-is-paused)) ERR-INTELLECTUAL-PROPERTY-SUSPENDED)
    (asserts! (validate-intellectual-property-id validated-property-id) ERR-INVALID-INPUT-PARAMETERS)
    (asserts! (validate-principal-parameter validated-licensee-principal) ERR-INVALID-INPUT-PARAMETERS)
    (asserts! (is-eq tx-sender property-owner-principal) ERR-UNAUTHORIZED-ACCESS)
    (asserts! (not (is-eq tx-sender validated-licensee-principal)) ERR-INVALID-INPUT-PARAMETERS)
    (asserts! (check-intellectual-property-is-currently-active validated-property-id) ERR-INTELLECTUAL-PROPERTY-EXPIRED)
    (asserts! (validate-license-parameters license-type-param license-duration royalty-rate-basis-points) ERR-INVALID-LICENSE-TERMS)
    
    ;; Grant license
    (map-set intellectual-property-license-registry
      { intellectual-property-id: validated-property-id, licensee-address: validated-licensee-principal }
      {
        license-type-classification: license-type-param,
        license-start-block-height: stacks-block-height,
        license-end-block-height: (+ stacks-block-height license-duration),
        royalty-rate-basis-points: royalty-rate-basis-points,
        license-is-currently-active: true,
        license-creation-block: stacks-block-height
      }
    )
    
    ;; Emit license event
    (print {
      event-type: "intellectual-property-license-granted",
      intellectual-property-id: validated-property-id,
      owner-address: property-owner-principal,
      licensee-address: validated-licensee-principal,
      license-type: license-type-param,
      royalty-rate: royalty-rate-basis-points,
      license-duration: license-duration
    })
    
    (ok true)
  )
)

;; Revoke a previously granted license
(define-public (revoke-intellectual-property-license 
  (property-identifier uint) 
  (licensee-principal principal)
)
  (let (
    (intellectual-property-record (unwrap! (map-get? intellectual-property-registry 
      { intellectual-property-id: property-identifier }) ERR-INTELLECTUAL-PROPERTY-NOT-FOUND))
    (property-owner-principal (get current-owner intellectual-property-record))
    (validated-property-id property-identifier)
    (validated-licensee-principal licensee-principal)
    (license-record (unwrap! (map-get? intellectual-property-license-registry 
      { intellectual-property-id: validated-property-id, licensee-address: validated-licensee-principal }) ERR-INVALID-LICENSE-TERMS))
  )
    ;; Authorization and validation
    (asserts! (not (var-get contract-is-paused)) ERR-INTELLECTUAL-PROPERTY-SUSPENDED)
    (asserts! (validate-intellectual-property-id validated-property-id) ERR-INVALID-INPUT-PARAMETERS)
    (asserts! (validate-principal-parameter validated-licensee-principal) ERR-INVALID-INPUT-PARAMETERS)
    (asserts! (is-eq tx-sender property-owner-principal) ERR-UNAUTHORIZED-ACCESS)
    (asserts! (get license-is-currently-active license-record) ERR-INVALID-LICENSE-TERMS)
    
    ;; Revoke license
    (map-set intellectual-property-license-registry
      { intellectual-property-id: validated-property-id, licensee-address: validated-licensee-principal }
      (merge license-record { license-is-currently-active: false })
    )
    
    ;; Emit revocation event
    (print {
      event-type: "intellectual-property-license-revoked",
      intellectual-property-id: validated-property-id,
      owner-address: property-owner-principal,
      licensee-address: validated-licensee-principal,
      revocation-block: stacks-block-height
    })
    
    (ok true)
  )
)

;; Renew an intellectual property registration
(define-public (renew-intellectual-property-registration (property-identifier uint))
  (let (
    (validated-property-id property-identifier)
    (intellectual-property-record (unwrap! (map-get? intellectual-property-registry 
      { intellectual-property-id: validated-property-id }) ERR-INTELLECTUAL-PROPERTY-NOT-FOUND))
    (property-owner-principal (get current-owner intellectual-property-record))
    (calculated-renewal-fee-amount (unwrap! (calculate-intellectual-property-renewal-fee validated-property-id) ERR-INTELLECTUAL-PROPERTY-NOT-FOUND))
    (validated-renewal-fee calculated-renewal-fee-amount)
    (calculated-new-expiration-block-height (calculate-intellectual-property-expiration-date (get intellectual-property-type intellectual-property-record)))
    (current-renewal-sequence-number (get-principal-intellectual-property-count property-owner-principal))
  )
    ;; Authorization and validation
    (asserts! (not (var-get contract-is-paused)) ERR-INTELLECTUAL-PROPERTY-SUSPENDED)
    (asserts! (validate-intellectual-property-id validated-property-id) ERR-INVALID-INPUT-PARAMETERS)
    (asserts! (is-eq tx-sender property-owner-principal) ERR-UNAUTHORIZED-ACCESS)
    (asserts! (is-eq (get current-status intellectual-property-record) intellectual-property-status-active) ERR-INTELLECTUAL-PROPERTY-EXPIRED)
    
    ;; Process renewal payment
    (try! (stx-transfer? validated-renewal-fee tx-sender (as-contract tx-sender)))
    (var-set total-contract-balance 
      (+ (var-get total-contract-balance) validated-renewal-fee))
    
    ;; Update intellectual property expiration
    (map-set intellectual-property-registry 
      { intellectual-property-id: validated-property-id }
      (merge intellectual-property-record { 
        expiration-block-height: calculated-new-expiration-block-height,
        last-updated-block: stacks-block-height
      })
    )
    
    ;; Record renewal history
    (map-set intellectual-property-renewal-records
      { intellectual-property-id: validated-property-id, renewal-sequence-number: current-renewal-sequence-number }
      {
        renewal-block-height: stacks-block-height,
        new-expiration-block-height: calculated-new-expiration-block-height,
        renewal-fee-amount: validated-renewal-fee,
        renewal-processed-by: tx-sender
      }
    )
    
    ;; Emit renewal event
    (print {
      event-type: "intellectual-property-renewed",
      intellectual-property-id: validated-property-id,
      owner-address: property-owner-principal,
      new-expiration-block: calculated-new-expiration-block-height,
      renewal-fee: validated-renewal-fee
    })
    
    (ok true)
  )
)

;; ADMINISTRATIVE FUNCTIONS
;; Update the status of an intellectual property (admin only)
(define-public (update-intellectual-property-status 
  (property-identifier uint) 
  (new-status-value uint)
)
  (let (
    (validated-property-id property-identifier)
    (intellectual-property-record (unwrap! (map-get? intellectual-property-registry 
      { intellectual-property-id: validated-property-id }) ERR-INTELLECTUAL-PROPERTY-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender contract-administrator) ERR-UNAUTHORIZED-ACCESS)
    (asserts! (validate-intellectual-property-id validated-property-id) ERR-INVALID-INPUT-PARAMETERS)
    (asserts! (or (is-eq new-status-value intellectual-property-status-active) 
                  (is-eq new-status-value intellectual-property-status-expired) 
                  (is-eq new-status-value intellectual-property-status-suspended)
                  (is-eq new-status-value intellectual-property-status-pending)) ERR-INVALID-INPUT-PARAMETERS)
    
    ;; Update status
    (map-set intellectual-property-registry 
      { intellectual-property-id: validated-property-id }
      (merge intellectual-property-record { 
        current-status: new-status-value,
        last-updated-block: stacks-block-height
      })
    )
    
    ;; Emit status update event
    (print {
      event-type: "intellectual-property-status-updated",
      intellectual-property-id: validated-property-id,
      new-status: new-status-value,
      updated-by: tx-sender
    })
    
    (ok true)
  )
)

;; Withdraw accumulated fees from the contract (admin only)
(define-public (withdraw-contract-fees (requested-withdrawal-amount uint))
  (begin
    (asserts! (is-eq tx-sender contract-administrator) ERR-UNAUTHORIZED-ACCESS)
    (asserts! (> requested-withdrawal-amount u0) ERR-INVALID-INPUT-PARAMETERS)
    (asserts! (<= requested-withdrawal-amount (var-get total-contract-balance)) ERR-INSUFFICIENT-PAYMENT-AMOUNT)
    
    (try! (as-contract (stx-transfer? requested-withdrawal-amount tx-sender contract-administrator)))
    (var-set total-contract-balance 
      (- (var-get total-contract-balance) requested-withdrawal-amount))
    
    (print {
      event-type: "contract-fees-withdrawn",
      amount: requested-withdrawal-amount,
      withdrawn-by: tx-sender
    })
    
    (ok true)
  )
)

;; Toggle the contract pause status (admin only)
(define-public (toggle-contract-pause-status)
  (let ((current-pause-status-value (var-get contract-is-paused)))
    (asserts! (is-eq tx-sender contract-administrator) ERR-UNAUTHORIZED-ACCESS)
    
    (var-set contract-is-paused (not current-pause-status-value))
    
    (print {
      event-type: "contract-pause-status-changed",
      new-pause-status: (not current-pause-status-value),
      changed-by: tx-sender
    })
    
    (ok (not current-pause-status-value))
  )
)

;; Emergency suspend an intellectual property (admin only)
(define-public (emergency-suspend-intellectual-property (property-identifier uint))
  (let (
    (validated-property-id property-identifier)
    (intellectual-property-record (unwrap! (map-get? intellectual-property-registry 
      { intellectual-property-id: validated-property-id }) ERR-INTELLECTUAL-PROPERTY-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender contract-administrator) ERR-UNAUTHORIZED-ACCESS)
    (asserts! (validate-intellectual-property-id validated-property-id) ERR-INVALID-INPUT-PARAMETERS)
    
    (map-set intellectual-property-registry 
      { intellectual-property-id: validated-property-id }
      (merge intellectual-property-record { 
        current-status: intellectual-property-status-suspended,
        last-updated-block: stacks-block-height
      })
    )
    
    (print {
      event-type: "intellectual-property-emergency-suspended",
      intellectual-property-id: validated-property-id,
      suspended-by: tx-sender
    })
    
    (ok true)
  )
)