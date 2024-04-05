
;; title: stake-ft
;; version:
;; summary:
;; description:

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ADMIN 'STV2FC97YPV3NCM3RKA6QYQVPW9QB77FSMQTZSD9)
(define-constant STACKING_MIN_THRESHOLD u1000000000000)
(define-constant STACKING_REGISTRATION u10080)
(define-constant STACKING_LOCK_PERIOD u10080)
(define-constant COMPLETE_STACKING_CYCLE (+ STACKING_REGISTRATION STACKING_LOCK_PERIOD))
(define-constant STACKING_INVALID_AMOUNT (err u300))
(define-constant STACKING_NOT_UNITS_OF_A_MILLION (err u301))
(define-constant STACKING_INSUFFICIENT_BALANCE (err u302))
(define-constant STACKING_CYCLE_NOT_ENDED (err u303))
(define-constant STACKING_CURRENT_CYCLE_NOT_ENDED (err u304))
(define-constant STACKING_REG_ENDED (err u305))
(define-constant STACKING_POOL_INACTIVE (err u306))
(define-constant STACKING_NOT_FOUND (err u307))
(define-constant ERR_UNAUTHORIZED (err u400))

(define-data-var current-reward-cycle uint u0)

(define-map is-pool-active { reward-cycle: uint } { time-opened: uint, active: bool })

(define-map stacker-info-for-cycle { stacker: principal, reward-cycle: uint } 
   { 
      amount: uint, 
      time-locked: uint,
      delegated-to: (optional principal), 
   }
)

(define-map reward-cycle-total-stacked
   { reward-cycle: uint }
   { total: uint }
)

(define-public (claim-cycle-rewards) 
   (let
      (
         (stacker-info (map-get? stacker-info-for-cycle { stacker: tx-sender, reward-cycle: (get-current-reward-cycle) }))
         (amount-stacked (get amount (unwrap! stacker-info STACKING_NOT_FOUND)))

         (total-stacked (get total (default-to { total: u0 } (map-get? reward-cycle-total-stacked { reward-cycle: (get-current-reward-cycle) }))))
         (users-rewards (/ (* amount-stacked u100000000) total-stacked))

         (pool-status (unwrap-panic (map-get? is-pool-active { reward-cycle: (get-current-reward-cycle) })))
         (time-opened (get time-opened pool-status))
         (active (get active pool-status))
      )
      (asserts! (is-eq active false) STACKING_CYCLE_NOT_ENDED)
      (asserts! (> block-height (+ time-opened COMPLETE_STACKING_CYCLE)) STACKING_CYCLE_NOT_ENDED)
      (contract-call? 'SP7V1SE7EA3ZG3QTWSBA2AAG8SRHEYJ06EBBD1J2.max-token transfer users-rewards (as-contract tx-sender) tx-sender none)
   )
)

(define-public (stack-token (amount uint) (delegate-to (optional principal)))
   (let
      (
         (token-balance (contract-call? 'SP7V1SE7EA3ZG3QTWSBA2AAG8SRHEYJ06EBBD1J2.max-token get-balance tx-sender))
         (pool-status (unwrap! (map-get? is-pool-active { reward-cycle: (get-current-reward-cycle) }) STACKING_POOL_INACTIVE))
         (time-opened (get time-opened pool-status))
         (total-stacked (get total (default-to { total: u0 } (map-get? reward-cycle-total-stacked { reward-cycle: (get-current-reward-cycle) }))))
      )
      ;; TODO: check if amount is in units of 1m
      (asserts! (is-none (get-stacker-info tx-sender (get-current-reward-cycle))) (err u43))
      (asserts! (>= amount STACKING_MIN_THRESHOLD) STACKING_INVALID_AMOUNT)
      (asserts! (< block-height (+ time-opened STACKING_REGISTRATION)) 
                  STACKING_REG_ENDED)

      (map-set reward-cycle-total-stacked { reward-cycle: (get-current-reward-cycle) } { total: (+ total-stacked amount) })

      (map-set stacker-info-for-cycle { stacker: tx-sender, reward-cycle: (get-current-reward-cycle) } {
         amount: amount, time-locked: block-height, delegated-to: delegate-to
      })
      
      (contract-call? 'SP7V1SE7EA3ZG3QTWSBA2AAG8SRHEYJ06EBBD1J2.max-token transfer amount tx-sender (as-contract tx-sender) none)
   )
)

(define-public (open-reward-cycle) 
   (begin
      (asserts! (or (is-eq tx-sender ADMIN) (is-eq tx-sender CONTRACT_OWNER)) ERR_UNAUTHORIZED)
      (map-set is-pool-active { reward-cycle: (+ u1 (get-current-reward-cycle)) } { time-opened: block-height, active: true })
      (let
         (
            (current-cycle (map-get? is-pool-active { reward-cycle: (get-current-reward-cycle) }))
            (active (get active current-cycle))
         )
         (if (is-none current-cycle) 
            (begin
               (var-set current-reward-cycle (+ (get-current-reward-cycle) u1))
               (ok true)
            )
            (begin
               (asserts! (is-eq (unwrap! active STACKING_POOL_INACTIVE) false) 
                           STACKING_CYCLE_NOT_ENDED)
               (var-set current-reward-cycle (+ (get-current-reward-cycle) u1))
               (ok true)
            )
         )
      )
   )
)

(define-public (close-reward-cycle) 
   (let
      (
         (pool-status (unwrap-panic (map-get? is-pool-active { reward-cycle: (get-current-reward-cycle) })))
         (time-opened (get time-opened pool-status))
      )
      (asserts! (or (is-eq tx-sender ADMIN) (is-eq tx-sender CONTRACT_OWNER)) ERR_UNAUTHORIZED)
      (asserts! (> block-height (+ time-opened COMPLETE_STACKING_CYCLE)) 
                  STACKING_CYCLE_NOT_ENDED)
      (ok (map-set is-pool-active { reward-cycle: (get-current-reward-cycle) } { time-opened: time-opened, active: false }))
   )
)

(define-read-only (get-current-reward-cycle) 
   (var-get current-reward-cycle)
)

(define-read-only (get-stacker-info (stacker principal) (reward-cycle uint)) 
   (map-get? stacker-info-for-cycle { stacker: stacker, reward-cycle: reward-cycle })
)

(define-read-only (get-total-amount-stacked-in-cycle (reward-cycle uint)) 
   (default-to { total: u0 } 
      (map-get? reward-cycle-total-stacked { reward-cycle: reward-cycle })
   )
)
