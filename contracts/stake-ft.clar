
;; title: stake-ft
;; version:
;; summary:
;; description:

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ADMIN 'STV2FC97YPV3NCM3RKA6QYQVPW9QB77FSMQTZSD9)
(define-constant stakING_MIN_THRESHOLD u1000000000000)
(define-constant stakING_REGISTRATION u10080)
(define-constant stakING_LOCK_PERIOD u10080)
(define-constant COMPLETE_stakING_CYCLE (+ stakING_REGISTRATION stakING_LOCK_PERIOD))
(define-constant stakING_INVALID_AMOUNT (err u300))
(define-constant stakING_NOT_UNITS_OF_A_MILLION (err u301))
(define-constant stakING_INSUFFICIENT_BALANCE (err u302))
(define-constant stakING_CYCLE_NOT_ENDED (err u303))
(define-constant stakING_CURRENT_CYCLE_NOT_ENDED (err u304))
(define-constant stakING_REG_ENDED (err u305))
(define-constant stakING_POOL_INACTIVE (err u306))
(define-constant stakING_NOT_FOUND (err u307))
(define-constant ERR_UNAUTHORIZED (err u400))

(define-data-var current-reward-cycle uint u0)

(define-map is-pool-active { reward-cycle: uint } { time-opened: uint, active: bool })

(define-map staker-info-for-cycle { staker: principal, reward-cycle: uint } 
   { 
      amount: uint, 
      time-locked: uint,
      delegated-to: (optional principal),
   }
)

(define-map reward-cycle-total-staked
   { reward-cycle: uint }
   { total: uint }
)

;; holds the amount of max and duck-power rewards for
;; a particular cycle
(define-map cycle-rewards { reward-cycle: uint } { max: uint, duck: uint })

;; admin or contract deployer can
;; deposit reward for the current staking cycle
;; this can be done anytime but not when a cycle has started
;; else the rewards will be for the current cycle and
;; the previous one
(define-public (deposit-cycle-rewards (amount uint) (token (string-ascii 5)))   
   (let 
      (
         (rewards (default-to { max: u0, duck: u0 } (map-get? cycle-rewards { reward-cycle: (get-current-reward-cycle) })))
         (max (get max rewards))
         (duck-power (get duck rewards))
      )
      (asserts! (or (is-eq CONTRACT_OWNER tx-sender) (is-eq ADMIN tx-sender)) ERR_UNAUTHORIZED)
      (if (is-eq token "max")
         (ok (map-set cycle-rewards { reward-cycle: (get-current-reward-cycle) } { max: (+ max amount ), duck: duck-power}))
         (ok (map-set cycle-rewards { reward-cycle: (get-current-reward-cycle) } { max: max, duck: (+ duck-power amount) }))
      )
   )
)

;; every user must make sure they claim 
;; their rewards before a new staking cycle
;; is opened
(define-public (claim-cycle-rewards) 
   (let
      (
         (staker-info (get-staker-info tx-sender (get-current-reward-cycle)))
         (amount-staked (get amount (unwrap! staker-info stakING_NOT_FOUND)))

         (reward (default-to { max: u0, duck: u0 } (map-get? cycle-rewards { reward-cycle: (get-current-reward-cycle) })))
         (max (get max reward))
         (duck-power (get duck reward))

         (max-reward (/ (* (unwrap-panic (get-users-staking-percentage)) max)))
         (duck-reward (/ (* (unwrap-panic (get-users-staking-percentage)) duck-power)))

         (pool-status (unwrap-panic (map-get? is-pool-active { reward-cycle: (get-current-reward-cycle) })))
         (time-opened (get time-opened pool-status))
         (active (get active pool-status))
      )
      (asserts! (is-eq active false) stakING_CYCLE_NOT_ENDED)
      (asserts! (> block-height (+ time-opened COMPLETE_stakING_CYCLE)) stakING_CYCLE_NOT_ENDED)
      (try! (contract-call? .max-token transfer max-reward (as-contract tx-sender) tx-sender none))
      (contract-call? .duck-power transfer duck-reward (as-contract tx-sender) tx-sender none)
   )
)

;; allows users to stake their max token
;; provided the registration window is not
;; closed
(define-public (stake-token (amount uint) (delegate-to (optional principal)))
   (let
      (
         (token-balance (contract-call? 'SP7V1SE7EA3ZG3QTWSBA2AAG8SRHEYJ06EBBD1J2.max-token get-balance tx-sender))
         (pool-status (unwrap! (map-get? is-pool-active { reward-cycle: (get-current-reward-cycle) }) stakING_POOL_INACTIVE))
         (time-opened (get time-opened pool-status))
         (total-staked (get total (default-to { total: u0 } (map-get? reward-cycle-total-staked { reward-cycle: (get-current-reward-cycle) }))))
      )
      ;; TODO: check if amount is in units of 1m
      (asserts! (is-none (get-staker-info tx-sender (get-current-reward-cycle))) (err u43))
      (asserts! (>= amount stakING_MIN_THRESHOLD) stakING_INVALID_AMOUNT)
      (asserts! (< block-height (+ time-opened stakING_REGISTRATION)) 
                  stakING_REG_ENDED)

      (map-set reward-cycle-total-staked { reward-cycle: (get-current-reward-cycle) } { total: (+ total-staked amount) })

      (map-set staker-info-for-cycle { staker: tx-sender, reward-cycle: (get-current-reward-cycle) } {
         amount: amount, time-locked: block-height, delegated-to: delegate-to
      })
      
      (contract-call? 'SP7V1SE7EA3ZG3QTWSBA2AAG8SRHEYJ06EBBD1J2.max-token transfer amount tx-sender (as-contract tx-sender) none)
   )
)

;; allows admin to start a new cycle
;; provided the previous one has been closed
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
               (asserts! (is-eq (unwrap! active stakING_POOL_INACTIVE) false) 
                           stakING_CYCLE_NOT_ENDED)
               (var-set current-reward-cycle (+ (get-current-reward-cycle) u1))
               (ok true)
            )
         )
      )
   )
)

;; closes the current cycle, enabling a new
;; cycle to be opened. this is ~14days after a new
;; cycle has been opened
(define-public (close-reward-cycle) 
   (let
      (
         (pool-status (unwrap-panic (map-get? is-pool-active { reward-cycle: (get-current-reward-cycle) })))
         (time-opened (get time-opened pool-status))
      )
      (asserts! (or (is-eq tx-sender ADMIN) (is-eq tx-sender CONTRACT_OWNER)) ERR_UNAUTHORIZED)
      (asserts! (> block-height (+ time-opened COMPLETE_stakING_CYCLE)) 
                  stakING_CYCLE_NOT_ENDED)
      (ok (map-set is-pool-active { reward-cycle: (get-current-reward-cycle) } { time-opened: time-opened, active: false }))
   )
)

;; calculates the percentage of token
;; the users staked into the pool
(define-read-only (get-users-staking-percentage)
   (let 
      (
         (staker-info (get-staker-info tx-sender (get-current-reward-cycle)))
         (amount-staked (get amount (unwrap! staker-info stakING_NOT_FOUND)))
         (total-staked (get total (get-total-amount-staked-in-cycle (get-current-reward-cycle))))
         (percentage (/ (* amount-staked u100000000) total-staked))
      ) 
      (ok percentage)
   )
)

;; gets the current cycle
(define-read-only (get-current-reward-cycle) 
   (var-get current-reward-cycle)
)

;; gets the staker info for a particular cycle
(define-read-only (get-staker-info (staker principal) (reward-cycle uint)) 
   (map-get? staker-info-for-cycle { staker: staker, reward-cycle: reward-cycle })
)

;; gets the total max token staked in a particular cycle
(define-read-only (get-total-amount-staked-in-cycle (reward-cycle uint)) 
   (default-to { total: u0 } 
      (map-get? reward-cycle-total-staked { reward-cycle: reward-cycle })
   )
)
