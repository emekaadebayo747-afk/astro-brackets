;; Astro Brackets - Decentralized Gaming Platform Smart Contract
;; This contract handles player registration, tournaments, and prize distribution

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-insufficient-funds (err u103))
(define-constant err-tournament-full (err u104))
(define-constant err-tournament-not-active (err u105))
(define-constant err-unauthorized (err u106))

;; Data Variables
(define-data-var next-player-id uint u1)
(define-data-var next-tournament-id uint u1)
(define-data-var platform-fee-percentage uint u5) ;; 5% platform fee

;; Data Maps
(define-map players 
    principal 
    {
        player-id: uint,
        skill-level: uint, ;; 1-10 scale representing different galaxies
        total-tournaments: uint,
        total-wins: uint,
        astro-card-level: uint
    }
)

(define-map tournaments
    uint
    {
        creator: principal,
        name: (string-ascii 50),
        entry-fee: uint,
        max-participants: uint,
        current-participants: uint,
        prize-pool: uint,
        status: (string-ascii 20), ;; "active", "completed", "cancelled"
        winner: (optional principal),
        galaxy-tier: uint ;; skill level requirement
    }
)

(define-map tournament-participants
    {tournament-id: uint, participant: principal}
    {
        position: uint,
        eliminated: bool
    }
)

(define-map astro-cards
    principal
    {
        card-id: uint,
        level: uint,
        wins: uint,
        special-abilities: uint, ;; bitmask for abilities
        visual-traits: uint
    }
)

;; Read-only functions
(define-read-only (get-player (player principal))
    (map-get? players player)
)

(define-read-only (get-tournament (tournament-id uint))
    (map-get? tournaments tournament-id)
)

(define-read-only (get-astro-card (player principal))
    (map-get? astro-cards player)
)

(define-read-only (get-tournament-participant (tournament-id uint) (participant principal))
    (map-get? tournament-participants {tournament-id: tournament-id, participant: participant})
)

(define-read-only (get-next-player-id)
    (var-get next-player-id)
)

(define-read-only (get-next-tournament-id)
    (var-get next-tournament-id)
)

;; Private functions
(define-private (calculate-skill-level (wins uint) (total-tournaments uint))
    (if (is-eq total-tournaments u0)
        u1
        (let ((win-rate (/ (* wins u100) total-tournaments)))
            (if (>= win-rate u90) u10
            (if (>= win-rate u80) u9
            (if (>= win-rate u70) u8
            (if (>= win-rate u60) u7
            (if (>= win-rate u50) u6
            (if (>= win-rate u40) u5
            (if (>= win-rate u30) u4
            (if (>= win-rate u20) u3
            (if (>= win-rate u10) u2
                u1)))))))))
        )
    )
)

(define-private (evolve-astro-card (player principal))
    (match (map-get? astro-cards player)
        card 
        (let (
            (new-level (+ (get level card) u1))
            (new-abilities (+ (get special-abilities card) u1))
        )
            (map-set astro-cards player
                (merge card {
                    level: new-level,
                    special-abilities: new-abilities,
                    visual-traits: (+ (get visual-traits card) u1)
                })
            )
            (ok true)
        )
        (err err-not-found)
    )
)

;; Public functions
(define-public (register-player)
    (let (
        (player-id (var-get next-player-id))
    )
        (asserts! (is-none (map-get? players tx-sender)) err-already-exists)
        
        ;; Register player
        (map-set players tx-sender {
            player-id: player-id,
            skill-level: u1,
            total-tournaments: u0,
            total-wins: u0,
            astro-card-level: u1
        })
        
        ;; Mint initial Astro Card
        (map-set astro-cards tx-sender {
            card-id: player-id,
            level: u1,
            wins: u0,
            special-abilities: u1,
            visual-traits: u1
        })
        
        ;; Increment next player ID
        (var-set next-player-id (+ player-id u1))
        
        (ok player-id)
    )
)

(define-public (create-tournament (name (string-ascii 50)) (entry-fee uint) (max-participants uint) (galaxy-tier uint))
    (let (
        (tournament-id (var-get next-tournament-id))
    )
        (asserts! (is-some (map-get? players tx-sender)) err-not-found)
        (asserts! (and (>= galaxy-tier u1) (<= galaxy-tier u10)) err-not-found)
        
        (map-set tournaments tournament-id {
            creator: tx-sender,
            name: name,
            entry-fee: entry-fee,
            max-participants: max-participants,
            current-participants: u0,
            prize-pool: u0,
            status: "active",
            winner: none,
            galaxy-tier: galaxy-tier
        })
        
        (var-set next-tournament-id (+ tournament-id u1))
        (ok tournament-id)
    )
)

(define-public (join-tournament (tournament-id uint))
    (match (map-get? tournaments tournament-id)
        tournament
        (match (map-get? players tx-sender)
            player
            (begin
                (asserts! (is-eq (get status tournament) "active") err-tournament-not-active)
                (asserts! (< (get current-participants tournament) (get max-participants tournament)) err-tournament-full)
                (asserts! (>= (get skill-level player) (get galaxy-tier tournament)) err-unauthorized)
                (asserts! (>= (stx-get-balance tx-sender) (get entry-fee tournament)) err-insufficient-funds)
                
                ;; Transfer entry fee
                (try! (stx-transfer? (get entry-fee tournament) tx-sender (as-contract tx-sender)))
                
                ;; Add participant
                (map-set tournament-participants 
                    {tournament-id: tournament-id, participant: tx-sender}
                    {
                        position: (+ (get current-participants tournament) u1),
                        eliminated: false
                    }
                )
                
                ;; Update tournament
                (map-set tournaments tournament-id
                    (merge tournament {
                        current-participants: (+ (get current-participants tournament) u1),
                        prize-pool: (+ (get prize-pool tournament) (get entry-fee tournament))
                    })
                )
                
                (ok true)
            )
            err-not-found
        )
        err-not-found
    )
)

(define-public (complete-tournament (tournament-id uint) (winner principal))
    (match (map-get? tournaments tournament-id)
        tournament
        (begin
            (asserts! (is-eq tx-sender (get creator tournament)) err-unauthorized)
            (asserts! (is-eq (get status tournament) "active") err-tournament-not-active)
            (asserts! (is-some (map-get? tournament-participants {tournament-id: tournament-id, participant: winner})) err-not-found)
            
            ;; Calculate prize distribution
            (let (
                (prize-pool (get prize-pool tournament))
                (platform-fee (/ (* prize-pool (var-get platform-fee-percentage)) u100))
                (winner-prize (- prize-pool platform-fee))
            )
                ;; Transfer prize to winner
                (try! (as-contract (stx-transfer? winner-prize tx-sender winner)))
                
                ;; Update tournament status
                (map-set tournaments tournament-id
                    (merge tournament {
                        status: "completed",
                        winner: (some winner)
                    })
                )
                
                ;; Update winner's stats
                (match (map-get? players winner)
                    player-stats
                    (let (
                        (new-wins (+ (get total-wins player-stats) u1))
                        (new-tournaments (+ (get total-tournaments player-stats) u1))
                        (new-skill-level (calculate-skill-level new-wins new-tournaments))
                    )
                        (map-set players winner
                            (merge player-stats {
                                total-tournaments: new-tournaments,
                                total-wins: new-wins,
                                skill-level: new-skill-level,
                                astro-card-level: (+ (get astro-card-level player-stats) u1)
                            })
                        )
                        
                        ;; Evolve winner's Astro Card
                        (unwrap! (evolve-astro-card winner) err-not-found)
                        
                        (ok true)
                    )
                    err-not-found
                )
            )
        )
        err-not-found
    )
)

(define-public (update-platform-fee (new-fee uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (<= new-fee u20) err-unauthorized) ;; Max 20% fee
        (var-set platform-fee-percentage new-fee)
        (ok true)
    )
)

;; Emergency functions
(define-public (cancel-tournament (tournament-id uint))
    (match (map-get? tournaments tournament-id)
        tournament
        (begin
            (asserts! (or (is-eq tx-sender (get creator tournament)) (is-eq tx-sender contract-owner)) err-unauthorized)
            (asserts! (is-eq (get status tournament) "active") err-tournament-not-active)
            
            ;; Update status
            (map-set tournaments tournament-id
                (merge tournament {status: "cancelled"})
            )
            
            ;; Note: In a production contract, you'd want to implement refund logic here
            (ok true)
        )
        err-not-found
    )
)