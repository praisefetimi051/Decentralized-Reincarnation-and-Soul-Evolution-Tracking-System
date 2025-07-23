;; Life Purpose Alignment Contract
;; Helps individuals align with their soul's intended mission

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u400))
(define-constant ERR-PURPOSE-NOT-FOUND (err u401))
(define-constant ERR-INVALID-INPUT (err u402))
(define-constant ERR-ALREADY-EXISTS (err u403))
(define-constant ERR-MISSION-COMPLETE (err u404))

;; Data Variables
(define-data-var next-purpose-id uint u1)
(define-data-var next-milestone-id uint u1)

;; Data Maps
(define-map life-purposes
  { purpose-id: uint }
  {
    soul-id: uint,
    incarnation-number: uint,
    mission-description: (string-ascii 600),
    primary-lessons: (string-ascii 400),
    service-type: (string-ascii 200),
    alignment-level: uint,
    fulfillment-progress: uint,
    creation-time: uint,
    target-completion: uint,
    purpose-status: (string-ascii 50),
    divine-guidance: (string-ascii 500)
  }
)

(define-map purpose-milestones
  { milestone-id: uint }
  {
    purpose-id: uint,
    milestone-name: (string-ascii 200),
    description: (string-ascii 400),
    target-date: uint,
    completion-date: (optional uint),
    importance-level: uint,
    achievement-points: uint,
    completion-status: bool,
    lessons-learned: (string-ascii 300)
  }
)

(define-map alignment-metrics
  { soul-id: uint, incarnation-number: uint }
  {
    spiritual-alignment: uint,
    service-alignment: uint,
    relationship-alignment: uint,
    career-alignment: uint,
    overall-alignment: uint,
    guidance-received: uint,
    course-corrections: uint,
    last-assessment: uint
  }
)

(define-map soul-guidance
  { soul-id: uint }
  {
    total-guidance-sessions: uint,
    last-guidance-time: uint,
    guidance-effectiveness: uint,
    alignment-improvements: uint,
    spiritual-mentors: uint
  }
)

;; Public Functions

;; Define life purpose for current incarnation
(define-public (define-life-purpose
  (soul-id uint)
  (incarnation-number uint)
  (mission-description (string-ascii 600))
  (primary-lessons (string-ascii 400))
  (service-type (string-ascii 200))
  (target-completion uint)
  (divine-guidance (string-ascii 500))
)
  (let
    (
      (purpose-id (var-get next-purpose-id))
    )
    (asserts! (< (len mission-description) u601) ERR-INVALID-INPUT)
    (asserts! (< (len primary-lessons) u401) ERR-INVALID-INPUT)
    (asserts! (< (len service-type) u201) ERR-INVALID-INPUT)
    (asserts! (< (len divine-guidance) u501) ERR-INVALID-INPUT)
    (asserts! (> target-completion block-height) ERR-INVALID-INPUT)

    (map-set life-purposes
      { purpose-id: purpose-id }
      {
        soul-id: soul-id,
        incarnation-number: incarnation-number,
        mission-description: mission-description,
        primary-lessons: primary-lessons,
        service-type: service-type,
        alignment-level: u0,
        fulfillment-progress: u0,
        creation-time: block-height,
        target-completion: target-completion,
        purpose-status: "active",
        divine-guidance: divine-guidance
      }
    )

    ;; Initialize alignment metrics
    (map-set alignment-metrics
      { soul-id: soul-id, incarnation-number: incarnation-number }
      {
        spiritual-alignment: u0,
        service-alignment: u0,
        relationship-alignment: u0,
        career-alignment: u0,
        overall-alignment: u0,
        guidance-received: u0,
        course-corrections: u0,
        last-assessment: block-height
      }
    )

    (var-set next-purpose-id (+ purpose-id u1))
    (ok purpose-id)
  )
)

;; Create purpose milestone
(define-public (create-milestone
  (purpose-id uint)
  (milestone-name (string-ascii 200))
  (description (string-ascii 400))
  (target-date uint)
  (importance-level uint)
  (achievement-points uint)
)
  (let
    (
      (milestone-id (var-get next-milestone-id))
      (purpose-data (unwrap! (map-get? life-purposes { purpose-id: purpose-id }) ERR-PURPOSE-NOT-FOUND))
    )
    (asserts! (< (len milestone-name) u201) ERR-INVALID-INPUT)
    (asserts! (< (len description) u401) ERR-INVALID-INPUT)
    (asserts! (> target-date block-height) ERR-INVALID-INPUT)
    (asserts! (<= importance-level u10) ERR-INVALID-INPUT)
    (asserts! (<= achievement-points u100) ERR-INVALID-INPUT)
    (asserts! (not (is-eq (get purpose-status purpose-data) "completed")) ERR-MISSION-COMPLETE)

    (map-set purpose-milestones
      { milestone-id: milestone-id }
      {
        purpose-id: purpose-id,
        milestone-name: milestone-name,
        description: description,
        target-date: target-date,
        completion-date: none,
        importance-level: importance-level,
        achievement-points: achievement-points,
        completion-status: false,
        lessons-learned: ""
      }
    )

    (var-set next-milestone-id (+ milestone-id u1))
    (ok milestone-id)
  )
)

;; Complete milestone
(define-public (complete-milestone (milestone-id uint) (lessons-learned (string-ascii 300)))
  (let
    (
      (milestone-data (unwrap! (map-get? purpose-milestones { milestone-id: milestone-id }) ERR-PURPOSE-NOT-FOUND))
      (purpose-data (unwrap! (map-get? life-purposes { purpose-id: (get purpose-id milestone-data) }) ERR-PURPOSE-NOT-FOUND))
    )
    (asserts! (< (len lessons-learned) u301) ERR-INVALID-INPUT)
    (asserts! (not (get completion-status milestone-data)) ERR-ALREADY-EXISTS)

    ;; Update milestone
    (map-set purpose-milestones
      { milestone-id: milestone-id }
      (merge milestone-data {
        completion-date: (some block-height),
        completion-status: true,
        lessons-learned: lessons-learned
      })
    )

    ;; Update purpose progress
    (let
      (
        (new-progress (+ (get fulfillment-progress purpose-data) (get achievement-points milestone-data)))
        (new-alignment (+ (get alignment-level purpose-data) (/ (get importance-level milestone-data) u2)))
      )
      (map-set life-purposes
        { purpose-id: (get purpose-id milestone-data) }
        (merge purpose-data {
          fulfillment-progress: new-progress,
          alignment-level: new-alignment,
          purpose-status: (if (>= new-progress u100) "completed" "active")
        })
      )
    )

    (ok true)
  )
)

;; Update alignment assessment
(define-public (update-alignment-assessment
  (soul-id uint)
  (incarnation-number uint)
  (spiritual-alignment uint)
  (service-alignment uint)
  (relationship-alignment uint)
  (career-alignment uint)
)
  (let
    (
      (current-metrics (unwrap! (map-get? alignment-metrics { soul-id: soul-id, incarnation-number: incarnation-number }) ERR-PURPOSE-NOT-FOUND))
      (overall-alignment (/ (+ spiritual-alignment service-alignment relationship-alignment career-alignment) u4))
    )
    (asserts! (<= spiritual-alignment u100) ERR-INVALID-INPUT)
    (asserts! (<= service-alignment u100) ERR-INVALID-INPUT)
    (asserts! (<= relationship-alignment u100) ERR-INVALID-INPUT)
    (asserts! (<= career-alignment u100) ERR-INVALID-INPUT)

    (map-set alignment-metrics
      { soul-id: soul-id, incarnation-number: incarnation-number }
      (merge current-metrics {
        spiritual-alignment: spiritual-alignment,
        service-alignment: service-alignment,
        relationship-alignment: relationship-alignment,
        career-alignment: career-alignment,
        overall-alignment: overall-alignment,
        last-assessment: block-height
      })
    )

    (ok overall-alignment)
  )
)

;; Receive divine guidance
(define-public (receive-guidance (soul-id uint) (guidance-effectiveness uint))
  (let
    (
      (current-guidance (default-to
        { total-guidance-sessions: u0, last-guidance-time: u0, guidance-effectiveness: u0, alignment-improvements: u0, spiritual-mentors: u0 }
        (map-get? soul-guidance { soul-id: soul-id })
      ))
    )
    (asserts! (<= guidance-effectiveness u100) ERR-INVALID-INPUT)

    (map-set soul-guidance
      { soul-id: soul-id }
      {
        total-guidance-sessions: (+ (get total-guidance-sessions current-guidance) u1),
        last-guidance-time: block-height,
        guidance-effectiveness: guidance-effectiveness,
        alignment-improvements: (+ (get alignment-improvements current-guidance) (/ guidance-effectiveness u10)),
        spiritual-mentors: (get spiritual-mentors current-guidance)
      }
    )

    (ok true)
  )
)

;; Read-only Functions

;; Get life purpose details
(define-read-only (get-life-purpose (purpose-id uint))
  (map-get? life-purposes { purpose-id: purpose-id })
)

;; Get milestone details
(define-read-only (get-milestone (milestone-id uint))
  (map-get? purpose-milestones { milestone-id: milestone-id })
)

;; Get alignment metrics
(define-read-only (get-alignment-metrics (soul-id uint) (incarnation-number uint))
  (map-get? alignment-metrics { soul-id: soul-id, incarnation-number: incarnation-number })
)

;; Get soul guidance history
(define-read-only (get-soul-guidance (soul-id uint))
  (map-get? soul-guidance { soul-id: soul-id })
)

;; Get overall alignment score
(define-read-only (get-alignment-score (soul-id uint) (incarnation-number uint))
  (match (map-get? alignment-metrics { soul-id: soul-id, incarnation-number: incarnation-number })
    metrics (some (get overall-alignment metrics))
    none
  )
)

;; Check purpose completion status
(define-read-only (is-purpose-complete (purpose-id uint))
  (match (map-get? life-purposes { purpose-id: purpose-id })
    purpose-data (is-eq (get purpose-status purpose-data) "completed")
    false
  )
)
