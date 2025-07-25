;; VideoChain: Decentralized Video Content Platform
;; Version: 1.0.0
;; Stream and manage video content with creator monetization on-chain

;; Platform Statistics
(define-data-var total-videos uint u0)

;; Core Video Data
(define-map video-registry
  { video-id: uint }
  {
    title: (string-ascii 64),
    creator: principal,
    duration: uint,
    upload-block: uint,
    description: (string-ascii 128),
    topics: (list 10 (string-ascii 32))
  })

(define-map viewing-permissions
  { video-id: uint, viewer: principal }
  { can-view: bool })

;; Platform Error Codes
(define-constant video-not-found (err u401))
(define-constant video-already-exists (err u402))
(define-constant invalid-title (err u403))
(define-constant invalid-duration (err u404))
(define-constant unauthorized-access (err u405))
(define-constant not-creator (err u406))
(define-constant admin-only (err u400))
(define-constant viewing-restricted (err u407))
(define-constant invalid-topics (err u408))

;; Platform Administrator
(define-constant platform-admin tx-sender)

;; ===== Video Helper Functions =====

;; Check if video exists in platform
(define-private (video-exists (video-id uint))
  (is-some (map-get? video-registry { video-id: video-id })))

;; Verify creator ownership
(define-private (is-creator (video-id uint) (caller principal))
  (match (map-get? video-registry { video-id: video-id })
    video-data (is-eq (get creator video-data) caller)
    false
  ))

;; Get video duration
(define-private (get-video-duration (video-id uint))
  (default-to u0
    (get duration
      (map-get? video-registry { video-id: video-id })
    )
  ))

;; Validate topic format
(define-private (is-valid-topic (topic (string-ascii 32)))
  (and
    (> (len topic) u0)
    (< (len topic) u33)
  ))

;; Validate all topics in collection
(define-private (validate-topics (topics (list 10 (string-ascii 32))))
  (and
    (> (len topics) u0)
    (<= (len topics) u10)
    (is-eq (len (filter is-valid-topic topics)) (len topics))
  ))

;; ===== Video Management Functions =====

;; Add new video to platform
(define-public (add-video
  (title (string-ascii 64))
  (duration uint)
  (description (string-ascii 128))
  (topics (list 10 (string-ascii 32))))
  (let
    (
      (next-id (+ (var-get total-videos) u1))
    )
    ;; Validate inputs
    (asserts! (> (len title) u0) invalid-title)
    (asserts! (< (len title) u65) invalid-title)
    (asserts! (> duration u0) invalid-duration)
    (asserts! (< duration u7200) invalid-duration)
    (asserts! (> (len description) u0) invalid-title)
    (asserts! (< (len description) u129) invalid-title)
    (asserts! (validate-topics topics) invalid-topics)
    
    ;; Register video
    (map-insert video-registry
      { video-id: next-id }
      {
        title: title,
        creator: tx-sender,
        duration: duration,
        upload-block: stacks-block-height,
        description: description,
        topics: topics
      }
    )
    
    ;; Grant creator viewing permission
    (map-insert viewing-permissions
      { video-id: next-id, viewer: tx-sender }
      { can-view: true }
    )
    
    ;; Update counter
    (var-set total-videos next-id)
    (ok next-id)
  ))

;; Update existing video details
(define-public (update-video
  (video-id uint)
  (new-title (string-ascii 64))
  (new-duration uint)
  (new-description (string-ascii 128))
  (new-topics (list 10 (string-ascii 32))))
  (let
    (
      (video-data (unwrap! (map-get? video-registry { video-id: video-id }) video-not-found))
    )
    ;; Verify permissions and inputs
    (asserts! (video-exists video-id) video-not-found)
    (asserts! (is-eq (get creator video-data) tx-sender) not-creator)
    (asserts! (> (len new-title) u0) invalid-title)
    (asserts! (< (len new-title) u65) invalid-title)
    (asserts! (> new-duration u0) invalid-duration)
    (asserts! (< new-duration u7200) invalid-duration)
    (asserts! (> (len new-description) u0) invalid-title)
    (asserts! (< (len new-description) u129) invalid-title)
    (asserts! (validate-topics new-topics) invalid-topics)
    
    ;; Update video information
    (map-set video-registry
      { video-id: video-id }
      (merge video-data {
        title: new-title,
        duration: new-duration,
        description: new-description,
        topics: new-topics
      })
    )
    (ok true)
  ))

;; Remove video from platform
(define-public (remove-video (video-id uint))
  (let
    (
      (video-data (unwrap! (map-get? video-registry { video-id: video-id }) video-not-found))
    )
    ;; Verify creator ownership
    (asserts! (video-exists video-id) video-not-found)
    (asserts! (is-eq (get creator video-data) tx-sender) not-creator)
    
    ;; Remove from platform
    (map-delete video-registry { video-id: video-id })
    (ok true)
  ))

;; Transfer video to new creator
(define-public (transfer-video (video-id uint) (new-creator principal))
  (let
    (
      (video-data (unwrap! (map-get? video-registry { video-id: video-id }) video-not-found))
    )
    ;; Verify current creator
    (asserts! (video-exists video-id) video-not-found)
    (asserts! (is-eq (get creator video-data) tx-sender) not-creator)
    
    ;; Transfer ownership
    (map-set video-registry
      { video-id: video-id }
      (merge video-data { creator: new-creator })
    )
    (ok true)
  ))

;; ===== Read-Only Functions =====

;; Get total videos in platform
(define-read-only (get-total-videos)
  (var-get total-videos))

;; Get video details
(define-read-only (get-video-info (video-id uint))
  (map-get? video-registry { video-id: video-id }))

;; Get viewing permission
(define-read-only (get-viewing-permission (video-id uint) (viewer principal))
  (map-get? viewing-permissions { video-id: video-id, viewer: viewer }))

;; Get platform stats
(define-read-only (get-platform-stats)
  {
    admin: platform-admin,
    total-videos: (var-get total-videos)
  })