#lang racket

;; heavily restricted and simplified quantum physics simulator

(module+ test (require rackunit))
(provide)
(require (except-in pict bitmap freeze)
         (only-in 2htdp/image bitmap freeze)
         2htdp/universe
         racket/draw
         "./private/vec.rkt"
         "./private/spin-state.rkt")

#;
(module+ main
  (define double-stern-gerlach
    (experiment (branch (vec 0 0) (vec 1 0) SPIN_UP 1)
                (hash (vec 2 0) (sg #t)
                      (vec 4 0) (sg #f)
                      (vec 2 2) (detector "L")
                      (vec 4 2) (detector "D")
                      (vec 6 0) (detector "U"))))
  (experiment-run/big-bang double-stern-gerlach))

;; A System is a
(struct system [experiment particle] #:transparent)
;; where
;; experiment is an Experiment
;; particle is a Particle
;; Represents an experiment with particles running through it

;; A Particle is a (Listof Branch)
;; Represents a superposition of possibilities for a particle

;; A Branch is a
(struct branch [position velocity state amplitude] #:transparent)
;; where
;; position is a Vec
;; velocity is a Vec
;; state is a QuantumState
;; amplitude is a Complex representing the amplitude of this branch of the superposition
;; Represents a possible path/state of a particle

(module+ test
  (define example-branch
    (branch (vec 0 0) (vec 1 0) SPIN_UP 1)))

;; A QuantumState one of
;; - SpinState representing an electron
;; TODO polarization state for photon

;; An Experiment is a
(struct experiment [source components] #:transparent)
;; where
;; source is an ElectronSource
;; components is a (Hash Vec Component)
;; Represents the static configuration of an experiment

(module+ test
  (define example-experiment
    (experiment example-branch (hash (vec 2 0) (detector 'A)))))

;; An ElectronSource is a Branch
;; Represents the initial state of an electron getting fired into the experiment

;; A Component is one of
;; - A SG
;; - A Detector
;; - A Splitter
;; - A Joiner
;; - A Mirror
;; - A Glass
;; Represents a device in an experiment that affects particles.

;; A SG is a
(struct sg [horizontal?] #:transparent)
;; where
;; horizontal? is a boolean representing whether the apparatus measures horizontal spin or vertical spin
;; Represents a stern-gerlach apparatus, which sends a particle down or right based on its spin.
;; Maintains coherence.

;; A Detector is a
(struct detector [name] #:transparent)
;; where
;; name is a Symbol
;; Represents a particle detector. Collapses the wave function when a particle it detects the particle.
;; Breaks coherence.

;; A Splitter is a
(struct splitter [] #:transparent)
;; Represents a beam splitter, which randomly sends the particle down or right.
;; Maintains coherence.

;; A Joiner is a
(struct joiner [] #:transparent)
;; Represents a beam joiner, which takes in particles from up and/or left and sends them right
;; Maintains coherence.

;; A Mirror is a
(struct mirror [norm] #:transparent)
;; where
;; norm is a Vec representing the normal of the mirror's surface. Can only be diagonal. Not necessarily a unit vector.
;; Changes the direction of a particle.
;; Maintains coherence.

;; A Glass is a
(struct glass [phase-shift] #:transparent)
;; where
;; phase-shift is a Complex which will be multiplied with branch amplitude for a particle passing through
;; Shifts the phase of a particle that passes through.
;; Maintains coherence.

;; A Result is one of
;; A Detector
;; #f, representing no particle detection

;; Experiment -> (Hash Result Real)
;; Runs the experiment many times and returns a probabliity distribution on detectors
(define (experiment-run ex)
  (define sys (system ex (experiment-emit-particle ex)))
  (define N 100)
  (define counts
    (for/fold ([counts (hasheq)])
              ([_ (in-range N)])
      (define result (system-steps sys))
      (hash-set counts result (add1 (hash-ref counts result 0)))))
  (for/hasheq ([(result count) (in-hash counts)])
    (values result (/ count N))))

;; Experiment -> Particle
;; emit a particle from the source
(define (experiment-emit-particle ex)
  (list (experiment-source ex)))

;; System -> Result
;; Step until experiment is over.
(define (system-steps sys)
  (define sys^ (system-step sys))
  (define maybe-detector (system-check-for-detection sys^))
  (cond
    [maybe-detector maybe-detector]
    [(system-done? sys) #f]
    [else (system-steps sys^)]))

;; System -> (or #f Detector)
;; Checks for a particle detected by a detector.
(define (system-check-for-detection sys)
  (for*/or ([brnch (system-particle sys)]
            [(pos cmp) (in-hash (experiment-components (system-experiment sys)))])
    (and (detector? cmp)
         (equal? (branch-position brnch)
                 pos)
         (equal? (branch-velocity brnch)
                 (vec 0 0)))))

;; System -> Boolean
;; Is it impossible for a particle to be detected in the future?
;; This could be true if all branches are out of bounds or there are no branches left.
(define (system-done? sys)
  (or (zero? (length (system-particle)))
      (for/and ([brnch (system-particle sys)])
        (or (branch-escaped? sys brnch)
            (equal? (vec 0 0) (branch-velocity brnch))))))

;; System Branch -> Boolean?
;; Is the branch outside of the system and never going to return?
(define (branch-escaped? sys brnch)
  (define px (vec-x (branch-position brnch)))
  (define py (vec-y (branch-position brnch)))
  (define vx (vec-x (branch-velocity brnch)))
  (define vy (vec-y (branch-velocity brnch)))
  (define-values (minx maxx miny maxy)
    (system-bounds sys))
  (or (and (> px maxx) (> vx 0))
      (and (< px minx) (< vx 0))
      (and (> py maxy) (> vy 0))
      (and (< py miny) (< vy 0))))

;; System -> (values Integer Integer Integer Integer)
;; Bounds of the system, determined by components and source
(define (system-bounds sys)
  (define positions
    (cons
     (branch-position (experiment-source (system-experiment sys)))
     (for/list ([(pos cmp) (in-hash (experiment-components (system-experiment sys)))])
       pos)))
  (for/fold ([minx +inf.0]
             [maxx -inf.0]
             [miny +inf.0]
             [maxy -inf.0])
            ([pos positions])
    (match pos
      [(vec x y)
       (values (exact-round (min minx x))
               (exact-round (max maxx x))
               (exact-round (min miny y))
               (exact-round (max maxy y)))])))

;; System -> System
;; Advances the system 1 timestep.
;; Makes a random choice for decoherence events like detection.
(define (system-step sys)
  ;; inspired by ECS pattern
  ;; order matters:
  ;; detector before movement in case we need to stop a particle.
  ;; (optimization) detector before components in case we prune branches.
  ;; (optimization) branchers like splitter and SG at end of components for efficiency.
  ;; movement after components because components may update velocity.
  ;; interference after movement and phase because particles have to be in final position and phase.
  ;; prune-impossible-branches after interference bc it can lead to zero-amplitude branches
  ;; renormalization last to be safe
  (define handlers
    (list handle-detector
          handle-joiner
          handle-mirror
          handle-glass
          handle-stern-gerlach
          handle-splitter

          handle-velocity
          handle-phase-shift
          handle-interference

          prune-impossible-branches

          ; just to be safe
          system-renormalize))
  (for/fold ([sys sys])
            ([handler handlers])
    (handler sys)))

(define (handle-detector sys)
  (let/ec abort; TODO this might not play nice if you move to continuation-based exact probabilistic programming
    (for/fold ([sys sys])
              ([(pos cmp) (in-hash (experiment-components (system-experiment sys)))])
      (if (detector? cmp)
          (let ([branches-in-detector (filter (lambda (brnch) (equal? pos (branch-position brnch)))
                                              (system-particle sys))])
            (for/fold ([sys sys])
                      ([brnch branches-in-detector])
              ;; collapse
              (define p (branch-probability brnch))
              (if (choice p)
                  (abort (detect-particle sys brnch))
                  (delete-branch sys brnch))))
          sys))))

(module+ test
  (parameterize ([current-choice (const #t)])
    (check-equal? (handle-detector (system example-experiment (list (branch (vec 2 0) (vec 1 0) (vec 1 0) 1/rad2)
                                                                    (branch (vec 0 1) (vec 1 0) (vec 1 0) 1/rad2))))
                  (system example-experiment (list (branch (vec 2 0) (vec 0 0) (vec 1 0) 1)))))
  (parameterize ([current-choice (const #f)])
    (check-equal? (handle-detector (system example-experiment (list (branch (vec 2 0) (vec 1 0) (vec 1 0) 1/rad2)
                                                                    (branch (vec 0 1) (vec 1 0) (vec 1 0) 1/rad2))))
                  (system example-experiment (list (branch (vec 0 1) (vec 1 0) (vec 1 0) 1.0))))))

(define current-choice (make-parameter (lambda (p) (< (random) p))))
(define (choice p) ((current-choice) p))

;; Branch -> Real
(define (branch-probability brnch)
  (real-part (* (branch-amplitude brnch) (conjugate (branch-amplitude brnch)))))

;; System Branch -> System
(define (detect-particle sys brnch)
  (struct-copy
   system sys
   [particle (list (struct-copy
                    branch brnch
                    [amplitude 1]
                    [velocity (vec 0 0)]))]))

;; System Branch -> System
(define (delete-branch sys brnch)
  (system-renormalize
   (struct-copy
    system sys
    [particle (filter (lambda (b) (not (eq? b brnch)))
                      (system-particle sys))])))

;; System Branch -> System
;; Scale all probabilities to achieve unitarity
(define (system-renormalize sys)
  (define total-probability
    (for/sum ([brnch (system-particle sys)])
      (branch-probability brnch)))
  (struct-copy
   system sys
   [particle (for/list ([brnch (system-particle sys)])
               (struct-copy
                branch brnch
                [amplitude (/ (branch-amplitude brnch) (sqrt total-probability))]))]))

(module+ test
  (check-equal? (system-renormalize (system example-experiment (list (branch (vec 0 0) (vec 0 0) (vec 1 0) 1)
                                                                     (branch (vec 1 0) (vec 0 0) (vec 1 0) 1))))
                (system example-experiment (list (branch (vec 0 0) (vec 0 0) (vec 1 0) 1/rad2)
                                                 (branch (vec 1 0) (vec 0 0) (vec 1 0) 1/rad2)))))

(define (handle-mirror sys)
  (handle-component sys
                    mirror?
                    (lambda (sys pos mrr brnch)
                      (define v (branch-velocity brnch))
                      (define n (mirror-norm mrr))
                      (define v^ (vec-map (vec-reflect v n)
                                          exact-round))
                      (list (struct-copy
                             branch brnch
                             [velocity v^]
                             ; 180 degree phase shift
                             [amplitude (- (branch-amplitude brnch))])))))

(module+ test
  ;; -> \
  ;;    |
  ;;    v
  (let ([exp (experiment example-branch (hash (vec 1 0) (mirror (vec 1 1))))]
        [brnch (branch (vec 1 0) (vec 1 0) SPIN_UP 1)])
    (check-equal? (handle-mirror (system exp (list brnch)))
                  (system exp
                          (list (struct-copy
                                 branch brnch
                                 [velocity (vec 0 -1)]
                                 [amplitude -1]))))))

(define (handle-glass sys)
  (handle-component sys
                    glass?
                    (lambda (sys pos gls brnch)
                      (list (struct-copy
                             branch brnch
                             [amplitude (* (glass-phase-shift gls) (branch-amplitude brnch))])))))

(define (handle-stern-gerlach sys)
  ; TODO check for spin state once there is polarization
  (system-renormalize; necessary due to rounding errors on spin state projection calculations.
   (handle-component sys
                     sg?
                     (lambda (sys pos cmp brnch)
                       (define s1
                         (if (sg-horizontal? cmp)
                             SPIN_RIGHT
                             SPIN_UP))
                       (define s2
                         (if (sg-horizontal? cmp)
                             SPIN_LEFT
                             SPIN_DOWN))
                       (if (equal? (vec 1 0) (branch-velocity brnch))
                           ; superposition in spin state translated to superposition in position/momentum
                           (list (struct-copy
                                  branch brnch
                                  [velocity (vec 1 0)]
                                  [amplitude (* (vec-dot s1 (branch-state brnch)) (branch-amplitude brnch))]
                                  [state s1])
                                 (struct-copy
                                  branch brnch
                                  [velocity (vec 0 1)]
                                  [amplitude (* (vec-dot s2 (branch-state brnch)) (branch-amplitude brnch))]
                                  [state s2]))
                           (list brnch))))))

(module+ test
  ;; spin up, vertical sg
  (let ([exp (experiment example-branch (hash (vec 1 0) (sg #f)))]
        [brnch (branch (vec 1 0) (vec 1 0) SPIN_UP 1)])
    (check-equal? (handle-stern-gerlach (system exp (list brnch)))
                  (system exp
                          (list (struct-copy
                                 branch brnch
                                 [velocity (vec 1 0)]
                                 [amplitude 1]
                                 [state SPIN_UP])
                                (struct-copy
                                 branch brnch
                                 [velocity (vec 0 1)]
                                 [amplitude 0]
                                 [state SPIN_DOWN])))))
  ; spin left, horizontal sg
  (let ([exp (experiment example-branch (hash (vec 1 0) (sg #t)))]
        [brnch (branch (vec 1 0) (vec 1 0) SPIN_LEFT 1)])
    (check-equal? (handle-stern-gerlach (system exp (list brnch)))
                  (system exp
                          (list (struct-copy
                                 branch brnch
                                 [velocity (vec 1 0)]
                                 [amplitude 0.0]
                                 [state SPIN_RIGHT])
                                (struct-copy
                                 branch brnch
                                 [velocity (vec 0 1)]
                                 [amplitude 1.0]
                                 [state SPIN_LEFT]))))))

(define (handle-splitter sys)
  (handle-component sys
                    splitter?
                    (lambda (sys pos cmp brnch)
                      (if (equal? (vec 1 0) (branch-velocity brnch))
                          (list brnch
                                (struct-copy
                                 branch brnch
                                 [velocity (vec 0 1)]
                                 ; +90 degree phase shift
                                 [amplitude (* 0+1i (branch-amplitude brnch))]))
                          (list brnch)))))

(module+ test
  (check-equal? (handle-splitter (system example-experiment (list example-branch)))
                (system example-experiment (list example-branch)))
  (let ([exp (experiment example-branch (hash (vec 1 0) (splitter)))]
        [brnch (branch (vec 1 0) (vec 1 0) SPIN_UP 1)])
    (check-equal? (handle-splitter (system exp (list brnch)))
                  (system exp
                          (list brnch
                                (struct-copy
                                 branch brnch
                                 [velocity (vec 0 1)]
                                 [amplitude 0+1i]))))))

(define (handle-joiner sys)
  (handle-component sys
                    joiner?
                    (lambda (sys pos cmp brnch)
                      (if (equal? (vec 0 1) (branch-velocity brnch))
                          (list (struct-copy
                                 branch brnch
                                 [velocity (vec 1 0)]
                                 ; -90 degree phase shift
                                 [amplitude (* 0-1i (branch-amplitude brnch))]))
                          (list brnch)))))

(module+ test
  (let ([exp (experiment example-branch (hash (vec 1 0) (joiner)))]
        [brnch (branch (vec 1 0) (vec 0 1) SPIN_UP 1)])
    (check-equal? (handle-joiner (system exp (list brnch)))
                  (system exp
                          (list (struct-copy
                                 branch brnch
                                 [velocity (vec 1 0)]
                                 [amplitude 0-1i])
                                )))))

;; System -> System
;; Move particles According to velocity
(define (handle-velocity sys)
  (system-map-branches sys
                       (lambda (brnch)
                         (struct-copy
                          branch brnch
                          [position
                           (vec-add (branch-position brnch)
                                    (branch-velocity brnch))]))))

(module+ test
  (check-equal? (handle-velocity (system example-experiment (list example-branch)))
                (system example-experiment (list (struct-copy
                                                  branch example-branch
                                                  [position (vec 1 0)])))))

;; System (Branch -> Branch) -> System
;; Apply f to every branch
(define (system-map-branches sys f)
  (system-map-branches/nondet sys (lambda (brnch) (list (f brnch)))))

;; System (Branch -> (Listof Branch)) -> System
;; Apply f to every branch, where f can split and delete branches
(define (system-map-branches/nondet sys f)
  (struct-copy
    system sys
    [particle
     (append-map f (system-particle sys))]))

(define TIME_EVOLUTION_PHASE_SHIFT -1); e^i*pi
(define MOMENTUM_PHASE_SHIFT (vec 0+1i 0+1i)); e^i*pi/2, e^i*pi/2
(define (handle-phase-shift sys)
  (system-map-branches sys
                       (lambda (brnch)
                         (if (vec-zero? (branch-velocity brnch))
                             brnch
                             (struct-copy
                              branch brnch
                              [amplitude
                               (* (branch-amplitude brnch)
                                  TIME_EVOLUTION_PHASE_SHIFT
                                  (vec-dot (branch-velocity brnch)
                                           MOMENTUM_PHASE_SHIFT))])))))

(module+ test
  (check-equal? (handle-phase-shift (system example-experiment (list example-branch)))
                (system example-experiment (list (struct-copy
                                                  branch example-branch
                                                  [amplitude
                                                   0-1i]))))
  ;; particle moving left gets opposite phase shift
  (let ([brnch (struct-copy
                branch example-branch
                [velocity (vec -1 0)])])
    (check-equal? (handle-phase-shift (system example-experiment (list brnch)))
                  (system example-experiment (list (struct-copy
                                                    branch brnch
                                                    [amplitude
                                                     0+1i]))))))

;; System -> System
(define (handle-interference sys)
  ;; interference is just broken. If all paths destructively interfere, the particle vanishes. going to accept this.
  ;; TODO weighted sum of spin states even though that breaks unitarity
  ;; TODO renormalize across branches
  ;; TODO renormalize spin state(?)
  (define groups
    (group-by (lambda (brnch) (cons (branch-position brnch)
                                    (branch-velocity brnch)))
              (system-particle sys)))
  (struct-copy
   system sys
   [particle
    (for/list ([branches groups])
      (define branch0 (first branches))
      (let ([s (for/fold ([s (vec 0 0)]);TODO handle polarization
                         ([brnch branches])
                 ; sum of spin states weighted by branch amplitude
                 (vec-add s (vec-scale (branch-amplitude brnch) (branch-state brnch))))])
                (if (vec-zero? s)
                    ; prune
                    (struct-copy
                     branch branch0
                     [amplitude 0])
                    (branch (branch-position branch0)
                            (branch-velocity branch0)
                            (vec-normalize s)
                            (for/sum ([brnch branches]) (branch-amplitude brnch))))))]))

(module+ test
  ;; destructive and constructive interference
  (check-equal? (handle-interference (system example-experiment (list (branch (vec 0 0) (vec 1 0) SPIN_UP 1/2); destructive
                                                                      (branch (vec 0 0) (vec 1 0) SPIN_UP -1/2)
                                                                      (branch (vec 0 0) (vec 0 1) SPIN_UP 1/2); constructive
                                                                      (branch (vec 0 0) (vec 0 1) SPIN_DOWN 1/2))))
                (system example-experiment (list (branch (vec 0 0) (vec 1 0) SPIN_UP 0)
                                                 ; SPIN_RIGHT bc right = 1/rad2(up + down)
                                                 (branch (vec 0 0) (vec 0 1) SPIN_RIGHT 1))))
  ;; spin annihilation edge case
  (check-equal? (handle-interference (system example-experiment (list (branch (vec 0 0) (vec 1 0) SPIN_UP 1/rad2)
                                                                      (branch (vec 0 0) (vec 1 0) (vec-scale -1 SPIN_UP) 1/rad2))))
                (system example-experiment (list (branch (vec 0 0) (vec 1 0) SPIN_UP 0)))))

;; System -> System
(define (prune-impossible-branches sys)
  (struct-copy
   system sys
   [particle
    (filter (lambda (brnch)
              (not (zero? (branch-amplitude brnch))))
            (system-particle sys))]))

;; System (Component -> Boolean) (System Vec Compoment Branch -> (Listof Branch))
;; helper for local components
(define (handle-component sys component? f)
  (for/fold ([sys sys])
            ([(pos cmp) (in-hash (experiment-components (system-experiment sys)))])
    (if (component? cmp)
        (system-map-branches/nondet sys
                                    (lambda (brnch)
                                      (if (equal? pos (branch-position brnch))
                                          (f sys pos cmp brnch)
                                          (list brnch))))
        sys)))

;;; visual

;; Experiment -> System
;; Runs the system once
(define (experiment-run/big-bang ex)
  (define sys (system ex (experiment-emit-particle ex)))
  (big-bang sys
    [on-key (lambda (sys . _) (system-step sys))]
    [on-tick (lambda (sys) sys)]
    [on-draw (compose freeze pict->bitmap system->pict)]))

(define CELL_SIZE 50)

;; System -> Pict
;; draw the system, with increasing y going up
(define (system->pict sys)
  (define-values (minx^ maxx^ miny^ maxy^)
    (system-bounds sys))
  ; add some margin
  (define minx (sub1 minx^))
  (define maxx (add1 maxx^))
  (define miny (sub1 miny^))
  (define maxy (add1 maxy^))
  (define width (exact-round (add1 (- maxx minx))))
  (define height (exact-round (add1 (- maxy miny))))
  (table width
         (for*/list ([y (in-inclusive-range maxy miny -1)]
                     [x (in-inclusive-range minx maxx)])
           (cc-superimpose
            (rectangle CELL_SIZE CELL_SIZE
                       #:border-color "gray")
            (system-cell->pict sys (vec x y))))
         cc-superimpose cc-superimpose
         0 0))

;; System Vec -> Pict
;; draw the stuff at the given position in the system
(define (system-cell->pict sys pos)
  (apply cc-superimpose
         (let ([cmp (hash-ref (experiment-components (system-experiment sys))
                              pos
                              #f)])
           (if cmp
               (component->pict cmp)
               (blank)))
         (for/list ([brnch (system-particle sys)]
                    #:when (equal? pos (branch-position brnch)))
           (branch->pict brnch))))

(define CELL_RECT
  (rectangle CELL_SIZE CELL_SIZE
        #:border-color "black"))

;; Component -> Pict
;; draw an experiment component
(define (component->pict cmp)
  (match cmp
    [(sg horizontal?)
     (cc-superimpose
      CELL_RECT
      (if horizontal?
          (hline (/ CELL_SIZE 2) 1)
          (vline 1 (/ CELL_SIZE 2))))]
    [(detector name)
     (cc-superimpose
      CELL_RECT
      (scale-to-fit (text name)
                    (scale CELL_RECT
                           0.6)
                    #:mode 'preserve/max))]
    [(splitter)
     (shear (hline CELL_SIZE 1 #:segment 5) 0 -1)]
    [(joiner)
     (hc-append (hline CELL_SIZE 1) (arrowhead (/ CELL_SIZE 5) 0))]
    [(mirror (vec 1 1))
     (shear (hline CELL_SIZE 1) 0 1)]
    [(mirror (vec 1 -1))
     (shear (hline CELL_SIZE 1) 0 -1)]
    [(mirror norm)
     (error 'component->pict "unknown norm ~a" norm)]
    [(glass _)
     (filled-rectangle CELL_SIZE CELL_SIZE
                       #:color "gray")]))

;; Branch -> Pict
(define ELECTRON_RADIUS (/ CELL_SIZE 4))
(define (branch->pict brnch)
  ;; [x] pie chart for probability.
  ;; [ ] line with arrow arrow from center to edge for phase direction.
  ;; [ ] line from center to half-radius with no arrowhead for spin direction (xz projection of bloch sphere).
  ;; [ ] arrow for velocity direction, outside the circle.
  ;; [ ] center of pict should be center of circle, even though velocity arrow goes outside.
  (define definite-spin-angle (get-definite-spin-angle brnch))
  (define p (branch-probability brnch))
  (define p-angle (* p 2 pi))
  (cc-superimpose
   (sector ELECTRON_RADIUS 0 p-angle)
   (circle (* 2 ELECTRON_RADIUS))))

;; Branch -> Real
(define (get-definite-spin-angle brnch)
  ;; TODO update for polarization
  ;; xz plane projection of location on bloch sphere
  (define s (branch-state s))
  (define up (vec-x s))
  (define down (vec-y s))
  ;; coordinates on the bloch sphere
  (define x (* 2 (real-part (* (conjugate up) down))))
  (define z (real-part (- (* (conjugate up) up) (* conjugate down) down)))
  (angle (make-rectangular x z)))

(define (sector radius start-angle end-angle #:color [color "gray"])
  (dc (lambda (dc x y)
        (define old-brush (send dc get-brush))
        (define old-pen (send dc get-pen))

        (send dc set-brush (new brush% [color color]))
        (send dc set-pen (new pen% [color color]))

        ;; Draw the pie slice
        (send dc draw-arc
              x y                           ; top-left corner
              (* 2 radius) (* 2 radius)     ; width and height
              start-angle                    ; start angle (radians)
              end-angle)                     ; end angle (radians)

        (send dc set-brush old-brush)
        (send dc set-pen old-pen))
      (* 2 radius)
      (* 2 radius)))

#;
(show-pict (branch->pict (branch (vec 0 0)
                                 (vec 1 0)
                                 SPIN_UP
                                 1/rad2)))
