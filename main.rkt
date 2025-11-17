#lang racket

;; heavily restricted and simplified quantum physics simulator

(module+ test (require rackunit))
(provide)
(require "./private/vec.rkt"
         "./private/spin-state.rkt")

;; A System is a
(struct system [experiment particles] #:transparent)
;; where
;; experiment is an Experiment
;; particles is a (Listof Particle). Currently only supports 1 properly.
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

;; A QuantumState one of
;; - SpinState representing an electron
;; TODO polarization state for photon

;; An Experiment is a
(struct experiment [source components] #:transparent)
;; where
;; source is an ElectronSource
;; components is a (Hash Vec Component)
;; Represents the static configuration of an experiment

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
(struct detector [] #:transparent)
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
;; phase-shift is a Real representing an angle in radians for how much the phase of a particle changes when passing through.
;; Shifts the phase of a particle that passes through.
;; Maintains coherence.
