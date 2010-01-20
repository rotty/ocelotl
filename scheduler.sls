;;; scheduler.sls --- Simple round-robin scheduler

;; Copyright (C) 2009, 2010 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;; A simple round-robin scheduler built on nestable engines. This is
;; meant to be invoked from some kind of external main-loop.

;;; Code:
#!r6rs

(library (ocelotl scheduler)
  (export make-scheduler
          scheduler?
          scheduler-has-work?
          scheduler-work
          scheduler-enqueue!)
  (import (rnrs)
          (srfi :8 receive)
          (srfi :39 parameters)
          (spells queue)
          (spells engines))

(define-record-type scheduler
  (fields tasks)
  (protocol (lambda (p)
              (lambda ()
                (p (make-empty-queue))))))

;; Time slices are currently irrelevant, as there is no preemption
;; anyway
(define (scheduler-time-slice scheduler)
  100)

(define (scheduler-work scheduler)
  (and (scheduler-has-work? scheduler)
       (let ((task (dequeue! (scheduler-tasks scheduler))))
         (task (scheduler-time-slice scheduler)
               (lambda (trap-handler ticks engine-maker)
                 (trap-handler engine-maker))
               (lambda (engine)
                 (enqueue! (scheduler-tasks scheduler) engine)))
         #t)))

(define (scheduler-has-work? scheduler)
  (not (queue-empty? (scheduler-tasks scheduler))))

(define (scheduler-enqueue! scheduler proc consumer resume?)
  (enqueue! (scheduler-tasks scheduler)
            (make-engine
             (lambda (engine-return)
               (define (yield arg)
                 (engine-return
                  (lambda (engine-maker)
                    (if (resume? arg)
                        (scheduler-restart scheduler engine-maker arg)))))
               (let ((val (proc yield)))
                 (engine-return (lambda (engine-maker) (consumer val))))))))

(define (scheduler-restart scheduler engine-maker v)
  (enqueue! (scheduler-tasks scheduler) (engine-maker v)))

)
