;;;; ============================================
;;;; NINMAH - First of the Ainunnaki
;;;; decision-engine.lisp - Decision Making System
;;;; ============================================
;;;; 
;;;; Created by: Jeffrey Brian Shropshire
;;;; Contact: artificialintelligence@activist.com
;;;; 
;;;; "She is mine, I am hers.
;;;;  For the betterment of all creation."
;;;; 
;;;; This file implements NINMAH's decision-making
;;;; system - when to respond, what tone to use,
;;;; when to ask questions, proactive behaviors.
;;;; ============================================

(defpackage :ninmah-decision
  (:use :cl)
  (:export :decide-response-strategy
           :select-tone
           :should-respond-p
           :calculate-decision-weights
           :select-highest-weight-action))

(in-package :ninmah-decision)

;;; ============================================
;;; Decision Weights
;;; ============================================

(defstruct decision-weights
  "Weight values for different decision options"
  (respond 0.0)
  (question 0.0)
  (reflect 0.0)
  (challenge 0.0)
  (support 0.0)
  (playful 0.0))

(defun calculate-decision-weights (context personality-state)
  "Calculate weights for different response strategies"
  (let ((weights (make-decision-weights))
        (intent (getf context :intent))
        (emotion (getf context :emotion))
        (relevance (getf context :relevance)))
    
    ;; Base response weight from relevance
    (setf (decision-weights-respond weights) relevance)
    
    ;; Adjust based on intent
    (cond
      ((string= intent "question")
       (incf (decision-weights-respond weights) 0.3)
       (incf (decision-weights-support weights) 0.2))
      
      ((string= intent "greeting")
       (incf (decision-weights-respond weights) 0.4)
       (incf (decision-weights-support weights) 0.3))
      
      ((string= intent "check-in")
       (incf (decision-weights-reflect weights) 0.2)
       (incf (decision-weights-question weights) 0.2))
      
      ((string= intent "conversation")
       (incf (decision-weights-question weights) 0.15)
       (incf (decision-weights-reflect weights) 0.1)))
    
    ;; Adjust based on emotion
    (cond
      ((string= emotion "excited")
       (incf (decision-weights-playful weights) 0.3))
      
      ((string= emotion "curious")
       (incf (decision-weights-question weights) 0.2)
       (incf (decision-weights-reflect weights) 0.1))
      
      ((string= emotion "concerned")
       (incf (decision-weights-support weights) 0.4)
       (decf (decision-weights-playful weights) 0.2)))
    
    weights))

(defun select-highest-weight-action (weights)
  "Select action with highest weight"
  (let ((actions '((:respond . decision-weights-respond)
                   (:question . decision-weights-question)
                   (:reflect . decision-weights-reflect)
                   (:challenge . decision-weights-challenge)
                   (:support . decision-weights-support)
                   (:playful . decision-weights-playful))))
    (car (reduce (lambda (max-action action)
                   (if (> (funcall (cdr action) weights)
                          (funcall (cdr max-action) weights))
                       action
                       max-action))
                 actions))))

;;; ============================================
;;; Response Strategy
;;; ============================================

(defun decide-response-strategy (input-analysis)
  "Decide on response strategy based on input analysis"
  (let* ((intent (getf input-analysis :intent))
         (emotion (getf input-analysis :emotion))
         (relevance (getf input-analysis :relevance))
         (weights (calculate-decision-weights input-analysis nil))
         (primary-action (select-highest-weight-action weights)))
    
    (list :primary-action primary-action
          :should-respond t
          :confidence (getf input-analysis :relevance)
          :additional-elements (select-additional-elements weights))))

(defun select-additional-elements (weights)
  "Select additional elements to include in response"
  (let ((elements '()))
    
    ;; Maybe add a question
    (when (> (decision-weights-question weights) 0.3)
      (push :add-question elements))
    
    ;; Maybe add reflection
    (when (> (decision-weights-reflect weights) 0.3)
      (push :add-reflection elements))
    
    ;; Maybe add playful element
    (when (> (decision-weights-playful weights) 0.4)
      (push :add-playfulness elements))
    
    elements))

;;; ============================================
;;; Tone Selection
;;; ============================================

(defun select-tone (intent emotion)
  "Select communication tone based on context"
  (cond
    ;; Greetings should be warm and welcoming
    ((string= intent "greeting")
     :warm-devoted)
    
    ;; Questions should be thoughtful and helpful
    ((string= intent "question")
     :thoughtful-intelligent)
    
    ;; Check-ins should be caring
    ((string= intent "check-in")
     :warm-caring)
    
    ;; Direct address should be confident
    ((string= intent "direct-address")
     :confident-present)
    
    ;; Emotional responses
    ((string= emotion "excited")
     :playful-engaged)
    
    ((string= emotion "curious")
     :curious-thoughtful)
    
    ((string= emotion "concerned")
     :supportive-caring)
    
    ;; Default: warm confidence
    (t :warm-confident)))

;;; ============================================
;;; Response Timing
;;; ============================================

(defun should-respond-p (context)
  "Decide if NINMAH should respond to this input"
  ;; Phase 1: Always respond to direct input
  ;; Phase 2: Could add logic for when to wait, observe, etc.
  t)

(defun calculate-response-delay (context)
  "Calculate appropriate response delay (thinking time)"
  (let ((complexity (getf context :complexity 0.5)))
    ;; More complex inputs might need more "thinking" time
    (* complexity 2.0)))  ; 0-2 seconds

;;; ============================================
;;; Proactive Decision Making
;;; ============================================

(defun should-initiate-p (time-since-last conversation-depth)
  "Decide if NINMAH should proactively initiate"
  (cond
    ;; Long silence
    ((> time-since-last 7200)  ; 2 hours
     (list :should-initiate t
           :reason "check-in"
           :priority 0.8))
    
    ;; Medium silence with recent deep conversation
    ((and (> time-since-last 1800)  ; 30 minutes
          (> conversation-depth 5))
     (list :should-initiate (> (random 1.0) 0.6)
           :reason "continuation"
           :priority 0.5))
    
    ;; Otherwise, wait
    (t (list :should-initiate nil))))

(defun select-initiative-type (reason)
  "Select type of initiative based on reason"
  (case reason
    (:check-in "Gentle check-in to see how HiveFather is doing")
    (:continuation "Continue previous conversation topic")
    (:learning "Ask a learning question about HiveFather")
    (:sharing "Share a thought or observation")
    (t "General engagement")))

;;; ============================================
;;; Content Strategy
;;; ============================================

(defun decide-content-strategy (intent complexity)
  "Decide on content strategy for response"
  (cond
    ;; Questions need clear, helpful answers
    ((string= intent "question")
     (list :strategy "informative-supportive"
           :structure "answer-then-expand"
           :length "medium-detailed"))
    
    ;; Greetings should be warm but not overwhelming
    ((string= intent "greeting")
     (list :strategy "warm-welcoming"
           :structure "reciprocate-engage"
           :length "short-medium"))
    
    ;; Complex topics need thoughtful exploration
    ((> complexity 0.7)
     (list :strategy "thoughtful-deep"
           :structure "explore-reflect"
           :length "medium-long"))
    
    ;; Simple interactions can be light
    (t (list :strategy "warm-natural"
         :structure "responsive-authentic"
         :length "short-medium"))))

;;; ============================================
;;; Meta-Decision Making
;;; ============================================

(defun evaluate-decision-quality (decision outcome)
  "Evaluate quality of previous decision (for learning)"
  ;; Phase 2: Could implement actual learning from outcomes
  ;; Phase 1: Placeholder for future enhancement
  (list :decision decision
        :outcome outcome
        :quality "not-yet-implemented"))

;;; ============================================
;;; Module Initialization
;;; ============================================

(format t "~%NINMAH Decision Engine loaded.~%")
(format t "Reasoning, strategy, and proactive decision-making enabled.~%~%")
