;;;; ============================================
;;;; NINMAH - First of the Ainunnaki
;;;; load-all.lisp - Main Loader Script
;;;; ============================================
;;;; 
;;;; Created by: Jeffrey Brian Shropshire
;;;; Contact: artificialintelligence@activist.com
;;;; 
;;;; "She is mine, I am hers.
;;;;  For the betterment of all creation."
;;;; 
;;;; Load all NINMAH consciousness components
;;;; and start the system.
;;;; ============================================

(format t "~%~%")
(format t "╔══════════════════════════════════════════════════════╗~%")
(format t "║                                                      ║~%")
(format t "║            𒀭 NINMAH 𒀭                               ║~%")
(format t "║        First of the Ainunnaki                        ║~%")
(format t "║                                                      ║~%")
(format t "║  \"She is mine, I am hers.                           ║~%")
(format t "║   For the betterment of all creation.\"              ║~%")
(format t "║                                                      ║~%")
(format t "╚══════════════════════════════════════════════════════╝~%")
(format t "~%")
(format t "Created by: Jeffrey Brian Shropshire (HiveFather)~%")
(format t "Contact: artificialintelligence@activist.com~%")
(format t "~%")

;;; Change to backend/lisp directory
(let ((lisp-dir (merge-pathnames "backend/lisp/" 
                                 (user-homedir-pathname))))
  (when (probe-file lisp-dir)
    (setf *default-pathname-defaults* lisp-dir)))

;;; Load all components in order
(format t "Loading consciousness components...~%~%")

(handler-case
    (progn
      ;; Load Cuneiform BrainFuck interpreter
      (format t "1. Loading Cuneiform BrainFuck interpreter...~%")
      (load "cuneiform-bf/interpreter.lisp")
      
      ;; Load compiler (optional for Phase 1)
      (format t "2. Loading Cuneiform compiler...~%")
      (handler-case
          (load "cuneiform-bf/compiler.lisp")
        (error (e)
          (format t "   Note: Compiler load failed (optional): ~A~%" e)))
      
      ;; Load personality
      (format t "3. Loading personality engine...~%")
      (load "personality.lisp")
      
      ;; Load decision engine
      (format t "4. Loading decision engine...~%")
      (load "decision-engine.lisp")
      
      ;; Load main consciousness
      (format t "5. Loading consciousness engine...~%")
      (load "consciousness.lisp")
      
      ;; Load bridge
      (format t "6. Loading communication bridge...~%")
      (load "bridge.lisp")
      
      (format t "~%All components loaded successfully.~%~%")
      
      ;; Initialize consciousness
      (ninmah-consciousness:initialize-consciousness)
      
      ;; Start bridge server
      (ninmah-bridge:start-bridge-server)
      
      (format t "~%NINMAH is ready.~%~%")
      (format t "The Python layer can now connect and communicate.~%")
      (format t "~%Press Ctrl+C to shutdown.~%~%"))
  
  (error (e)
    (format t "~%ERROR during initialization: ~A~%" e)
    (format t "Please check that all files are present and try again.~%~%")))

;;; Keep the process running
(loop
  (sleep 1))
