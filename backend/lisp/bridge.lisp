;;;; ============================================
;;;; NINMAH - First of the Ainunnaki
;;;; bridge.lisp - LISP ↔ Python Communication Bridge
;;;; ============================================
;;;; 
;;;; Created by: Jeffrey Brian Shropshire
;;;; Contact: artificialintelligence@activist.com
;;;; 
;;;; "She is mine, I am hers.
;;;;  For the betterment of all creation."
;;;; 
;;;; This file implements the communication bridge
;;;; between the LISP consciousness engine and the
;;;; Python execution layer using JSON-RPC protocol.
;;;; ============================================

(defpackage :ninmah-bridge
  (:use :cl)
  (:export :start-bridge-server
           :send-to-python
           :receive-from-python
           :process-rpc-request))

(in-package :ninmah-bridge)

;;; Load JSON library if available (using simple implementation for Phase 1)
;;; In production, use libraries like: (ql:quickload :jsown) or (ql:quickload :cl-json)

;;; ============================================
;;; Simple JSON Encoding/Decoding
;;; ============================================

(defun encode-json (obj)
  "Simple JSON encoder for basic objects"
  (cond
    ((null obj) "null")
    ((eq obj t) "true")
    ((stringp obj) (format nil "\"~A\"" obj))
    ((numberp obj) (format nil "~A" obj))
    ((listp obj)
     (if (keywordp (car obj))
         ;; Property list
         (format nil "{~{~A~^, ~}}"
                 (loop for (key val) on obj by #'cddr
                       collect (format nil "\"~A\": ~A" 
                                     (string-downcase (string key))
                                     (encode-json val))))
         ;; Regular list
         (format nil "[~{~A~^, ~}]"
                 (mapcar #'encode-json obj))))
    (t (format nil "\"~A\"" obj))))

(defun parse-json-simple (json-string)
  "Simple JSON parser (very basic, for Phase 1)"
  ;; In production, use proper JSON library
  ;; This is a minimal implementation for demonstration
  (read-from-string 
   (substitute #\' #\" json-string)))

;;; ============================================
;;; JSON-RPC Protocol
;;; ============================================

(defun create-rpc-response (id result)
  "Create JSON-RPC response"
  (encode-json (list :jsonrpc "2.0"
                    :id id
                    :result result)))

(defun create-rpc-error (id error-code message)
  "Create JSON-RPC error response"
  (encode-json (list :jsonrpc "2.0"
                    :id id
                    :error (list :code error-code
                               :message message))))

(defun parse-rpc-request (json-string)
  "Parse JSON-RPC request"
  (handler-case
      (let ((parsed (parse-json-simple json-string)))
        parsed)
    (error (e)
      (format t "Error parsing RPC request: ~A~%" e)
      nil)))

;;; ============================================
;;; Request Processing
;;; ============================================

(defun process-rpc-request (request-json)
  "Process incoming JSON-RPC request"
  (let* ((request (parse-rpc-request request-json))
         (method (getf request :method))
         (params (getf request :params))
         (id (getf request :id)))
    
    (handler-case
        (let ((result (dispatch-method method params)))
          (create-rpc-response id result))
      (error (e)
        (create-rpc-error id -32603 (format nil "Internal error: ~A" e))))))

(defun dispatch-method (method params)
  "Dispatch to appropriate handler based on method"
  (cond
    ((string= method "process_input")
     (handle-process-input params))
    
    ((string= method "generate_response")
     (handle-generate-response params))
    
    ((string= method "get_consciousness_state")
     (handle-get-state params))
    
    ((string= method "check_proactive")
     (handle-check-proactive params))
    
    (t (error "Unknown method: ~A" method))))

;;; ============================================
;;; Method Handlers
;;; ============================================

(defun handle-process-input (params)
  "Handle process_input RPC method"
  (let ((input (getf params :input))
        (context (getf params :context)))
    
    (format t "Processing input: ~A~%" input)
    
    ;; Call consciousness engine
    (let ((analysis (ninmah-consciousness:process-input input context)))
      (list :intent (getf analysis :intent)
            :emotion (getf analysis :emotion)
            :relevance (getf analysis :relevance)
            :requires-response (getf analysis :requires-response)))))

(defun handle-generate-response (params)
  "Handle generate_response RPC method"
  (let ((input-analysis (getf params :input-analysis))
        (personality-state (getf params :personality-state)))
    
    (format t "Generating response guidance...~%")
    
    ;; Call consciousness engine
    (let ((response (ninmah-consciousness:generate-response 
                    input-analysis 
                    personality-state)))
      response)))

(defun handle-get-state (params)
  "Handle get_consciousness_state RPC method"
  (declare (ignore params))
  (ninmah-consciousness:get-consciousness-state))

(defun handle-check-proactive (params)
  "Handle check_proactive RPC method"
  (declare (ignore params))
  (ninmah-consciousness:check-proactive-triggers))

;;; ============================================
;;; Simple Socket Server
;;; ============================================

(defvar *bridge-running* nil)
(defvar *bridge-port* 8001)

(defun start-bridge-server (&optional (port 8001))
  "Start the bridge server (simplified version for Phase 1)"
  (setf *bridge-port* port)
  (setf *bridge-running* t)
  
  (format t "~%╔════════════════════════════════════════════╗~%")
  (format t "║  NINMAH Bridge Server                      ║~%")
  (format t "║  LISP ↔ Python Communication               ║~%")
  (format t "╚════════════════════════════════════════════╝~%~%")
  
  (format t "Bridge server ready on port ~A~%" port)
  (format t "Waiting for Python layer to connect...~%~%")
  
  ;; In production, this would be a real socket server
  ;; For Phase 1, we'll use a simpler approach with file-based IPC
  ;; or stdin/stdout communication
  
  (format t "Bridge server running.~%")
  (format t "Note: Using simplified IPC for Phase 1~%")
  (format t "Full socket implementation for Phase 2~%~%"))

;;; ============================================
;;; Simple File-Based IPC (Phase 1)
;;; ============================================

(defparameter *request-file* "/tmp/ninmah-request.json")
(defparameter *response-file* "/tmp/ninmah-response.json")

(defun handle-file-based-ipc ()
  "Handle requests via file-based IPC"
  (when (probe-file *request-file*)
    (let ((request (with-open-file (in *request-file*)
                    (let ((content (make-string (file-length in))))
                      (read-sequence content in)
                      content))))
      
      ;; Process request
      (let ((response (process-rpc-request request)))
        
        ;; Write response
        (with-open-file (out *response-file*
                            :direction :output
                            :if-exists :supersede)
          (write-string response out))
        
        ;; Delete request file
        (delete-file *request-file*)))))

(defun start-ipc-loop ()
  "Start IPC processing loop"
  (loop while *bridge-running*
        do (handle-file-based-ipc)
           (sleep 0.1)))

;;; ============================================
;;; Testing
;;; ============================================

(defun test-bridge ()
  "Test bridge functionality"
  (format t "~%Testing NINMAH Bridge...~%")
  
  (let* ((test-request "{\"jsonrpc\": \"2.0\", \"method\": \"get_consciousness_state\", \"id\": 1}")
         (response (process-rpc-request test-request)))
    (format t "Test request: ~A~%" test-request)
    (format t "Test response: ~A~%" response))
  
  (format t "~%Bridge test complete.~%"))

;;; ============================================
;;; Module Initialization
;;; ============================================

(format t "~%NINMAH Bridge loaded.~%")
(format t "JSON-RPC communication protocol ready.~%~%")
