;;;; connection.lisp - IRC connection management

(in-package #:clatter-irc)

;;; Conditions

(define-condition irc-error (error)
  ((message :initarg :message :reader irc-error-message))
  (:report (lambda (c s) (format s "IRC error: ~A" (irc-error-message c)))))

(define-condition connection-error (irc-error) ()
  (:report (lambda (c s) (format s "Connection error: ~A" (irc-error-message c)))))

(define-condition authentication-error (irc-error) ()
  (:report (lambda (c s) (format s "Authentication error: ~A" (irc-error-message c)))))

(define-condition protocol-error (irc-error) ()
  (:report (lambda (c s) (format s "Protocol error: ~A" (irc-error-message c)))))

;;; Connection class

(defclass connection ()
  ((server :initarg :server :accessor connection-server
           :documentation "Server hostname")
   (port :initarg :port :accessor connection-port
         :initform nil
         :documentation "Server port")
   (nick :initarg :nick :accessor connection-nick
         :documentation "Current nickname")
   (username :initarg :username :accessor connection-username
             :initform nil
             :documentation "Username for USER command")
   (realname :initarg :realname :accessor connection-realname
             :initform nil
             :documentation "Realname for USER command")
   (password :initarg :password :accessor connection-password
             :initform nil
             :documentation "Server password")
   ;; TLS options
   (tls :initarg :tls :accessor connection-tls-p
        :initform nil
        :documentation "Use TLS/SSL")
   (client-cert :initarg :client-cert :accessor connection-client-cert
                :initform nil
                :documentation "Path to client certificate for SASL EXTERNAL")
   ;; SASL options
   (sasl-username :initarg :sasl-username :accessor connection-sasl-username
                  :initform nil
                  :documentation "SASL username (defaults to nick)")
   (sasl-password :initarg :sasl-password :accessor connection-sasl-password
                  :initform nil
                  :documentation "SASL password")
   ;; Internal state
   (socket :initform nil :accessor connection-socket)
   (stream :initform nil :accessor connection-stream)
   (state :initform :disconnected :accessor connection-state
          :documentation "Connection state: :disconnected :connecting :registering :connected")
   (read-thread :initform nil :accessor connection-read-thread)
   (write-lock :initform (bt:make-lock "irc-write") :accessor connection-write-lock)
   ;; IRCv3 state
   (cap-negotiating :initform nil :accessor connection-cap-negotiating)
   (cap-enabled :initform nil :accessor connection-cap-enabled
                :documentation "List of enabled capabilities")
   (sasl-state :initform nil :accessor connection-sasl-state)
   ;; IRCv3 batch tracking
   (active-batches :initform (make-hash-table :test 'equal) :accessor connection-active-batches)
   ;; Labeled response tracking
   (pending-labels :initform (make-hash-table :test 'equal) :accessor connection-pending-labels)
   (label-counter :initform 0 :accessor connection-label-counter)
   ;; Channels and users
   (channels :initform (make-hash-table :test 'equalp) :accessor connection-channels
             :documentation "Hash of channel name -> channel object")
   ;; Reconnection
   (reconnect-enabled :initarg :reconnect :accessor connection-reconnect-enabled
                      :initform t)
   (reconnect-attempts :initform 0 :accessor connection-reconnect-attempts)
   ;; Health monitoring
   (last-activity :initform (get-universal-time) :accessor connection-last-activity)
   (ping-sent-time :initform nil :accessor connection-ping-sent-time)
   ;; Hooks
   (hooks :initform (make-hash-table :test 'eq) :accessor connection-hooks
          :documentation "Hash of hook-name -> list of handler functions")
   ;; User data slot for application use
   (user-data :initarg :user-data :accessor connection-user-data
              :initform nil
              :documentation "Arbitrary user data"))
  (:documentation "IRC connection"))

(defmethod print-object ((conn connection) stream)
  (print-unreadable-object (conn stream :type t)
    (format stream "~A@~A:~A (~A)"
            (connection-nick conn)
            (connection-server conn)
            (connection-port conn)
            (connection-state conn))))

;;; Connection creation

(defun make-connection (server nick &key
                                      (port nil)
                                      (tls t)
                                      username
                                      realname
                                      password
                                      client-cert
                                      sasl-username
                                      sasl-password
                                      (reconnect t)
                                      user-data)
  "Create a new IRC connection object.
   
   SERVER - IRC server hostname
   NICK - Desired nickname
   PORT - Server port (default: 6697 for TLS, 6667 for plain)
   TLS - Use TLS/SSL (default: t)
   USERNAME - Username for USER command (default: nick)
   REALNAME - Realname for USER command
   PASSWORD - Server password
   CLIENT-CERT - Path to client certificate for SASL EXTERNAL
   SASL-USERNAME - SASL username (default: nick)
   SASL-PASSWORD - SASL password for SASL PLAIN
   RECONNECT - Enable automatic reconnection (default: t)
   USER-DATA - Arbitrary user data"
  (make-instance 'connection
                 :server server
                 :port (or port (if tls *default-tls-port* *default-port*))
                 :nick nick
                 :tls tls
                 :username (or username nick)
                 :realname (or realname *default-realname*)
                 :password password
                 :client-cert client-cert
                 :sasl-username (or sasl-username nick)
                 :sasl-password sasl-password
                 :reconnect reconnect
                 :user-data user-data))

;;; Low-level I/O

(defun send-raw (conn line)
  "Send a raw line to the IRC server (thread-safe)."
  (bt:with-lock-held ((connection-write-lock conn))
    (let ((stream (connection-stream conn)))
      (when stream
        (handler-case
            (progn
              (write-string line stream)
              (write-char #\Return stream)
              (write-char #\Linefeed stream)
              (force-output stream)
              (irc-log :debug ">>> ~A" line)
              t)
          (error (e)
            (irc-log :error "Send error: ~A" e)
            nil))))))

(defun read-line-crlf (stream)
  "Read a line terminated by CRLF from stream."
  (let ((line (make-array 512 :element-type 'character :fill-pointer 0 :adjustable t)))
    (handler-case
        (loop for char = (read-char stream nil nil)
              while char
              do (cond
                   ((char= char #\Return)
                    ;; Expect LF next
                    (let ((next (read-char stream nil nil)))
                      (when (and next (char= next #\Linefeed))
                        (return (coerce line 'string)))))
                   ((char= char #\Linefeed)
                    ;; Bare LF (some servers)
                    (return (coerce line 'string)))
                   (t
                    (vector-push-extend char line)))
              finally (return (if (> (length line) 0)
                                  (coerce line 'string)
                                  nil)))
      (error () nil))))

;;; Connection establishment

(defun connect (conn)
  "Connect to the IRC server and start registration.
   Returns T on success, signals CONNECTION-ERROR on failure."
  (when (eq (connection-state conn) :connected)
    (return-from connect t))
  
  (let ((server (connection-server conn))
        (port (connection-port conn))
        (use-tls (connection-tls-p conn))
        (client-cert (connection-client-cert conn)))
    
    (setf (connection-state conn) :connecting)
    (irc-log :info "Connecting to ~A:~A~A..." server port (if use-tls " (TLS)" ""))
    
    (handler-case
        (let ((sock (usocket:socket-connect server port :element-type '(unsigned-byte 8))))
          (setf (connection-socket conn) sock)
          (let ((raw-stream (usocket:socket-stream sock)))
            (setf (connection-stream conn)
                  (if use-tls
                      (if (and client-cert (probe-file client-cert))
                          ;; TLS with client certificate
                          (cl+ssl:make-ssl-client-stream
                           raw-stream
                           :hostname server
                           :certificate client-cert
                           :key client-cert
                           :external-format :utf-8)
                          ;; TLS without client certificate
                          (cl+ssl:make-ssl-client-stream
                           raw-stream
                           :hostname server
                           :external-format :utf-8))
                      ;; Plain connection
                      (flexi-streams:make-flexi-stream
                       raw-stream
                       :external-format :utf-8))))
          
          (setf (connection-state conn) :registering
                (connection-last-activity conn) (get-universal-time))
          
          ;; Start registration
          (start-registration conn)
          
          ;; Start read thread
          (setf (connection-read-thread conn)
                (bt:make-thread
                 (lambda () (read-loop conn))
                 :name (format nil "irc-read-~A" server)))
          
          (irc-log :info "Connected to ~A:~A" server port)
          t)
      
      (error (e)
        (setf (connection-state conn) :disconnected)
        (irc-log :error "Connection failed: ~A" e)
        (error 'connection-error :message (format nil "~A" e))))))

(defun disconnect (conn &optional (message *default-quit-message*))
  "Disconnect from the IRC server."
  (when (connection-stream conn)
    (ignore-errors
      (when message
        (send-raw conn (format-irc-line "QUIT" message))))
    (ignore-errors
      (close (connection-stream conn)))
    (ignore-errors
      (usocket:socket-close (connection-socket conn))))
  
  (setf (connection-stream conn) nil
        (connection-socket conn) nil
        (connection-state conn) :disconnected
        (connection-cap-enabled conn) nil
        (connection-cap-negotiating conn) nil
        (connection-sasl-state conn) nil)
  
  ;; Clear batches and labels
  (clrhash (connection-active-batches conn))
  (clrhash (connection-pending-labels conn))
  
  (run-hooks conn 'on-disconnect conn)
  (irc-log :info "Disconnected from ~A" (connection-server conn)))

(defun connectedp (conn)
  "Return T if connection is established."
  (eq (connection-state conn) :connected))

;;; Registration

(defun start-registration (conn)
  "Start the IRC registration process."
  ;; Request capabilities
  (setf (connection-cap-negotiating conn) t)
  (send-raw conn "CAP LS 302")
  
  ;; Send PASS if we have a password
  (when (connection-password conn)
    (send-raw conn (format-irc-line "PASS" (connection-password conn))))
  
  ;; Send NICK and USER
  (send-raw conn (format-irc-line "NICK" (connection-nick conn)))
  (send-raw conn (format-irc-line "USER"
                                  (connection-username conn)
                                  "0" "*"
                                  (connection-realname conn))))

;;; Read loop

(defun read-loop (conn)
  "Main read loop - runs in a separate thread."
  (unwind-protect
       (handler-case
           (loop while (and (connection-stream conn)
                            (not (eq (connection-state conn) :disconnected)))
                 for line = (read-line-crlf (connection-stream conn))
                 while line
                 do (setf (connection-last-activity conn) (get-universal-time))
                    (irc-log :debug "<<< ~A" line)
                    (handler-case
                        (handle-message conn (parse-message line))
                      (error (e)
                        (irc-log :error "Error handling message: ~A" e))))
         (end-of-file ()
           (irc-log :info "Connection closed by server"))
         (error (e)
           (irc-log :error "Read error: ~A" e)))
    ;; Cleanup
    (when (not (eq (connection-state conn) :disconnected))
      (setf (connection-state conn) :disconnected)
      (run-hooks conn 'on-disconnect conn)
      ;; Attempt reconnection if enabled
      (when (connection-reconnect-enabled conn)
        (schedule-reconnect conn)))))

(defun schedule-reconnect (conn)
  "Schedule a reconnection attempt."
  (let* ((attempts (incf (connection-reconnect-attempts conn)))
         (delay (min (* *reconnect-base-delay* (expt 2 (1- attempts)))
                     *reconnect-max-delay*)))
    (when (or (null *reconnect-max-attempts*)
              (<= attempts *reconnect-max-attempts*))
      (irc-log :info "Reconnecting in ~A seconds (attempt ~A)..." delay attempts)
      (bt:make-thread
       (lambda ()
         (sleep delay)
         (handler-case
             (connect conn)
           (error (e)
             (irc-log :error "Reconnection failed: ~A" e)
             (schedule-reconnect conn))))
       :name "irc-reconnect"))))

;;; Hook system

(defun add-hook (conn hook-name function &key (priority :normal))
  "Add a hook function for HOOK-NAME.
   FUNCTION receives (connection message) for message hooks,
   or (connection &rest args) for other hooks.
   PRIORITY can be :first, :normal, or :last."
  (declare (ignore priority))  ; TODO: implement priority
  (push function (gethash hook-name (connection-hooks conn))))

(defun remove-hook (conn hook-name function)
  "Remove a specific hook function."
  (setf (gethash hook-name (connection-hooks conn))
        (remove function (gethash hook-name (connection-hooks conn)))))

(defun remove-all-hooks (conn &optional hook-name)
  "Remove all hooks, or all hooks for HOOK-NAME if specified."
  (if hook-name
      (remhash hook-name (connection-hooks conn))
      (clrhash (connection-hooks conn))))

(defun run-hooks (conn hook-name &rest args)
  "Run all hooks for HOOK-NAME with ARGS."
  (dolist (fn (gethash hook-name (connection-hooks conn)))
    (handler-case
        (apply fn args)
      (error (e)
        (irc-log :error "Hook error (~A): ~A" hook-name e)))))

;;; Capability negotiation

(defun cap-enabled-p (conn capability)
  "Check if a capability is enabled."
  (member capability (connection-cap-enabled conn) :test #'string-equal))

(defun enabled-capabilities (conn)
  "Return list of enabled capabilities."
  (copy-list (connection-cap-enabled conn)))

;;; Convenience macro

(defmacro with-connection ((var server nick &rest options) &body body)
  "Execute BODY with a connected IRC connection bound to VAR.
   Automatically disconnects when done."
  `(let ((,var (make-connection ,server ,nick ,@options)))
     (unwind-protect
          (progn
            (connect ,var)
            ,@body)
       (disconnect ,var))))
