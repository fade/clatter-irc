;;;; dcc.lisp - DCC (Direct Client-to-Client) protocol support

(in-package #:clatter-irc)

;;; DCC Manager

(defvar *dcc-manager* nil
  "Global DCC manager instance.")

(defclass dcc-manager ()
  ((connections :initform (make-hash-table :test 'equal)
                :accessor dcc-manager-connections
                :documentation "Hash table of id -> dcc-connection")
   (next-id :initform 1
            :accessor dcc-manager-next-id
            :documentation "Next connection ID to assign")
   (irc-connection :initarg :irc-connection
                   :accessor dcc-manager-irc-connection
                   :documentation "Associated IRC connection"))
  (:documentation "Manages all DCC connections."))

(defun make-dcc-manager (&optional irc-connection)
  "Create a new DCC manager."
  (setf *dcc-manager* (make-instance 'dcc-manager :irc-connection irc-connection)))

(defun dcc-manager-add (manager connection)
  "Add a connection to the manager, assigning it an ID."
  (let ((id (dcc-manager-next-id manager)))
    (setf (dcc-connection-id connection) id)
    (setf (gethash id (dcc-manager-connections manager)) connection)
    (incf (dcc-manager-next-id manager))
    id))

(defun dcc-manager-remove (manager id)
  "Remove a connection from the manager."
  (remhash id (dcc-manager-connections manager)))

(defun dcc-manager-find (manager id)
  "Find a connection by ID."
  (gethash id (dcc-manager-connections manager)))

(defun dcc-list (manager)
  "List all DCC connections."
  (loop for conn being the hash-values of (dcc-manager-connections manager)
        collect conn))

(defun dcc-manager-pending (manager)
  "List pending (awaiting accept/reject) connections."
  (loop for conn being the hash-values of (dcc-manager-connections manager)
        when (eq (dcc-connection-state conn) :pending)
          collect conn))

;;; DCC Connection Base Class

(defclass dcc-connection ()
  ((id :initform nil
       :accessor dcc-connection-id
       :documentation "Unique ID for this connection")
   (nick :initarg :nick
         :accessor dcc-connection-nick
         :documentation "Remote user's nick")
   (remote-ip :initarg :remote-ip
              :initform nil
              :accessor dcc-connection-remote-ip
              :documentation "Remote IP address (as integer or string)")
   (remote-port :initarg :remote-port
                :initform nil
                :accessor dcc-connection-remote-port
                :documentation "Remote port")
   (direction :initarg :direction
              :accessor dcc-connection-direction
              :documentation "Direction: :incoming or :outgoing")
   (state :initform :pending
          :accessor dcc-connection-state
          :documentation "State: :pending :connecting :connected :transferring :completed :error")
   (socket :initform nil
           :accessor dcc-connection-socket)
   (stream :initform nil
           :accessor dcc-connection-stream)
   (thread :initform nil
           :accessor dcc-connection-thread)
   (error-message :initform nil
                  :accessor dcc-connection-error-message)
   (created-time :initform (get-universal-time)
                 :accessor dcc-connection-created-time))
  (:documentation "Base class for DCC connections"))

;;; DCC CHAT

(defclass dcc-chat (dcc-connection)
  ((on-message :initarg :on-message
               :initform nil
               :accessor dcc-chat-on-message
               :documentation "Callback for received messages: (lambda (chat message))"))
  (:documentation "DCC CHAT connection"))

;;; DCC SEND/RECEIVE

(defclass dcc-send (dcc-connection)
  ((filename :initarg :filename
             :accessor dcc-send-filename
             :documentation "Name of the file")
   (filepath :initarg :filepath
             :initform nil
             :accessor dcc-send-filepath
             :documentation "Full path to the file")
   (filesize :initarg :filesize
             :initform 0
             :accessor dcc-send-filesize
             :documentation "Size of the file in bytes")
   (bytes-transferred :initform 0
                      :accessor dcc-send-bytes-transferred
                      :documentation "Bytes transferred so far")
   (on-progress :initarg :on-progress
                :initform nil
                :accessor dcc-send-on-progress
                :documentation "Progress callback: (lambda (send bytes-transferred total))"))
  (:documentation "DCC SEND/RECEIVE connection"))

;;; IP utilities

(defvar *dcc-local-ip* nil
  "Cached local IP address for DCC. Set via set-dcc-ip or auto-detected.")

(defun detect-local-ip ()
  "Try to detect local IP by connecting to a remote host."
  (handler-case
      (let ((socket (usocket:socket-connect "8.8.8.8" 53 
                                            :protocol :datagram
                                            :element-type '(unsigned-byte 8))))
        (unwind-protect
            (let ((local-addr (usocket:get-local-name socket)))
              (when local-addr
                (usocket:host-to-hostname local-addr)))
          (usocket:socket-close socket)))
    (error () nil)))

(defun get-local-ip ()
  "Get local IP address for DCC."
  (or *dcc-local-ip*
      (detect-local-ip)
      "127.0.0.1"))

(defun set-dcc-ip (ip-string)
  "Manually set the DCC IP address."
  (setf *dcc-local-ip* ip-string))

(defun find-available-port ()
  "Find an available port in the DCC port range."
  (loop for port from *dcc-port-range-start* to *dcc-port-range-end*
        do (handler-case
               (let ((socket (usocket:socket-listen usocket:*wildcard-host* port)))
                 (usocket:socket-close socket)
                 (return port))
             (error () nil))
        finally (return nil)))

;;; DCC CHAT operations

(defun dcc-initiate-chat (manager nick &optional irc-conn)
  "Initiate a DCC CHAT with nick."
  (let* ((local-ip (get-local-ip))
         (port (find-available-port))
         (conn (or irc-conn (dcc-manager-irc-connection manager))))
    (unless port
      (irc-log :error "No available port for DCC CHAT")
      (return-from dcc-initiate-chat nil))
    (handler-case
        (let* ((listener (usocket:socket-listen usocket:*wildcard-host* port :reuse-address t))
               (chat (make-instance 'dcc-chat
                                    :nick nick
                                    :direction :outgoing)))
          (setf (dcc-connection-socket chat) listener)
          (dcc-manager-add manager chat)
          ;; Send CTCP DCC CHAT
          (let* ((ip-int (ip-string-to-integer local-ip))
                 (ctcp-msg (format nil "~CDCC CHAT chat ~A ~A~C"
                                   (code-char 1) ip-int port (code-char 1))))
            (when conn
              (send-raw conn (format-irc-line "PRIVMSG" nick ctcp-msg))))
          (irc-log :info "DCC CHAT request sent to ~A" nick)
          ;; Start listener thread
          (setf (dcc-connection-thread chat)
                (bt:make-thread
                 (lambda () (dcc-chat-listen chat manager listener))
                 :name (format nil "dcc-chat-listen-~A" nick)))
          chat)
      (error (e)
        (irc-log :error "Failed to initiate DCC CHAT: ~A" e)
        nil))))

(defun dcc-chat-listen (chat manager listener)
  "Wait for incoming connection on DCC CHAT listener."
  (handler-case
      (let ((socket (usocket:socket-accept listener :element-type '(unsigned-byte 8))))
        (usocket:socket-close listener)
        (setf (dcc-connection-socket chat) socket
              (dcc-connection-stream chat) (flexi-streams:make-flexi-stream
                                            (usocket:socket-stream socket)
                                            :external-format :utf-8)
              (dcc-connection-state chat) :connected)
        (irc-log :info "DCC CHAT with ~A established" (dcc-connection-nick chat))
        ;; Start read loop
        (dcc-chat-read-loop chat manager))
    (error (e)
      (setf (dcc-connection-state chat) :error
            (dcc-connection-error-message chat) (format nil "~A" e))
      (irc-log :error "DCC CHAT listen failed: ~A" e))))

(defun dcc-chat-read-loop (chat manager)
  "Read messages from DCC CHAT connection."
  (declare (ignore manager))
  (handler-case
      (loop for line = (read-line (dcc-connection-stream chat) nil nil)
            while line
            do (when (dcc-chat-on-message chat)
                 (funcall (dcc-chat-on-message chat) chat line)))
    (error (e)
      (setf (dcc-connection-state chat) :error
            (dcc-connection-error-message chat) (format nil "~A" e)))))

(defun dcc-chat-send (chat message)
  "Send a message on a DCC CHAT connection."
  (when (and (eq (dcc-connection-state chat) :connected)
             (dcc-connection-stream chat))
    (write-line message (dcc-connection-stream chat))
    (force-output (dcc-connection-stream chat))))

;;; DCC SEND operations

(defun dcc-initiate-send (manager nick filepath &optional irc-conn)
  "Initiate a DCC SEND to nick."
  (handler-case
      (progn
        (unless (probe-file filepath)
          (irc-log :error "File not found: ~A" filepath)
          (return-from dcc-initiate-send nil))
        (let* ((local-ip (get-local-ip))
               (port (find-available-port))
               (filename (file-namestring filepath))
               (filesize (with-open-file (f filepath) (file-length f)))
               (conn (or irc-conn (dcc-manager-irc-connection manager))))
          (unless port
            (irc-log :error "No available port for DCC SEND")
            (return-from dcc-initiate-send nil))
          (let* ((listener (usocket:socket-listen usocket:*wildcard-host* port :reuse-address t))
                 (send (make-instance 'dcc-send
                                      :nick nick
                                      :direction :outgoing
                                      :filename filename
                                      :filepath filepath
                                      :filesize filesize)))
            (setf (dcc-connection-socket send) listener)
            (dcc-manager-add manager send)
            ;; Send CTCP DCC SEND
            (let* ((ip-int (ip-string-to-integer local-ip))
                   (ctcp-msg (format nil "~CDCC SEND ~A ~A ~A ~A~C"
                                     (code-char 1) filename ip-int port filesize (code-char 1))))
              (when conn
                (send-raw conn (format-irc-line "PRIVMSG" nick ctcp-msg))))
            (irc-log :info "DCC SEND ~A to ~A (~A bytes)" filename nick filesize)
            ;; Start listener thread
            (setf (dcc-connection-thread send)
                  (bt:make-thread
                   (lambda () (dcc-send-listen send manager listener))
                   :name (format nil "dcc-send-listen-~A" filename)))
            send)))
    (error (e)
      (irc-log :error "Failed to initiate DCC SEND: ~A" e)
      nil)))

(defun dcc-send-listen (send manager listener)
  "Wait for incoming connection on DCC SEND listener."
  (declare (ignore manager))
  (irc-log :info "DCC SEND waiting for connection...")
  (handler-case
      (let ((socket (usocket:socket-accept listener :element-type '(unsigned-byte 8))))
        (usocket:socket-close listener)
        (setf (dcc-connection-socket send) socket
              (dcc-connection-stream send) (usocket:socket-stream socket)
              (dcc-connection-state send) :transferring)
        (irc-log :info "DCC SEND connection accepted")
        ;; Start sending
        (dcc-send-file send))
    (error (e)
      (setf (dcc-connection-state send) :error
            (dcc-connection-error-message send) (format nil "~A" e))
      (irc-log :error "DCC SEND listen failed: ~A" e))))

(defun dcc-send-file (send)
  "Send the file over DCC."
  (handler-case
      (with-open-file (file (dcc-send-filepath send) :element-type '(unsigned-byte 8))
        (let ((buffer (make-array 4096 :element-type '(unsigned-byte 8)))
              (stream (dcc-connection-stream send)))
          (loop for bytes-read = (read-sequence buffer file)
                while (> bytes-read 0)
                do (write-sequence buffer stream :end bytes-read)
                   (force-output stream)
                   (incf (dcc-send-bytes-transferred send) bytes-read)
                   (when (dcc-send-on-progress send)
                     (funcall (dcc-send-on-progress send)
                              send
                              (dcc-send-bytes-transferred send)
                              (dcc-send-filesize send))))
          (setf (dcc-connection-state send) :completed)
          (irc-log :info "DCC SEND completed: ~A" (dcc-send-filename send))))
    (error (e)
      (setf (dcc-connection-state send) :error
            (dcc-connection-error-message send) (format nil "~A" e))
      (irc-log :error "DCC SEND failed: ~A" e)))
  ;; Cleanup
  (ignore-errors (close (dcc-connection-stream send)))
  (ignore-errors (usocket:socket-close (dcc-connection-socket send))))

;;; DCC RECEIVE operations

(defun dcc-accept (manager id &optional save-path)
  "Accept a pending DCC offer."
  (let ((conn (dcc-manager-find manager id)))
    (unless conn
      (irc-log :error "DCC connection ~A not found" id)
      (return-from dcc-accept nil))
    (unless (eq (dcc-connection-state conn) :pending)
      (irc-log :error "DCC connection ~A not pending" id)
      (return-from dcc-accept nil))
    (etypecase conn
      (dcc-chat
       (dcc-accept-chat conn manager))
      (dcc-send
       (dcc-accept-send conn manager save-path)))))

(defun dcc-accept-chat (chat manager)
  "Accept a DCC CHAT offer."
  (declare (ignore manager))
  (setf (dcc-connection-state chat) :connecting)
  (setf (dcc-connection-thread chat)
        (bt:make-thread
         (lambda ()
           (handler-case
               (let* ((ip (if (integerp (dcc-connection-remote-ip chat))
                              (ip-integer-to-string (dcc-connection-remote-ip chat))
                              (dcc-connection-remote-ip chat)))
                      (port (dcc-connection-remote-port chat))
                      (socket (usocket:socket-connect ip port :element-type '(unsigned-byte 8))))
                 (setf (dcc-connection-socket chat) socket
                       (dcc-connection-stream chat) (flexi-streams:make-flexi-stream
                                                     (usocket:socket-stream socket)
                                                     :external-format :utf-8)
                       (dcc-connection-state chat) :connected)
                 (irc-log :info "DCC CHAT connected to ~A" (dcc-connection-nick chat))
                 (dcc-chat-read-loop chat nil))
             (error (e)
               (setf (dcc-connection-state chat) :error
                     (dcc-connection-error-message chat) (format nil "~A" e))
               (irc-log :error "DCC CHAT connect failed: ~A" e))))
         :name (format nil "dcc-chat-~A" (dcc-connection-nick chat))))
  chat)

(defun dcc-accept-send (send manager save-path)
  "Accept a DCC SEND offer (receive a file)."
  (declare (ignore manager))
  (let ((filepath (or save-path
                      (merge-pathnames (dcc-send-filename send)
                                       (or *dcc-download-directory*
                                           (user-homedir-pathname))))))
    (setf (dcc-send-filepath send) filepath
          (dcc-connection-state send) :connecting)
    (setf (dcc-connection-thread send)
          (bt:make-thread
           (lambda ()
             (handler-case
                 (let* ((ip (if (integerp (dcc-connection-remote-ip send))
                                (ip-integer-to-string (dcc-connection-remote-ip send))
                                (dcc-connection-remote-ip send)))
                        (port (dcc-connection-remote-port send))
                        (socket (usocket:socket-connect ip port :element-type '(unsigned-byte 8))))
                   (setf (dcc-connection-socket send) socket
                         (dcc-connection-stream send) (usocket:socket-stream socket)
                         (dcc-connection-state send) :transferring)
                   (irc-log :info "DCC RECV connecting to ~A:~A" ip port)
                   (dcc-receive-file send))
               (error (e)
                 (setf (dcc-connection-state send) :error
                       (dcc-connection-error-message send) (format nil "~A" e))
                 (irc-log :error "DCC RECV failed: ~A" e))))
           :name (format nil "dcc-recv-~A" (dcc-send-filename send))))
    send))

(defun dcc-receive-file (send)
  "Receive a file over DCC."
  (handler-case
      (with-open-file (file (dcc-send-filepath send)
                            :direction :output
                            :element-type '(unsigned-byte 8)
                            :if-exists :supersede)
        (let ((buffer (make-array 4096 :element-type '(unsigned-byte 8)))
              (stream (dcc-connection-stream send))
              (total (dcc-send-filesize send)))
          (loop for bytes-read = (read-sequence buffer stream)
                while (> bytes-read 0)
                do (write-sequence buffer file :end bytes-read)
                   (incf (dcc-send-bytes-transferred send) bytes-read)
                   (when (dcc-send-on-progress send)
                     (funcall (dcc-send-on-progress send)
                              send
                              (dcc-send-bytes-transferred send)
                              total))
                until (>= (dcc-send-bytes-transferred send) total))
          (setf (dcc-connection-state send) :completed)
          (irc-log :info "DCC RECV completed: ~A" (dcc-send-filename send))))
    (error (e)
      (setf (dcc-connection-state send) :error
            (dcc-connection-error-message send) (format nil "~A" e))
      (irc-log :error "DCC RECV failed: ~A" e)))
  ;; Cleanup
  (ignore-errors (close (dcc-connection-stream send)))
  (ignore-errors (usocket:socket-close (dcc-connection-socket send))))

;;; DCC reject/close

(defun dcc-reject (manager id)
  "Reject a pending DCC offer."
  (let ((conn (dcc-manager-find manager id)))
    (when conn
      (setf (dcc-connection-state conn) :error
            (dcc-connection-error-message conn) "Rejected")
      (dcc-manager-remove manager id))))

(defun dcc-close (manager id)
  "Close a DCC connection."
  (let ((conn (dcc-manager-find manager id)))
    (when conn
      (ignore-errors
        (when (dcc-connection-stream conn)
          (close (dcc-connection-stream conn))))
      (ignore-errors
        (when (dcc-connection-socket conn)
          (usocket:socket-close (dcc-connection-socket conn))))
      (when (dcc-connection-thread conn)
        (ignore-errors (bt:destroy-thread (dcc-connection-thread conn))))
      (dcc-manager-remove manager id))))

;;; Handle incoming DCC offers

(defun dcc-handle-offer (manager sender dcc-type dcc-args)
  "Handle an incoming DCC offer from CTCP."
  (let ((type (string-upcase dcc-type)))
    (cond
      ((string= type "CHAT")
       (dcc-handle-chat-offer manager sender dcc-args))
      ((string= type "SEND")
       (dcc-handle-send-offer manager sender dcc-args))
      (t
       (irc-log :warn "Unknown DCC type: ~A" type)))))

(defun dcc-handle-chat-offer (manager sender args)
  "Handle incoming DCC CHAT offer."
  ;; Format: chat <ip> <port>
  (let* ((parts (split-string args #\Space))
         (ip-int (parse-integer (second parts) :junk-allowed t))
         (port (parse-integer (third parts) :junk-allowed t)))
    (when (and ip-int port)
      (let ((chat (make-instance 'dcc-chat
                                 :nick sender
                                 :direction :incoming
                                 :remote-ip ip-int
                                 :remote-port port)))
        (dcc-manager-add manager chat)
        (irc-log :info "DCC CHAT offer from ~A (use dcc-accept ~A)"
                 sender (dcc-connection-id chat))
        chat))))

(defun dcc-handle-send-offer (manager sender args)
  "Handle incoming DCC SEND offer."
  ;; Format: <filename> <ip> <port> <size>
  (let* ((parts (split-string args #\Space))
         (filename (first parts))
         (ip-int (parse-integer (second parts) :junk-allowed t))
         (port (parse-integer (third parts) :junk-allowed t))
         (filesize (parse-integer (fourth parts) :junk-allowed t)))
    (when (and filename ip-int port)
      (let ((send (make-instance 'dcc-send
                                 :nick sender
                                 :direction :incoming
                                 :filename filename
                                 :filesize (or filesize 0)
                                 :remote-ip ip-int
                                 :remote-port port)))
        (dcc-manager-add manager send)
        (irc-log :info "DCC SEND offer: ~A from ~A (~A bytes) (use dcc-accept ~A)"
                 filename sender (or filesize "?") (dcc-connection-id send))
        send))))
