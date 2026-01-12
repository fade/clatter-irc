;;;; handlers.lisp - IRC message handlers

(in-package #:clatter-irc)

;;; Main message dispatcher

(defun handle-message (conn msg)
  "Handle an incoming IRC message."
  (when msg
    ;; Run raw message hook first
    (run-hooks conn 'on-raw conn msg)
    
    ;; Dispatch based on command
    (let ((command (message-command msg)))
      (cond
        ;; Numeric replies
        ((every #'digit-char-p command)
         (handle-numeric conn msg (parse-integer command)))
        
        ;; Named commands
        ((string= command "PING")
         (handle-ping conn msg))
        ((string= command "PONG")
         (handle-pong conn msg))
        ((string= command "PRIVMSG")
         (handle-privmsg conn msg))
        ((string= command "NOTICE")
         (handle-notice conn msg))
        ((string= command "JOIN")
         (handle-join conn msg))
        ((string= command "PART")
         (handle-part conn msg))
        ((string= command "QUIT")
         (handle-quit conn msg))
        ((string= command "KICK")
         (handle-kick conn msg))
        ((string= command "NICK")
         (handle-nick-change conn msg))
        ((string= command "MODE")
         (handle-mode conn msg))
        ((string= command "TOPIC")
         (handle-topic conn msg))
        ((string= command "INVITE")
         (handle-invite conn msg))
        ((string= command "CAP")
         (handle-cap conn msg))
        ((string= command "AUTHENTICATE")
         (handle-authenticate conn msg))
        ((string= command "BATCH")
         (handle-batch conn msg))
        ((string= command "ERROR")
         (handle-error conn msg))
        (t
         ;; Unknown command - just run hooks
         (run-hooks conn 'on-message conn msg))))))

;;; PING/PONG

(defun handle-ping (conn msg)
  "Respond to PING with PONG."
  (let ((server (first (message-params msg))))
    (pong conn server)))

(defun handle-pong (conn msg)
  "Handle PONG response."
  (declare (ignore msg))
  (setf (connection-ping-sent-time conn) nil))

;;; PRIVMSG and NOTICE

(defun handle-privmsg (conn msg)
  "Handle PRIVMSG - regular messages and CTCP."
  (let* ((params (message-params msg))
         (target (first params))
         (text (second params))
         (prefix (parse-prefix (message-prefix msg)))
         (sender (when prefix (prefix-nick prefix))))
    (if (and text (> (length text) 1)
             (char= (char text 0) (code-char 1))
             (char= (char text (1- (length text))) (code-char 1)))
        ;; CTCP request
        (handle-ctcp conn msg sender target (subseq text 1 (1- (length text))))
        ;; Regular message
        (run-hooks conn 'on-privmsg conn msg sender target text))))

(defun handle-notice (conn msg)
  "Handle NOTICE - regular notices and CTCP replies."
  (let* ((params (message-params msg))
         (target (first params))
         (text (second params))
         (prefix (parse-prefix (message-prefix msg)))
         (sender (when prefix (prefix-nick prefix))))
    (if (and text (> (length text) 1)
             (char= (char text 0) (code-char 1))
             (char= (char text (1- (length text))) (code-char 1)))
        ;; CTCP reply
        (handle-ctcp-reply conn msg sender (subseq text 1 (1- (length text))))
        ;; Regular notice
        (run-hooks conn 'on-notice conn msg sender target text))))

;;; CTCP handling

(defun handle-ctcp (conn msg sender target ctcp-content)
  "Handle CTCP request."
  (let* ((space-pos (position #\Space ctcp-content))
         (ctcp-cmd (string-upcase (if space-pos
                                      (subseq ctcp-content 0 space-pos)
                                      ctcp-content)))
         (ctcp-args (if space-pos
                        (subseq ctcp-content (1+ space-pos))
                        "")))
    (cond
      ;; VERSION - reply with client version
      ((string= ctcp-cmd "VERSION")
       (ctcp-reply conn sender "VERSION" "clatter-irc 0.1.0"))
      
      ;; PING - echo back
      ((string= ctcp-cmd "PING")
       (ctcp-reply conn sender "PING" ctcp-args))
      
      ;; TIME - reply with current time
      ((string= ctcp-cmd "TIME")
       (ctcp-reply conn sender "TIME" (format-datetime)))
      
      ;; DCC - handle DCC offers
      ((string= ctcp-cmd "DCC")
       (run-hooks conn 'on-dcc conn msg sender ctcp-args))
      
      ;; Other CTCP
      (t
       (run-hooks conn 'on-ctcp conn msg sender target ctcp-cmd ctcp-args)))))

(defun handle-ctcp-reply (conn msg sender ctcp-content)
  "Handle CTCP reply."
  (let* ((space-pos (position #\Space ctcp-content))
         (ctcp-cmd (string-upcase (if space-pos
                                      (subseq ctcp-content 0 space-pos)
                                      ctcp-content)))
         (ctcp-args (if space-pos
                        (subseq ctcp-content (1+ space-pos))
                        "")))
    (run-hooks conn 'on-ctcp-reply conn msg sender ctcp-cmd ctcp-args)))

;;; Channel events

(defun handle-join (conn msg)
  "Handle JOIN message."
  (let* ((prefix (parse-prefix (message-prefix msg)))
         (nick (when prefix (prefix-nick prefix)))
         (channel (first (message-params msg))))
    ;; Track our own joins
    (when (nick-equal nick (connection-nick conn))
      (setf (gethash (normalize-channel channel) (connection-channels conn)) t))
    (run-hooks conn 'on-join conn msg nick channel)))

(defun handle-part (conn msg)
  "Handle PART message."
  (let* ((prefix (parse-prefix (message-prefix msg)))
         (nick (when prefix (prefix-nick prefix)))
         (params (message-params msg))
         (channel (first params))
         (reason (second params)))
    ;; Track our own parts
    (when (nick-equal nick (connection-nick conn))
      (remhash (normalize-channel channel) (connection-channels conn)))
    (run-hooks conn 'on-part conn msg nick channel reason)))

(defun handle-quit (conn msg)
  "Handle QUIT message."
  (let* ((prefix (parse-prefix (message-prefix msg)))
         (nick (when prefix (prefix-nick prefix)))
         (reason (first (message-params msg))))
    (run-hooks conn 'on-quit conn msg nick reason)))

(defun handle-kick (conn msg)
  "Handle KICK message."
  (let* ((prefix (parse-prefix (message-prefix msg)))
         (kicker (when prefix (prefix-nick prefix)))
         (params (message-params msg))
         (channel (first params))
         (kicked (second params))
         (reason (third params)))
    ;; Track if we were kicked
    (when (nick-equal kicked (connection-nick conn))
      (remhash (normalize-channel channel) (connection-channels conn)))
    (run-hooks conn 'on-kick conn msg kicker channel kicked reason)))

(defun handle-nick-change (conn msg)
  "Handle NICK message."
  (let* ((prefix (parse-prefix (message-prefix msg)))
         (old-nick (when prefix (prefix-nick prefix)))
         (new-nick (first (message-params msg))))
    ;; Track our own nick changes
    (when (nick-equal old-nick (connection-nick conn))
      (setf (connection-nick conn) new-nick))
    (run-hooks conn 'on-nick conn msg old-nick new-nick)))

(defun handle-mode (conn msg)
  "Handle MODE message."
  (let* ((params (message-params msg))
         (target (first params))
         (modes (rest params)))
    (run-hooks conn 'on-mode conn msg target modes)))

(defun handle-topic (conn msg)
  "Handle TOPIC message."
  (let* ((prefix (parse-prefix (message-prefix msg)))
         (setter (when prefix (prefix-nick prefix)))
         (params (message-params msg))
         (channel (first params))
         (topic (second params)))
    (run-hooks conn 'on-topic conn msg setter channel topic)))

(defun handle-invite (conn msg)
  "Handle INVITE message."
  (let* ((prefix (parse-prefix (message-prefix msg)))
         (inviter (when prefix (prefix-nick prefix)))
         (params (message-params msg))
         (invited (first params))
         (channel (second params)))
    (run-hooks conn 'on-invite conn msg inviter invited channel)))

;;; Numeric replies

(defun handle-numeric (conn msg code)
  "Handle numeric reply."
  (let ((name (reply-code-name code)))
    ;; Handle specific numerics
    (case code
      ;; Welcome - registration complete
      (001
       (setf (connection-state conn) :connected
             (connection-reconnect-attempts conn) 0)
       (run-hooks conn 'on-connect conn))
      
      ;; Nick in use
      (433
       (let ((new-nick (concatenate 'string (connection-nick conn) "_")))
         (setf (connection-nick conn) new-nick)
         (nick conn new-nick)))
      
      ;; SASL success
      (903
       (setf (connection-sasl-state conn) :done)
       (when (connection-cap-negotiating conn)
         (cap-end conn)))
      
      ;; SASL failure
      ((904 905 906)
       (setf (connection-sasl-state conn) :failed)
       (irc-log :error "SASL authentication failed")
       (when (connection-cap-negotiating conn)
         (cap-end conn))))
    
    ;; Run generic numeric hook
    (run-hooks conn 'on-numeric conn msg code name)))

;;; CAP (capability negotiation)

(defun handle-cap (conn msg)
  "Handle CAP message for IRCv3 capability negotiation."
  (let* ((params (message-params msg))
         (subcommand (second params))
         (caps-string (car (last params))))
    (cond
      ;; LS - server listing available capabilities
      ((string-equal subcommand "LS")
       (let* ((available (split-string caps-string #\Space))
              (wanted (intersection *wanted-capabilities* available :test #'string-equal)))
         (if wanted
             (send-raw conn (format nil "CAP REQ :~{~A~^ ~}" wanted))
             (cap-end conn))))
      
      ;; ACK - server acknowledged our request
      ((string-equal subcommand "ACK")
       (let ((acked (split-string caps-string #\Space)))
         (dolist (cap acked)
           (pushnew cap (connection-cap-enabled conn) :test #'string-equal))
         ;; Start SASL if enabled and we have credentials
         (if (and (cap-enabled-p conn "sasl")
                  (or (connection-sasl-password conn)
                      (connection-client-cert conn)))
             (if (connection-client-cert conn)
                 (sasl-external conn)
                 (sasl-plain conn (connection-sasl-username conn)
                             (connection-sasl-password conn)))
             ;; No SASL, end CAP negotiation
             (progn
               (setf (connection-cap-negotiating conn) nil)
               (cap-end conn)))))
      
      ;; NAK - server rejected our request
      ((string-equal subcommand "NAK")
       (irc-log :warn "CAP NAK: ~A" caps-string)
       (setf (connection-cap-negotiating conn) nil)
       (cap-end conn))
      
      ;; NEW - server advertising new capabilities (cap-notify)
      ((string-equal subcommand "NEW")
       (let* ((available (split-string caps-string #\Space))
              (wanted (intersection *wanted-capabilities* available :test #'string-equal)))
         (when wanted
           (send-raw conn (format nil "CAP REQ :~{~A~^ ~}" wanted)))))
      
      ;; DEL - server removing capabilities
      ((string-equal subcommand "DEL")
       (let ((removed (split-string caps-string #\Space)))
         (setf (connection-cap-enabled conn)
               (set-difference (connection-cap-enabled conn) removed :test #'string-equal)))))))

;;; AUTHENTICATE (SASL)

(defun handle-authenticate (conn msg)
  "Handle AUTHENTICATE message during SASL."
  (let ((param (first (message-params msg))))
    (when (string= param "+")
      ;; Server ready for credentials
      (if (connection-client-cert conn)
          (send-sasl-external-credentials conn)
          (send-sasl-plain-credentials conn)))))

;;; BATCH (IRCv3)

(defun handle-batch (conn msg)
  "Handle BATCH message for message batching."
  (let* ((params (message-params msg))
         (batch-ref (first params))
         (batch-type (second params)))
    (if (and (> (length batch-ref) 0)
             (char= (char batch-ref 0) #\+))
        ;; Starting a batch
        (let ((batch-id (subseq batch-ref 1)))
          (setf (gethash batch-id (connection-active-batches conn))
                (list :type batch-type :messages nil)))
        ;; Ending a batch
        (let* ((batch-id (subseq batch-ref 1))
               (batch (gethash batch-id (connection-active-batches conn))))
          (when batch
            (remhash batch-id (connection-active-batches conn))
            ;; Process batch messages
            (run-hooks conn 'on-batch conn (getf batch :type) 
                       (nreverse (getf batch :messages))))))))

;;; ERROR

(defun handle-error (conn msg)
  "Handle ERROR message from server."
  (let ((error-msg (first (message-params msg))))
    (irc-log :error "Server error: ~A" error-msg)
    (run-hooks conn 'on-error conn msg error-msg)))
