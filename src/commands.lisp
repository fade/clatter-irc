;;;; commands.lisp - IRC command functions

(in-package #:clatter-irc)

;;; Basic IRC commands

(defun nick (conn nickname)
  "Change nickname."
  (send-raw conn (format-irc-line "NICK" nickname)))

(defun user- (conn username realname)
  "Send USER command."
  (send-raw conn (format-irc-line "USER" username "0" "*" realname)))

(defun pass (conn password)
  "Send server password."
  (send-raw conn (format-irc-line "PASS" password)))

(defun join (conn channel &optional key)
  "Join a channel."
  (if key
      (send-raw conn (format-irc-line "JOIN" channel key))
      (send-raw conn (format-irc-line "JOIN" channel))))

(defun part (conn channel &optional message)
  "Leave a channel."
  (if message
      (send-raw conn (format-irc-line "PART" channel message))
      (send-raw conn (format-irc-line "PART" channel))))

(defun quit (conn &optional (message *default-quit-message*))
  "Quit from the server."
  (send-raw conn (format-irc-line "QUIT" message)))

(defun privmsg (conn target message)
  "Send a message to a target (channel or nick)."
  (let ((sanitized (sanitize-input message)))
    (when (and sanitized (> (length sanitized) 0))
      (send-raw conn (format-irc-line "PRIVMSG" target sanitized)))))

(defun notice (conn target message)
  "Send a notice to a target."
  (let ((sanitized (sanitize-input message)))
    (when (and sanitized (> (length sanitized) 0))
      (send-raw conn (format-irc-line "NOTICE" target sanitized)))))

(defun kick (conn channel nick &optional reason)
  "Kick a user from a channel."
  (if reason
      (send-raw conn (format-irc-line "KICK" channel nick reason))
      (send-raw conn (format-irc-line "KICK" channel nick))))

(defun mode (conn target &optional mode &rest args)
  "Set or query modes."
  (cond
    ((and mode args)
     (send-raw conn (apply #'format-irc-line "MODE" target mode args)))
    (mode
     (send-raw conn (format-irc-line "MODE" target mode)))
    (t
     (send-raw conn (format-irc-line "MODE" target)))))

(defun topic (conn channel &optional new-topic)
  "Get or set channel topic."
  (if new-topic
      (send-raw conn (format-irc-line "TOPIC" channel new-topic))
      (send-raw conn (format-irc-line "TOPIC" channel))))

(defun invite (conn nick channel)
  "Invite a user to a channel."
  (send-raw conn (format-irc-line "INVITE" nick channel)))

(defun names (conn &optional channel)
  "Request names list for a channel."
  (if channel
      (send-raw conn (format-irc-line "NAMES" channel))
      (send-raw conn "NAMES")))

(defun list-channels (conn &optional pattern)
  "Request channel list."
  (if pattern
      (send-raw conn (format-irc-line "LIST" pattern))
      (send-raw conn "LIST")))

(defun who (conn mask &optional operators-only)
  "Request WHO information."
  (if operators-only
      (send-raw conn (format-irc-line "WHO" mask "o"))
      (send-raw conn (format-irc-line "WHO" mask))))

(defun whois (conn nick)
  "Request WHOIS information."
  (send-raw conn (format-irc-line "WHOIS" nick)))

(defun whowas (conn nick &optional count)
  "Request WHOWAS information."
  (if count
      (send-raw conn (format-irc-line "WHOWAS" nick (princ-to-string count)))
      (send-raw conn (format-irc-line "WHOWAS" nick))))

(defun ping (conn server)
  "Send PING to server."
  (send-raw conn (format-irc-line "PING" server)))

(defun pong (conn server &optional server2)
  "Send PONG response."
  (if server2
      (send-raw conn (format-irc-line "PONG" server server2))
      (send-raw conn (format-irc-line "PONG" server))))

(defun away (conn &optional message)
  "Set or clear away status."
  (if message
      (send-raw conn (format-irc-line "AWAY" message))
      (send-raw conn "AWAY")))

(defun userhost (conn &rest nicks)
  "Request userhost information."
  (send-raw conn (apply #'format-irc-line "USERHOST" nicks)))

(defun ison (conn &rest nicks)
  "Check if users are online."
  (send-raw conn (apply #'format-irc-line "ISON" nicks)))

;;; CTCP commands

(defun ctcp (conn target command &optional args)
  "Send a CTCP request."
  (let ((ctcp-msg (if args
                      (format nil "~C~A ~A~C" (code-char 1) command args (code-char 1))
                      (format nil "~C~A~C" (code-char 1) command (code-char 1)))))
    (send-raw conn (format-irc-line "PRIVMSG" target ctcp-msg))))

(defun ctcp-reply (conn target command &optional args)
  "Send a CTCP reply."
  (let ((ctcp-msg (if args
                      (format nil "~C~A ~A~C" (code-char 1) command args (code-char 1))
                      (format nil "~C~A~C" (code-char 1) command (code-char 1)))))
    (send-raw conn (format-irc-line "NOTICE" target ctcp-msg))))

;;; IRCv3 CAP commands

(defun cap-ls (conn &optional version)
  "Request capability list."
  (if version
      (send-raw conn (format nil "CAP LS ~A" version))
      (send-raw conn "CAP LS")))

(defun cap-req (conn &rest capabilities)
  "Request capabilities."
  (send-raw conn (format nil "CAP REQ :~{~A~^ ~}" capabilities)))

(defun cap-end (conn)
  "End capability negotiation."
  (send-raw conn "CAP END"))

;;; SASL authentication

(defun sasl-authenticate (conn mechanism)
  "Start SASL authentication with MECHANISM."
  (send-raw conn (format nil "AUTHENTICATE ~A" mechanism)))

(defun sasl-plain (conn username password)
  "Perform SASL PLAIN authentication."
  (sasl-authenticate conn "PLAIN")
  ;; The actual credentials are sent in response to AUTHENTICATE +
  ;; See handlers.lisp for the response handling
  (setf (connection-sasl-state conn) :authenticating))

(defun sasl-external (conn)
  "Perform SASL EXTERNAL authentication (using client certificate)."
  (sasl-authenticate conn "EXTERNAL")
  (setf (connection-sasl-state conn) :authenticating))

(defun send-sasl-plain-credentials (conn)
  "Send SASL PLAIN credentials (base64 encoded)."
  (let* ((sasl-user (connection-sasl-username conn))
         (sasl-pass (connection-sasl-password conn))
         (auth-string (format nil "~A~C~A~C~A" sasl-user (code-char 0) sasl-user (code-char 0) sasl-pass))
         (encoded (cl-base64:string-to-base64-string auth-string)))
    (send-raw conn (format nil "AUTHENTICATE ~A" encoded))))

(defun send-sasl-external-credentials (conn)
  "Send SASL EXTERNAL credentials (empty for client cert auth)."
  ;; EXTERNAL uses the client certificate, so we send +
  (send-raw conn "AUTHENTICATE +"))

;;; Operator commands

(defun oper (conn name password)
  "Authenticate as IRC operator."
  (send-raw conn (format-irc-line "OPER" name password)))

(defun kill (conn nick reason)
  "Kill a user (operator only)."
  (send-raw conn (format-irc-line "KILL" nick reason)))

(defun die (conn)
  "Shut down the server (operator only)."
  (send-raw conn "DIE"))

(defun restart- (conn)
  "Restart the server (operator only)."
  (send-raw conn "RESTART"))

(defun rehash (conn)
  "Reload server configuration (operator only)."
  (send-raw conn "REHASH"))

;;; Channel operator shortcuts

(defun op (conn channel nick)
  "Give operator status to a user."
  (mode conn channel "+o" nick))

(defun deop (conn channel nick)
  "Remove operator status from a user."
  (mode conn channel "-o" nick))

(defun voice (conn channel nick)
  "Give voice to a user."
  (mode conn channel "+v" nick))

(defun devoice (conn channel nick)
  "Remove voice from a user."
  (mode conn channel "-v" nick))

(defun ban (conn channel mask)
  "Ban a hostmask from a channel."
  (mode conn channel "+b" mask))

(defun unban (conn channel mask)
  "Remove a ban from a channel."
  (mode conn channel "-b" mask))

;;; Labeled response support (IRCv3)

(defun send-labeled (conn command callback)
  "Send a command with a label and call CALLBACK with the response.
   Only works if labeled-response capability is enabled."
  (if (cap-enabled-p conn "labeled-response")
      (let ((label (format nil "~A" (incf (connection-label-counter conn)))))
        (setf (gethash label (connection-pending-labels conn)) callback)
        (send-raw conn (format nil "@label=~A ~A" label command)))
      ;; Fallback: just send without label
      (send-raw conn command)))
