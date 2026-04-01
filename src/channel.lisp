;;;; channel.lisp - IRC channel and user tracking

(in-package #:clatter-irc)

;;; Channel user class

(defclass channel-user ()
  ((nick :initarg :nick :accessor user-nick
         :documentation "Current nickname")
   (prefix-modes :initarg :prefix-modes :initform "" :accessor user-prefix-modes
                 :documentation "Mode prefix chars, e.g. \"@+\" for op+voice.
Ordered highest to lowest privilege.")
   (account :initarg :account :initform nil :accessor user-account
            :documentation "Account name from extended-join or account-notify.
NIL if unknown, \"*\" if not logged in.")
   (host :initarg :host :initform nil :accessor user-host
         :documentation "user@host from userhost-in-names or WHO reply"))
  (:documentation "A user present in an IRC channel."))

(defmethod print-object ((u channel-user) stream)
  (print-unreadable-object (u stream :type t)
    (format stream "~A~A" (user-prefix-modes u) (user-nick u))))

(defun make-channel-user (nick &key (prefix-modes "") account host)
  "Create a channel-user instance."
  (make-instance 'channel-user
                 :nick nick
                 :prefix-modes prefix-modes
                 :account account
                 :host host))

;;; Channel class

(defclass channel ()
  ((name :initarg :name :accessor channel-name
         :documentation "Channel name including prefix, e.g. \"#lisp\"")
   (topic :initform nil :accessor channel-topic
          :documentation "Current channel topic text, or NIL if unset")
   (topic-who :initform nil :accessor channel-topic-who
              :documentation "Nick or hostmask of who set the topic")
   (topic-time :initform nil :accessor channel-topic-time
               :documentation "Universal time when the topic was set")
   (users :initform (make-hash-table :test 'equalp) :accessor channel-users
          :documentation "Hash of nick (string) -> channel-user object")
   (modes :initform nil :accessor channel-modes
          :documentation "List of active channel mode characters, e.g. (#\\n #\\t)")
   (created :initform nil :accessor channel-created
            :documentation "Channel creation time (universal-time) from RPL_CREATIONTIME")
   (joined-at :initform nil :accessor channel-joined-at
              :documentation "Universal time when we joined this channel"))
  (:documentation "Represents an IRC channel with its state: topic, users, modes.
Subclass this and pass :channel-class to make-connection for per-channel app state."))

(defmethod print-object ((ch channel) stream)
  (print-unreadable-object (ch stream :type t)
    (format stream "~A (~D users)" (channel-name ch)
            (hash-table-count (channel-users ch)))))

(defun make-channel (name &key (channel-class 'channel))
  "Create a channel instance of CHANNEL-CLASS."
  (let ((ch (make-instance channel-class :name name)))
    (setf (channel-joined-at ch) (get-universal-time))
    ch))

;;; Channel user management

(defun channel-add-user (channel nick &key (prefix-modes "") account host)
  "Add or update a user in CHANNEL. Returns the channel-user object."
  (let ((existing (gethash nick (channel-users channel))))
    (if existing
        (progn
          (when (and prefix-modes (> (length prefix-modes) 0))
            (setf (user-prefix-modes existing) prefix-modes))
          (when account
            (setf (user-account existing) account))
          (when host
            (setf (user-host existing) host))
          existing)
        (let ((user (make-channel-user nick
                                       :prefix-modes prefix-modes
                                       :account account
                                       :host host)))
          (setf (gethash nick (channel-users channel)) user)
          user))))

(defun channel-remove-user (channel nick)
  "Remove a user from CHANNEL. Returns T if the user was present."
  (remhash nick (channel-users channel)))

(defun channel-find-user (channel nick)
  "Find a user in CHANNEL by nick. Returns channel-user or NIL."
  (gethash nick (channel-users channel)))

(defun channel-rename-user (channel old-nick new-nick)
  "Update a user's nick in CHANNEL. Returns the channel-user or NIL."
  (let ((user (gethash old-nick (channel-users channel))))
    (when user
      (remhash old-nick (channel-users channel))
      (setf (user-nick user) new-nick)
      (setf (gethash new-nick (channel-users channel)) user)
      user)))

(defun channel-user-count (channel)
  "Return the number of users in CHANNEL."
  (hash-table-count (channel-users channel)))

(defun channel-user-list (channel)
  "Return a sorted list of nick strings in CHANNEL."
  (let ((nicks nil))
    (maphash (lambda (nick user)
               (declare (ignore user))
               (push nick nicks))
             (channel-users channel))
    (sort nicks #'string-lessp)))

(defun channel-user-nicks-with-prefix (channel)
  "Return a sorted list of prefixed nick strings (e.g. \"@nick\") in CHANNEL."
  (let ((entries nil))
    (maphash (lambda (nick user)
               (let ((prefix (user-prefix-modes user)))
                 (push (if (> (length prefix) 0)
                           (concatenate 'string (string (char prefix 0)) nick)
                           nick)
                       entries)))
             (channel-users channel))
    (sort entries #'string-lessp)))

;;; Prefix mode parsing (for NAMES reply)

(defparameter *prefix-chars* "@%+~&!."
  "Known IRC user prefix characters, ordered by convention.
Different networks may use different sets.")

(defun parse-nick-with-prefix (raw-nick)
  "Parse a nick that may have mode prefix characters.
Returns (values nick prefix-string).
E.g. \"@+Plonk\" -> (values \"Plonk\" \"@+\")"
  (let ((start 0))
    (loop while (and (< start (length raw-nick))
                     (find (char raw-nick start) *prefix-chars*))
          do (incf start))
    (values (subseq raw-nick start)
            (subseq raw-nick 0 start))))

;;; Connection-level channel management

(defun find-channel (conn channel-name)
  "Find a channel object on CONN by name. Returns the channel or NIL."
  (gethash (normalize-channel channel-name) (connection-channels conn)))

(defun ensure-channel (conn channel-name)
  "Find or create a channel object on CONN. Returns the channel."
  (let ((key (normalize-channel channel-name)))
    (or (gethash key (connection-channels conn))
        (let ((ch (make-channel channel-name
                                :channel-class (connection-channel-class conn))))
          (setf (gethash key (connection-channels conn)) ch)
          ch))))

(defun remove-channel (conn channel-name)
  "Remove a channel from CONN's tracking. Returns T if it was present."
  (remhash (normalize-channel channel-name) (connection-channels conn)))

(defun joined-channels (conn)
  "Return a list of channel objects currently joined on CONN."
  (let ((channels nil))
    (maphash (lambda (key ch)
               (declare (ignore key))
               (push ch channels))
             (connection-channels conn))
    channels))

(defun joined-channel-p (conn channel-name)
  "Return T if we are currently in CHANNEL-NAME on CONN."
  (not (null (gethash (normalize-channel channel-name) (connection-channels conn)))))
