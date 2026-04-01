;;;; test-channel.lisp - Tests for IRC channel tracking

(defpackage #:clatter-irc/test/channel
  (:use #:cl #:fiveam)
  (:import-from #:clatter-irc
                #:make-connection #:connection-channels #:connection-nick
                #:find-channel #:ensure-channel #:remove-channel
                #:joined-channels #:joined-channel-p
                #:channel #:channel-name #:channel-topic #:channel-topic-who
                #:channel-topic-time #:channel-users #:channel-modes
                #:channel-created #:channel-joined-at
                #:channel-user #:user-nick #:user-prefix-modes #:user-account #:user-host
                #:channel-add-user #:channel-remove-user #:channel-find-user
                #:channel-rename-user #:channel-user-count #:channel-user-list
                #:channel-user-nicks-with-prefix
                #:parse-nick-with-prefix
                #:make-channel #:make-channel-user))

(in-package #:clatter-irc/test/channel)

;;; Define test suite
(def-suite channel-tests
  :description "Tests for IRC channel tracking"
  :in :clatter-irc-tests)

(in-suite channel-tests)

;;; --- Unit tests for channel class ---

(test make-channel-defaults
  "Test channel creation with defaults"
  (let ((ch (make-channel "#test")))
    (is (string= "#test" (channel-name ch)))
    (is (null (channel-topic ch)))
    (is (null (channel-topic-who ch)))
    (is (null (channel-modes ch)))
    (is (= 0 (channel-user-count ch)))
    (is (not (null (channel-joined-at ch))))))

(test channel-add-and-find-user
  "Test adding and finding users in a channel"
  (let ((ch (make-channel "#test")))
    (channel-add-user ch "Alice")
    (channel-add-user ch "Bob" :prefix-modes "@")
    (is (= 2 (channel-user-count ch)))
    (let ((alice (channel-find-user ch "Alice"))
          (bob (channel-find-user ch "Bob")))
      (is (not (null alice)))
      (is (string= "Alice" (user-nick alice)))
      (is (string= "" (user-prefix-modes alice)))
      (is (not (null bob)))
      (is (string= "@" (user-prefix-modes bob))))))

(test channel-remove-user
  "Test removing users from a channel"
  (let ((ch (make-channel "#test")))
    (channel-add-user ch "Alice")
    (channel-add-user ch "Bob")
    (is (= 2 (channel-user-count ch)))
    (channel-remove-user ch "Alice")
    (is (= 1 (channel-user-count ch)))
    (is (null (channel-find-user ch "Alice")))
    (is (not (null (channel-find-user ch "Bob"))))))

(test channel-rename-user
  "Test renaming a user in a channel"
  (let ((ch (make-channel "#test")))
    (channel-add-user ch "OldNick" :prefix-modes "@")
    (channel-rename-user ch "OldNick" "NewNick")
    (is (null (channel-find-user ch "OldNick")))
    (let ((user (channel-find-user ch "NewNick")))
      (is (not (null user)))
      (is (string= "NewNick" (user-nick user)))
      (is (string= "@" (user-prefix-modes user))))))

(test channel-user-list-sorted
  "Test that channel-user-list returns sorted nicks"
  (let ((ch (make-channel "#test")))
    (channel-add-user ch "Charlie")
    (channel-add-user ch "Alice")
    (channel-add-user ch "Bob")
    (is (equal '("Alice" "Bob" "Charlie") (channel-user-list ch)))))

(test channel-user-nicks-with-prefix-display
  "Test prefixed nick display"
  (let ((ch (make-channel "#test")))
    (channel-add-user ch "Alice" :prefix-modes "@")
    (channel-add-user ch "Bob" :prefix-modes "+")
    (channel-add-user ch "Charlie")
    (let ((nicks (channel-user-nicks-with-prefix ch)))
      (is (= 3 (length nicks)))
      (is (member "+Bob" nicks :test #'string=))
      (is (member "@Alice" nicks :test #'string=))
      (is (member "Charlie" nicks :test #'string=)))))

(test channel-add-user-updates-existing
  "Test that adding an existing user updates their info"
  (let ((ch (make-channel "#test")))
    (channel-add-user ch "Alice")
    (channel-add-user ch "Alice" :prefix-modes "@" :account "alice_acct")
    (is (= 1 (channel-user-count ch)))
    (let ((user (channel-find-user ch "Alice")))
      (is (string= "@" (user-prefix-modes user)))
      (is (string= "alice_acct" (user-account user))))))

;;; --- Parse nick with prefix ---

(test parse-nick-with-prefix-basic
  "Test parsing nicks with mode prefixes"
  (multiple-value-bind (nick prefix) (parse-nick-with-prefix "Alice")
    (is (string= "Alice" nick))
    (is (string= "" prefix)))
  (multiple-value-bind (nick prefix) (parse-nick-with-prefix "@Alice")
    (is (string= "Alice" nick))
    (is (string= "@" prefix)))
  (multiple-value-bind (nick prefix) (parse-nick-with-prefix "@+Alice")
    (is (string= "Alice" nick))
    (is (string= "@+" prefix))))

;;; --- Connection-level channel management ---

(test connection-channel-tracking
  "Test find-channel, ensure-channel, remove-channel"
  (let ((conn (make-connection "irc.example.com" "testnick" :tls nil)))
    (is (null (find-channel conn "#test")))
    (is (not (joined-channel-p conn "#test")))
    (let ((ch (ensure-channel conn "#test")))
      (is (not (null ch)))
      (is (string= "#test" (channel-name ch)))
      (is (joined-channel-p conn "#test"))
      ;; ensure-channel returns existing
      (is (eq ch (ensure-channel conn "#test")))
      ;; Case insensitive
      (is (eq ch (find-channel conn "#TEST"))))
    (remove-channel conn "#test")
    (is (null (find-channel conn "#test")))
    (is (not (joined-channel-p conn "#test")))))

(test joined-channels-list
  "Test listing all joined channels"
  (let ((conn (make-connection "irc.example.com" "testnick" :tls nil)))
    (ensure-channel conn "#foo")
    (ensure-channel conn "#bar")
    (let ((channels (joined-channels conn)))
      (is (= 2 (length channels)))
      (is (find "#foo" channels :key #'channel-name :test #'string-equal))
      (is (find "#bar" channels :key #'channel-name :test #'string-equal)))))

;;; --- Handler integration tests (simulating IRC messages) ---

(defun make-test-conn ()
  "Create a connection for handler testing with state set to :connected."
  (let ((conn (make-connection "irc.example.com" "TestBot" :tls nil)))
    (setf (clatter-irc::connection-state conn) :connected)
    conn))

(defun simulate-message (conn raw-line)
  "Parse and dispatch a raw IRC line through clatter-irc's handler."
  (let ((msg (clatter-irc::parse-message raw-line)))
    (when msg
      (clatter-irc::handle-message conn msg))))

(test handler-join-creates-channel
  "Test that our own JOIN creates a channel object"
  (let ((conn (make-test-conn)))
    (simulate-message conn ":TestBot!user@host JOIN #lisp")
    (let ((ch (find-channel conn "#lisp")))
      (is (not (null ch)))
      (is (string= "#lisp" (channel-name ch)))
      ;; We should be in the user list
      (is (not (null (channel-find-user ch "TestBot")))))))

(test handler-join-other-user
  "Test that another user's JOIN adds them to an existing channel"
  (let ((conn (make-test-conn)))
    (simulate-message conn ":TestBot!user@host JOIN #lisp")
    (simulate-message conn ":Alice!alice@example.com JOIN #lisp")
    (let ((ch (find-channel conn "#lisp")))
      (is (= 2 (channel-user-count ch)))
      (let ((alice (channel-find-user ch "Alice")))
        (is (not (null alice)))
        (is (string= "alice@example.com" (user-host alice)))))))

(test handler-part-removes-user
  "Test that PART removes a user from the channel"
  (let ((conn (make-test-conn)))
    (simulate-message conn ":TestBot!user@host JOIN #lisp")
    (simulate-message conn ":Alice!alice@host JOIN #lisp")
    (simulate-message conn ":Alice!alice@host PART #lisp :goodbye")
    (let ((ch (find-channel conn "#lisp")))
      (is (not (null ch)))
      (is (= 1 (channel-user-count ch)))
      (is (null (channel-find-user ch "Alice"))))))

(test handler-part-self-removes-channel
  "Test that our own PART removes the channel entirely"
  (let ((conn (make-test-conn)))
    (simulate-message conn ":TestBot!user@host JOIN #lisp")
    (simulate-message conn ":TestBot!user@host PART #lisp")
    (is (null (find-channel conn "#lisp")))))

(test handler-quit-removes-from-all-channels
  "Test that QUIT removes a user from all channels"
  (let ((conn (make-test-conn)))
    (simulate-message conn ":TestBot!user@host JOIN #lisp")
    (simulate-message conn ":TestBot!user@host JOIN #emacs")
    (simulate-message conn ":Alice!alice@host JOIN #lisp")
    (simulate-message conn ":Alice!alice@host JOIN #emacs")
    (simulate-message conn ":Alice!alice@host QUIT :leaving")
    (is (null (channel-find-user (find-channel conn "#lisp") "Alice")))
    (is (null (channel-find-user (find-channel conn "#emacs") "Alice")))))

(test handler-kick-removes-user
  "Test that KICK removes the kicked user"
  (let ((conn (make-test-conn)))
    (simulate-message conn ":TestBot!user@host JOIN #lisp")
    (simulate-message conn ":Alice!alice@host JOIN #lisp")
    (simulate-message conn ":Op!op@host KICK #lisp Alice :behave")
    (let ((ch (find-channel conn "#lisp")))
      (is (not (null ch)))
      (is (null (channel-find-user ch "Alice"))))))

(test handler-kick-self-removes-channel
  "Test that being kicked removes the channel"
  (let ((conn (make-test-conn)))
    (simulate-message conn ":TestBot!user@host JOIN #lisp")
    (simulate-message conn ":Op!op@host KICK #lisp TestBot :bye")
    (is (null (find-channel conn "#lisp")))))

(test handler-nick-change-updates-channels
  "Test that NICK change updates user in all channels"
  (let ((conn (make-test-conn)))
    (simulate-message conn ":TestBot!user@host JOIN #lisp")
    (simulate-message conn ":Alice!alice@host JOIN #lisp")
    (simulate-message conn ":Alice!alice@host NICK Alice2")
    (let ((ch (find-channel conn "#lisp")))
      (is (null (channel-find-user ch "Alice")))
      (is (not (null (channel-find-user ch "Alice2")))))))

(test handler-topic-updates-channel
  "Test that TOPIC command updates channel topic"
  (let ((conn (make-test-conn)))
    (simulate-message conn ":TestBot!user@host JOIN #lisp")
    (simulate-message conn ":Alice!alice@host TOPIC #lisp :Welcome to #lisp!")
    (let ((ch (find-channel conn "#lisp")))
      (is (string= "Welcome to #lisp!" (channel-topic ch)))
      (is (string= "Alice" (channel-topic-who ch))))))

(test handler-numeric-332-topic
  "Test RPL_TOPIC (332) sets channel topic"
  (let ((conn (make-test-conn)))
    (simulate-message conn ":TestBot!user@host JOIN #lisp")
    (simulate-message conn ":server 332 TestBot #lisp :The topic text")
    (let ((ch (find-channel conn "#lisp")))
      (is (string= "The topic text" (channel-topic ch))))))

(test handler-numeric-353-namreply
  "Test RPL_NAMREPLY (353) populates user list"
  (let ((conn (make-test-conn)))
    (simulate-message conn ":TestBot!user@host JOIN #lisp")
    (simulate-message conn ":server 353 TestBot = #lisp :@Op +Voice Regular")
    (let ((ch (find-channel conn "#lisp")))
      ;; 3 from NAMREPLY + 1 from our JOIN = but Op/Voice/Regular might overlap
      ;; Actually TestBot from JOIN + Op, Voice, Regular from NAMES = 4
      (is (>= (channel-user-count ch) 3))
      (let ((op (channel-find-user ch "Op")))
        (is (not (null op)))
        (is (string= "@" (user-prefix-modes op))))
      (let ((voice (channel-find-user ch "Voice")))
        (is (not (null voice)))
        (is (string= "+" (user-prefix-modes voice))))
      (is (not (null (channel-find-user ch "Regular")))))))

(test handler-numeric-324-channelmodeis
  "Test RPL_CHANNELMODEIS (324) sets channel modes"
  (let ((conn (make-test-conn)))
    (simulate-message conn ":TestBot!user@host JOIN #lisp")
    (simulate-message conn ":server 324 TestBot #lisp +nt")
    (let ((ch (find-channel conn "#lisp")))
      (is (member #\n (channel-modes ch)))
      (is (member #\t (channel-modes ch))))))

;;; Run tests
(defun run-channel-tests ()
  (run! 'channel-tests))
