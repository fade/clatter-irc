;;;; test-connection.lisp - Tests for IRC connection management

(defpackage #:clatter-irc/test/connection
  (:use #:cl #:fiveam)
  (:import-from #:clatter-irc
                #:make-connection #:connection-server #:connection-nick
                #:connection-port #:connection-tls-p #:connection-username
                #:connection-sasl-username #:connection-sasl-password
                #:connection-user-data #:connection-state #:connectedp
                #:connection-cap-enabled #:connection-channels
                #:enabled-capabilities #:cap-enabled-p
                #:add-hook #:remove-hook #:remove-all-hooks #:run-hooks
                #:nick-equal #:normalize-channel))

(in-package #:clatter-irc/test/connection)

;;; Define test suite
(def-suite connection-tests
  :description "Tests for IRC connection management"
  :in :clatter-irc-tests)

(in-suite connection-tests)

;;; Connection creation tests

(test make-connection-defaults
  "Test connection creation with defaults"
  (let ((conn (make-connection "irc.example.com" "testnick")))
    (is (string= "irc.example.com" (connection-server conn)))
    (is (string= "testnick" (connection-nick conn)))
    (is (= 6697 (connection-port conn)))  ; TLS default
    (is (connection-tls-p conn))
    (is (string= "testnick" (connection-username conn)))
    (is (eq :disconnected (connection-state conn)))))

(test make-connection-plain
  "Test connection creation without TLS"
  (let ((conn (make-connection "irc.example.com" "testnick" :tls nil)))
    (is (= 6667 (connection-port conn)))
    (is (not (connection-tls-p conn)))))

(test make-connection-custom-port
  "Test connection creation with custom port"
  (let ((conn (make-connection "irc.example.com" "testnick" :port 7000)))
    (is (= 7000 (connection-port conn)))))

(test make-connection-with-sasl
  "Test connection creation with SASL credentials"
  (let ((conn (make-connection "irc.example.com" "testnick"
                               :sasl-username "myuser"
                               :sasl-password "mypass")))
    (is (string= "myuser" (connection-sasl-username conn)))
    (is (string= "mypass" (connection-sasl-password conn)))))

(test make-connection-user-data
  "Test connection with user data"
  (let ((conn (make-connection "irc.example.com" "testnick"
                               :user-data '(:app-name "test"))))
    (is (equal '(:app-name "test") (connection-user-data conn)))))

;;; Connection state tests

(test connection-initial-state
  "Test initial connection state"
  (let ((conn (make-connection "irc.example.com" "testnick")))
    (is (eq :disconnected (connection-state conn)))
    (is (not (connectedp conn)))
    (is (null (connection-cap-enabled conn)))
    (is (hash-table-p (connection-channels conn)))
    (is (= 0 (hash-table-count (connection-channels conn))))))

;;; Hook system tests

(test add-and-run-hook
  "Test adding and running hooks"
  (let ((conn (make-connection "irc.example.com" "testnick"))
        (called nil))
    (add-hook conn 'test-hook (lambda (arg) (setf called arg)))
    (run-hooks conn 'test-hook :test-value)
    (is (eq :test-value called))))

(test multiple-hooks
  "Test multiple hooks on same event"
  (let ((conn (make-connection "irc.example.com" "testnick"))
        (results nil))
    (add-hook conn 'test-hook (lambda (x) (push (list :first x) results)))
    (add-hook conn 'test-hook (lambda (x) (push (list :second x) results)))
    (run-hooks conn 'test-hook 42)
    (is (= 2 (length results)))
    (is (member '(:first 42) results :test #'equal))
    (is (member '(:second 42) results :test #'equal))))

(test remove-hook
  "Test removing a hook"
  (let ((conn (make-connection "irc.example.com" "testnick"))
        (called nil)
        (hook-fn (lambda () (setf called t))))
    (add-hook conn 'test-hook hook-fn)
    (remove-hook conn 'test-hook hook-fn)
    (run-hooks conn 'test-hook)
    (is (null called))))

(test remove-all-hooks
  "Test removing all hooks"
  (let ((conn (make-connection "irc.example.com" "testnick"))
        (called nil))
    (add-hook conn 'test-hook (lambda () (setf called t)))
    (remove-all-hooks conn 'test-hook)
    (run-hooks conn 'test-hook)
    (is (null called))))

;;; Capability tracking tests

(test cap-enabled-initially-empty
  "Test that no capabilities are enabled initially"
  (let ((conn (make-connection "irc.example.com" "testnick")))
    (is (null (enabled-capabilities conn)))
    (is (not (cap-enabled-p conn "sasl")))))

;;; Utility tests

(test nick-equal-case-insensitive
  "Test case-insensitive nick comparison"
  (is (nick-equal "TestNick" "testnick"))
  (is (nick-equal "NICK" "nick"))
  (is (not (nick-equal "nick1" "nick2"))))

(test normalize-channel
  "Test channel name normalization"
  (is (string= "#channel" (normalize-channel "#CHANNEL")))
  (is (string= "#test" (normalize-channel "#Test"))))

;;; Run tests
(defun run-connection-tests ()
  (run! 'connection-tests))
