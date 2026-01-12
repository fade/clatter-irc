;;;; test-protocol.lisp - Tests for IRC protocol parsing

(defpackage #:clatter-irc/test/protocol
  (:use #:cl #:fiveam)
  (:import-from #:clatter-irc
                #:parse-message #:message-command #:message-params
                #:message-prefix #:message-tags
                #:parse-prefix #:prefix-nick #:prefix-user #:prefix-host
                #:format-irc-line #:sanitize-input
                #:channel-name-p #:valid-channel-name-p
                #:ip-string-to-integer #:ip-integer-to-string
                #:format-tags))

(in-package #:clatter-irc/test/protocol)

;;; Define test suite
(def-suite protocol-tests
  :description "Tests for IRC protocol parsing and formatting")

(def-suite* :clatter-irc-tests
  :description "All clatter-irc tests")

(in-suite protocol-tests)

;;; Message parsing tests

(test parse-simple-message
  "Test parsing a simple IRC message"
  (let ((msg (parse-message "PING :server.example.com")))
    (is (string= "PING" (message-command msg)))
    (is (equal '("server.example.com") (message-params msg)))
    (is (null (message-prefix msg)))
    (is (null (message-tags msg)))))

(test parse-message-with-prefix
  "Test parsing a message with prefix"
  (let ((msg (parse-message ":nick!user@host PRIVMSG #channel :Hello world")))
    (is (string= "PRIVMSG" (message-command msg)))
    (is (string= "nick!user@host" (message-prefix msg)))
    (is (equal '("#channel" "Hello world") (message-params msg)))))

(test parse-message-with-tags
  "Test parsing an IRCv3 message with tags"
  (let ((msg (parse-message "@time=2024-01-01T12:00:00Z;account=nick :nick!user@host PRIVMSG #channel :Hello")))
    (is (string= "PRIVMSG" (message-command msg)))
    (is (string= "nick!user@host" (message-prefix msg)))
    (is (equal '("#channel" "Hello") (message-params msg)))
    (let ((tags (message-tags msg)))
      (is (string= "2024-01-01T12:00:00Z" (gethash "time" tags)))
      (is (string= "nick" (gethash "account" tags))))))

(test parse-numeric-reply
  "Test parsing a numeric reply"
  (let ((msg (parse-message ":server.example.com 001 nick :Welcome to IRC")))
    (is (string= "001" (message-command msg)))
    (is (string= "server.example.com" (message-prefix msg)))
    (is (equal '("nick" "Welcome to IRC") (message-params msg)))))

;;; Prefix parsing tests

(test parse-full-prefix
  "Test parsing a full nick!user@host prefix"
  (let ((prefix (parse-prefix "nick!user@host.example.com")))
    (is (string= "nick" (prefix-nick prefix)))
    (is (string= "user" (prefix-user prefix)))
    (is (string= "host.example.com" (prefix-host prefix)))))

(test parse-server-prefix
  "Test parsing a server-only prefix"
  (let ((prefix (parse-prefix "irc.server.com")))
    (is (string= "irc.server.com" (prefix-nick prefix)))
    (is (null (prefix-user prefix)))
    (is (null (prefix-host prefix)))))

(test parse-nick-host-prefix
  "Test parsing nick@host prefix (no user)"
  (let ((prefix (parse-prefix "nick@host.com")))
    (is (string= "nick" (prefix-nick prefix)))
    (is (null (prefix-user prefix)))
    (is (string= "host.com" (prefix-host prefix)))))

;;; Message formatting tests

(test format-simple-command
  "Test formatting a simple command"
  (is (string= "PING server" (format-irc-line "PING" "server"))))

(test format-privmsg
  "Test formatting a PRIVMSG"
  (is (string= "PRIVMSG #channel :Hello world" 
               (format-irc-line "PRIVMSG" "#channel" "Hello world"))))

(test format-join
  "Test formatting a JOIN command"
  (is (string= "JOIN #channel" (format-irc-line "JOIN" "#channel"))))

;;; Input sanitization tests

(test sanitize-removes-newlines
  "Test that sanitize-input removes newlines"
  (is (string= "Hello world" (sanitize-input "Hello\r\nworld")))
  (is (string= "Test message" (sanitize-input "Test\nmessage"))))

(test sanitize-removes-null
  "Test that sanitize-input removes null characters"
  (is (string= "Hello" (sanitize-input (format nil "Hel~Clo" (code-char 0))))))

;;; Channel validation tests

(test channel-name-detection
  "Test channel name detection"
  (is (channel-name-p "#channel"))
  (is (channel-name-p "&channel"))
  (is (not (channel-name-p "nickname")))
  (is (not (channel-name-p ""))))

(test valid-channel-names
  "Test valid channel name validation"
  (is (valid-channel-name-p "#lisp"))
  (is (valid-channel-name-p "#common-lisp"))
  (is (not (valid-channel-name-p "#chan nel")))  ; space
  (is (not (valid-channel-name-p ""))))

;;; IP conversion tests

(test ip-string-to-integer
  "Test IP string to integer conversion"
  (is (= 2130706433 (ip-string-to-integer "127.0.0.1")))
  (is (= 3232235777 (ip-string-to-integer "192.168.1.1"))))

(test ip-integer-to-string
  "Test IP integer to string conversion"
  (is (string= "127.0.0.1" (ip-integer-to-string 2130706433)))
  (is (string= "192.168.1.1" (ip-integer-to-string 3232235777))))

(test ip-roundtrip
  "Test IP conversion roundtrip"
  (let ((ip "10.20.30.40"))
    (is (string= ip (ip-integer-to-string (ip-string-to-integer ip))))))

;;; IRCv3 tag formatting tests

(test format-tags-simple
  "Test formatting simple tags"
  (let ((tags (make-hash-table :test 'equal)))
    (setf (gethash "label" tags) "abc123")
    (is (string= "@label=abc123 " (format-tags tags)))))

(test format-tags-multiple
  "Test formatting multiple tags"
  (let ((tags (make-hash-table :test 'equal)))
    (setf (gethash "a" tags) "1")
    (setf (gethash "b" tags) "2")
    (let ((result (format-tags tags)))
      (is (or (string= "@a=1;b=2 " result)
              (string= "@b=2;a=1 " result))))))

;;; Run tests
(defun run-protocol-tests ()
  (run! 'protocol-tests))
