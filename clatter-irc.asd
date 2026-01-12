;;;; clatter-irc.asd - ASDF system definition for clatter-irc

(defsystem "clatter-irc"
  :name "clatter-irc"
  :version "0.1.0"
  :author "Glenn Thompson"
  :license "MIT"
  :description "Modern Common Lisp IRC library with IRCv3 support"
  :long-description "A full-featured IRC client library extracted from CLatter.
Features include:
- IRCv3 capabilities (message-tags, SASL, CAP negotiation, labeled-response, batch)
- TLS/SSL with SNI and client certificate support
- SASL authentication (PLAIN, EXTERNAL)
- DCC CHAT and DCC SEND
- Robust reconnection with exponential backoff
- Thread-safe connection handling
- Input sanitization to prevent IRC injection"
  :depends-on ("usocket"
               "bordeaux-threads"
               "cl+ssl"
               "flexi-streams"
               "cl-base64"
               "split-sequence"
               "cl-ppcre")
  :serial t
  :components ((:module "src"
                :components ((:file "package")
                             (:file "constants")
                             (:file "utility")
                             (:file "protocol")
                             (:file "connection")
                             (:file "commands")
                             (:file "handlers")
                             (:file "dcc"))))
  :in-order-to ((test-op (test-op "clatter-irc/test"))))

(defsystem "clatter-irc/test"
  :depends-on ("clatter-irc" "fiveam")
  :components ((:module "test"
                :components ((:file "test-protocol")
                             (:file "test-connection"))))
  :perform (test-op (o s)
             (uiop:symbol-call :fiveam :run! :clatter-irc-tests)))
