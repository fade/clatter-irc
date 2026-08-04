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
                             (:file "channel")
                             (:file "commands")
                             (:file "handlers")
                             (:file "dcc"))))
  :in-order-to ((test-op (test-op "clatter-irc/test"))))

;;; Protocol-only system: the IRC wire-format codec (parsing, formatting,
;;; sanitization) with no socket, threading, TLS, or DCC layers.  Edge
;;; adapters and protocol consumers depend on this instead of the full
;;; clatter-irc system so a resident socket-connect is not loaded into
;;; the image.  Only cl-ppcre is needed (for mask-matches-p in utility.lisp).
(defsystem "clatter-irc/protocol"
  :description "IRC protocol parsing and formatting (no connection layer)"
  :depends-on ("cl-ppcre")
  :components ((:module "src"
                :components ((:file "package")
                             (:file "utility")
                             (:file "protocol")))))

(defsystem "clatter-irc/test"
  :depends-on ("clatter-irc" "fiveam")
  :components ((:module "test"
                :components ((:file "test-protocol")
                             (:file "test-connection")
                             (:file "test-channel"))))
  :perform (test-op (o s)
             (uiop:symbol-call :fiveam :run! :clatter-irc-tests)))
