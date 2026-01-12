# clatter-irc

A modern Common Lisp IRC library with IRCv3 support, extracted from [CLatter](https://github.com/glenneth1/CLatter).

## Features

- **IRCv3 Support**
  - Message tags
  - SASL authentication (PLAIN, EXTERNAL)
  - CAP negotiation
  - Labeled responses
  - Message batching
  - Server-time

- **TLS/SSL**
  - Full TLS support with SNI
  - Client certificate authentication

- **DCC**
  - DCC CHAT
  - DCC SEND/RECEIVE
  - Configurable port range

- **Robust Connection Handling**
  - Automatic reconnection with exponential backoff
  - Thread-safe message sending
  - Health monitoring

- **Security**
  - Input sanitization to prevent IRC injection
  - Proper handling of control characters

## Installation

Clone the repository and ensure it's in your ASDF source registry:

```lisp
(ql:quickload "clatter-irc")
```

Or load directly:

```lisp
(asdf:load-system "clatter-irc")
```

## Quick Start

```lisp
(use-package :clatter-irc)

;; Create a connection
(defvar *conn* (make-connection "irc.libera.chat" "mynick"
                                :tls t
                                :sasl-password "mypassword"))

;; Add hooks for events
(add-hook *conn* 'on-connect
          (lambda (conn)
            (join conn "#lisp")))

(add-hook *conn* 'on-privmsg
          (lambda (conn msg sender target text)
            (format t "~A -> ~A: ~A~%" sender target text)))

;; Connect
(connect *conn*)

;; Send messages
(privmsg *conn* "#lisp" "Hello from clatter-irc!")

;; Disconnect when done
(disconnect *conn*)
```

## API Overview

### Connection

```lisp
;; Create connection
(make-connection server nick &key port tls username realname
                 password client-cert sasl-username sasl-password
                 reconnect user-data)

;; Connect/disconnect
(connect conn)
(disconnect conn &optional message)
(connectedp conn)

;; Convenience macro
(with-connection (conn "server" "nick" :tls t)
  (join conn "#channel")
  ...)
```

### IRC Commands

```lisp
(join conn channel &optional key)
(part conn channel &optional message)
(privmsg conn target message)
(notice conn target message)
(nick conn new-nick)
(quit conn &optional message)
(kick conn channel nick &optional reason)
(mode conn target &optional mode &rest args)
(topic conn channel &optional new-topic)
(invite conn nick channel)
(who conn mask)
(whois conn nick)
(ctcp conn target command &optional args)
(ctcp-reply conn target command &optional args)
```

### Hooks

```lisp
(add-hook conn hook-name function)
(remove-hook conn hook-name function)
(remove-all-hooks conn &optional hook-name)
```

Available hooks:
- `on-connect` - `(conn)`
- `on-disconnect` - `(conn)`
- `on-privmsg` - `(conn msg sender target text)`
- `on-notice` - `(conn msg sender target text)`
- `on-join` - `(conn msg nick channel)`
- `on-part` - `(conn msg nick channel reason)`
- `on-quit` - `(conn msg nick reason)`
- `on-kick` - `(conn msg kicker channel kicked reason)`
- `on-nick` - `(conn msg old-nick new-nick)`
- `on-mode` - `(conn msg target modes)`
- `on-topic` - `(conn msg setter channel topic)`
- `on-invite` - `(conn msg inviter invited channel)`
- `on-numeric` - `(conn msg code name)`
- `on-ctcp` - `(conn msg sender target command args)`
- `on-ctcp-reply` - `(conn msg sender command args)`
- `on-raw` - `(conn msg)` - all messages
- `on-error` - `(conn msg error-message)`

### DCC

```lisp
;; Create DCC manager
(make-dcc-manager &optional irc-connection)

;; Initiate DCC
(dcc-initiate-chat manager nick)
(dcc-initiate-send manager nick filepath)

;; Handle offers
(dcc-accept manager id &optional save-path)
(dcc-reject manager id)
(dcc-close manager id)

;; List connections
(dcc-list manager)
```

### Message Parsing

```lisp
;; Parse a raw IRC message
(parse-message line) ; => message object

;; Access message parts
(message-command msg)
(message-params msg)
(message-prefix msg)
(message-tags msg)

;; Parse prefix
(parse-prefix "nick!user@host") ; => prefix object
(prefix-nick prefix)
(prefix-user prefix)
(prefix-host prefix)
```

### Utilities

```lisp
(sanitize-input text)           ; Remove dangerous characters
(channel-name-p string)         ; Check if string is a channel name
(valid-channel-name-p name)     ; Validate channel name
(valid-nick-p nick)             ; Validate nickname
(strip-formatting text)         ; Remove IRC formatting codes
(split-message text max-length) ; Split long messages
```

## Configuration

```lisp
;; Default ports
*default-port*      ; 6667
*default-tls-port*  ; 6697

;; Reconnection
*reconnect-base-delay*   ; 5 seconds
*reconnect-max-delay*    ; 300 seconds
*reconnect-max-attempts* ; nil (unlimited)

;; DCC
*dcc-port-range-start*   ; 49152
*dcc-port-range-end*     ; 65535
*dcc-download-directory* ; nil (current directory)

;; Logging
*log-function* ; Set to (lambda (level format &rest args) ...) for logging
```

## Comparison with cl-irc

| Feature | clatter-irc | cl-irc |
|---------|-------------|--------|
| IRCv3 support | ✓ | ✗ |
| SASL auth | ✓ | ✗ |
| TLS with SNI | ✓ | Partial |
| Client certs | ✓ | ✗ |
| Auto reconnect | ✓ | ✗ |
| Thread-safe | ✓ | Partial |
| Input sanitization | ✓ | ✗ |
| Last updated | 2026 | 2014 |

## License

MIT License

## Credits

Extracted from CLatter by Glenn Thompson.
