;;;; constants.lisp - Constants for clatter-irc

(in-package #:clatter-irc)

;;; Default connection settings
(defparameter *default-port* 6667
  "Default IRC port (plaintext)")

(defparameter *default-tls-port* 6697
  "Default IRC port (TLS)")

(defparameter *default-quit-message* "clatter-irc"
  "Default quit message")

(defparameter *default-realname* "clatter-irc user"
  "Default realname for USER command")

;;; IRCv3 capabilities we want to negotiate
(defparameter *wanted-capabilities*
  '("message-tags"           ; IRCv3.2 - tags on all messages
    "server-time"            ; Timestamp from server
    "batch"                  ; Message batching
    "labeled-response"       ; Request/response correlation
    "sasl"                   ; SASL authentication
    "cap-notify"             ; CAP change notifications
    "account-notify"         ; Account change notifications
    "away-notify"            ; Away status notifications
    "extended-join"          ; Extended JOIN info
    "multi-prefix"           ; Multiple prefixes in NAMES
    "userhost-in-names"      ; Full hostmask in NAMES
    "chghost"                ; Host change notifications
    "setname"                ; Realname change notifications
    "account-tag"            ; Account in message tags
    "echo-message")          ; Echo sent messages back
  "List of IRCv3 capabilities to request during connection")

;;; DCC settings
(defparameter *dcc-port-range-start* 49152
  "Start of port range for DCC connections")

(defparameter *dcc-port-range-end* 65535
  "End of port range for DCC connections")

(defparameter *dcc-download-directory* nil
  "Directory for DCC file downloads. If nil, uses current directory.")

(defparameter *dcc-timeout* 120
  "Timeout in seconds for DCC connection attempts")

;;; Reconnection settings
(defparameter *reconnect-base-delay* 5
  "Base delay in seconds before reconnection attempt")

(defparameter *reconnect-max-delay* 300
  "Maximum delay in seconds between reconnection attempts")

(defparameter *reconnect-max-attempts* nil
  "Maximum number of reconnection attempts. nil = unlimited")

;;; IRC numeric reply codes (RFC 2812)
(defparameter *reply-codes*
  '((001 . :rpl-welcome)
    (002 . :rpl-yourhost)
    (003 . :rpl-created)
    (004 . :rpl-myinfo)
    (005 . :rpl-isupport)
    (221 . :rpl-umodeis)
    (251 . :rpl-luserclient)
    (252 . :rpl-luserop)
    (253 . :rpl-luserunknown)
    (254 . :rpl-luserchannels)
    (255 . :rpl-luserme)
    (265 . :rpl-localusers)
    (266 . :rpl-globalusers)
    (301 . :rpl-away)
    (305 . :rpl-unaway)
    (306 . :rpl-nowaway)
    (311 . :rpl-whoisuser)
    (312 . :rpl-whoisserver)
    (313 . :rpl-whoisoperator)
    (314 . :rpl-whowasuser)
    (315 . :rpl-endofwho)
    (317 . :rpl-whoisidle)
    (318 . :rpl-endofwhois)
    (319 . :rpl-whoischannels)
    (321 . :rpl-liststart)
    (322 . :rpl-list)
    (323 . :rpl-listend)
    (324 . :rpl-channelmodeis)
    (329 . :rpl-creationtime)
    (330 . :rpl-whoisaccount)
    (331 . :rpl-notopic)
    (332 . :rpl-topic)
    (333 . :rpl-topicwhotime)
    (341 . :rpl-inviting)
    (352 . :rpl-whoreply)
    (353 . :rpl-namreply)
    (366 . :rpl-endofnames)
    (367 . :rpl-banlist)
    (368 . :rpl-endofbanlist)
    (369 . :rpl-endofwhowas)
    (372 . :rpl-motd)
    (375 . :rpl-motdstart)
    (376 . :rpl-endofmotd)
    (378 . :rpl-whoishost)
    (379 . :rpl-whoismodes)
    (381 . :rpl-youreoper)
    (391 . :rpl-time)
    (396 . :rpl-hosthidden)
    (401 . :err-nosuchnick)
    (402 . :err-nosuchserver)
    (403 . :err-nosuchchannel)
    (404 . :err-cannotsendtochan)
    (405 . :err-toomanychannels)
    (406 . :err-wasnosuchnick)
    (407 . :err-toomanytargets)
    (409 . :err-noorigin)
    (411 . :err-norecipient)
    (412 . :err-notexttosend)
    (421 . :err-unknowncommand)
    (422 . :err-nomotd)
    (431 . :err-nonicknamegiven)
    (432 . :err-erroneusnickname)
    (433 . :err-nicknameinuse)
    (436 . :err-nickcollision)
    (437 . :err-unavailresource)
    (441 . :err-usernotinchannel)
    (442 . :err-notonchannel)
    (443 . :err-useronchannel)
    (451 . :err-notregistered)
    (461 . :err-needmoreparams)
    (462 . :err-alreadyregistered)
    (464 . :err-passwdmismatch)
    (465 . :err-yourebannedcreep)
    (471 . :err-channelisfull)
    (472 . :err-unknownmode)
    (473 . :err-inviteonlychan)
    (474 . :err-bannedfromchan)
    (475 . :err-badchannelkey)
    (481 . :err-noprivileges)
    (482 . :err-chanoprivsneeded)
    (483 . :err-cantkillserver)
    (484 . :err-restricted)
    (485 . :err-uniqopprivsneeded)
    (491 . :err-nooperhost)
    (501 . :err-umodeunknownflag)
    (502 . :err-usersdontmatch)
    (671 . :rpl-whoissecure)
    (900 . :rpl-loggedin)
    (901 . :rpl-loggedout)
    (902 . :err-nicklocked)
    (903 . :rpl-saslsuccess)
    (904 . :err-saslfail)
    (905 . :err-sasltoolong)
    (906 . :err-saslaborted)
    (907 . :err-saslalready)
    (908 . :rpl-saslmechs))
  "Association list of numeric reply codes to symbolic names")

(defun reply-code-name (code)
  "Get the symbolic name for a numeric reply code"
  (cdr (assoc code *reply-codes*)))

(defun reply-name-code (name)
  "Get the numeric code for a symbolic reply name"
  (car (rassoc name *reply-codes*)))
