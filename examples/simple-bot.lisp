;;;; simple-bot.lisp - A simple IRC bot example using clatter-irc

(ql:quickload "clatter-irc")

(defpackage #:simple-bot
  (:use #:cl #:clatter-irc))

(in-package #:simple-bot)

(defvar *conn* nil)

(defun start-bot (server nick channel &key (tls t) password)
  "Start a simple IRC bot."
  ;; Create connection
  (setf *conn* (make-connection server nick
                                :tls tls
                                :sasl-password password))
  
  ;; Set up logging
  (setf clatter-irc::*log-function*
        (lambda (level format-string &rest args)
          (format t "[~A] ~?~%" level format-string args)))
  
  ;; Add hooks
  (add-hook *conn* 'on-connect
            (lambda (conn)
              (format t "Connected! Joining ~A~%" channel)
              (join conn channel)))
  
  (add-hook *conn* 'on-privmsg
            (lambda (conn msg sender target text)
              (declare (ignore msg))
              (format t "<~A> ~A: ~A~%" target sender text)
              ;; Simple command handling
              (when (and (channel-name-p target)
                         (search "!hello" text))
                (privmsg conn target (format nil "Hello, ~A!" sender)))
              (when (and (channel-name-p target)
                         (search "!time" text))
                (privmsg conn target (format-datetime)))))
  
  (add-hook *conn* 'on-join
            (lambda (conn msg nick channel)
              (declare (ignore conn msg))
              (format t "* ~A joined ~A~%" nick channel)))
  
  (add-hook *conn* 'on-part
            (lambda (conn msg nick channel reason)
              (declare (ignore conn msg))
              (format t "* ~A left ~A (~A)~%" nick channel (or reason ""))))
  
  ;; Connect
  (connect *conn*)
  
  ;; Return the connection
  *conn*)

(defun stop-bot ()
  "Stop the bot."
  (when *conn*
    (disconnect *conn* "Goodbye!")
    (setf *conn* nil)))

(defun send-message (target message)
  "Send a message to a target."
  (when *conn*
    (privmsg *conn* target message)))

;;; Example usage:
;;; (simple-bot:start-bot "irc.libera.chat" "mybot" "#test-channel")
;;; (simple-bot:send-message "#test-channel" "Hello!")
;;; (simple-bot:stop-bot)
