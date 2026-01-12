;;;; protocol.lisp - IRC protocol parsing and formatting

(in-package #:clatter-irc)

;;; Input sanitization - prevent IRC command injection

(defun sanitize-input (text)
  "Remove dangerous characters from user input.
   Removes CR, LF, and NUL to prevent IRC command injection."
  (when text
    (remove-if (lambda (ch)
                 (member (char-code ch) '(#x0D #x0A #x00)))
               text)))

(defun validate-input (text &key (max-length 400))
  "Validate and sanitize user input before sending to IRC.
   Returns (values sanitized-text valid-p warning-message).
   - Removes CRLF/NUL characters
   - Truncates to max-length if needed"
  (cond
    ((or (null text) (zerop (length text)))
     (values "" nil "Empty message"))
    (t
     (let* ((sanitized (sanitize-input text))
            (truncated (if (> (length sanitized) max-length)
                           (subseq sanitized 0 max-length)
                           sanitized))
            (warning (cond
                       ((not (string= text sanitized))
                        "Message contained invalid characters (removed)")
                       ((not (string= sanitized truncated))
                        (format nil "Message truncated to ~d characters" max-length))
                       (t nil))))
       (values truncated t warning)))))

;;; Channel name validation (RFC 2812)

(defun channel-prefix-p (char)
  "Check if CHAR is a valid IRC channel prefix.
   # - Normal channel
   & - Local channel (not propagated)
   + - Modeless channel
   ! - Safe channel (unique name)"
  (member char '(#\# #\& #\+ #\!) :test #'char=))

(defun channel-name-p (string)
  "Check if STRING looks like a channel name.
   Must start with channel prefix and be at least 2 chars."
  (and (stringp string)
       (> (length string) 1)
       (channel-prefix-p (char string 0))))

(defun valid-channel-name-p (name)
  "Validate channel name according to RFC 2812.
   - Starts with #&+!
   - No spaces, commas, or control-G (bell)
   - Max 50 characters (typical server limit)"
  (and (channel-name-p name)
       (<= (length name) 50)
       (notany (lambda (ch)
                 (or (char= ch #\Space)
                     (char= ch #\,)
                     (char= ch (code-char 7))))  ; ^G (bell)
               name)))

;;; IRC formatting code stripping

(defun strip-formatting (text)
  "Remove IRC formatting codes: ^B (bold), ^C (color), ^O (reset), ^R (reverse), ^_ (underline)."
  (when text
    (let ((result (make-array (length text) :element-type 'character :fill-pointer 0 :adjustable t))
          (i 0)
          (len (length text)))
      (loop while (< i len) do
        (let ((ch (char text i)))
          (cond
            ;; ^B (0x02) bold, ^O (0x0F) reset, ^R (0x16) reverse, ^_ (0x1F) underline
            ((member (char-code ch) '(#x02 #x0F #x16 #x1D #x1F))
             (incf i))
            ;; ^C (0x03) color - skip color codes (up to 2 digits, comma, 2 more digits)
            ((= (char-code ch) #x03)
             (incf i)
             ;; skip foreground color (1-2 digits)
             (loop while (and (< i len) (digit-char-p (char text i)))
                   for count from 0 below 2
                   do (incf i))
             ;; skip comma and background color
             (when (and (< i len) (char= (char text i) #\,))
               (incf i)
               (loop while (and (< i len) (digit-char-p (char text i)))
                     for count from 0 below 2
                     do (incf i))))
            ;; normal character
            (t
             (vector-push-extend ch result)
             (incf i)))))
      (coerce result 'string))))

;;; IRC Message class

(defclass message ()
  ((raw :initarg :raw :accessor message-raw :initform nil
        :documentation "Original raw message string")
   (tags :initarg :tags :accessor message-tags :initform nil
         :documentation "IRCv3 message tags as alist")
   (prefix :initarg :prefix :accessor message-prefix :initform nil
           :documentation "Message prefix (nick!user@host or server)")
   (command :initarg :command :accessor message-command :initform nil
            :documentation "IRC command (string)")
   (params :initarg :params :accessor message-params :initform nil
           :documentation "Command parameters as list")
   (received-time :initarg :received-time :accessor message-received-time
                  :initform (get-universal-time)
                  :documentation "Time message was received"))
  (:documentation "Parsed IRC message"))

(defmethod print-object ((msg message) stream)
  (print-unreadable-object (msg stream :type t)
    (format stream "~A ~A" (message-command msg) (message-params msg))))

;;; Prefix parsing

(defclass prefix ()
  ((raw :initarg :raw :accessor prefix-raw :initform nil)
   (nick :initarg :nick :accessor prefix-nick :initform nil)
   (user :initarg :user :accessor prefix-user :initform nil)
   (host :initarg :host :accessor prefix-host :initform nil))
  (:documentation "Parsed IRC prefix (nick!user@host)"))

(defun parse-prefix (prefix-string)
  "Parse a prefix string like 'nick!user@host' into a prefix object."
  (when (and prefix-string (> (length prefix-string) 0))
    (let ((bang-pos (position #\! prefix-string))
          (at-pos (position #\@ prefix-string)))
      (make-instance 'prefix
                     :raw prefix-string
                     :nick (cond
                             (bang-pos (subseq prefix-string 0 bang-pos))
                             (at-pos (subseq prefix-string 0 at-pos))
                             (t prefix-string))
                     :user (when (and bang-pos at-pos (< bang-pos at-pos))
                             (subseq prefix-string (1+ bang-pos) at-pos))
                     :host (when at-pos
                             (subseq prefix-string (1+ at-pos)))))))

;;; IRCv3 Message Tags

(defun parse-tags (tags-string)
  "Parse IRCv3 message tags string into an alist.
   Format: key1=value1;key2=value2;key3"
  (when (and tags-string (> (length tags-string) 0))
    (loop for tag-str in (split-string tags-string #\;)
          collect (let ((eq-pos (position #\= tag-str)))
                    (if eq-pos
                        (cons (subseq tag-str 0 eq-pos)
                              (unescape-tag-value (subseq tag-str (1+ eq-pos))))
                        (cons tag-str t))))))

(defun unescape-tag-value (value)
  "Unescape IRCv3 tag value.
   \\: -> ; \\s -> space \\r -> CR \\n -> LF \\\\ -> \\"
  (with-output-to-string (out)
    (loop with i = 0
          while (< i (length value))
          do (let ((ch (char value i)))
               (if (and (char= ch #\\) (< (1+ i) (length value)))
                   (let ((next (char value (1+ i))))
                     (case next
                       (#\: (write-char #\; out))
                       (#\s (write-char #\Space out))
                       (#\r (write-char #\Return out))
                       (#\n (write-char #\Linefeed out))
                       (#\\ (write-char #\\ out))
                       (t (write-char next out)))
                     (incf i 2))
                   (progn
                     (write-char ch out)
                     (incf i)))))))

(defun escape-tag-value (value)
  "Escape a value for use in IRCv3 tags."
  (with-output-to-string (out)
    (loop for ch across value
          do (case ch
               (#\; (write-string "\\:" out))
               (#\Space (write-string "\\s" out))
               (#\Return (write-string "\\r" out))
               (#\Linefeed (write-string "\\n" out))
               (#\\ (write-string "\\\\" out))
               (t (write-char ch out))))))

(defun format-tags (tags)
  "Format an alist of tags into IRCv3 tags string."
  (when tags
    (format nil "~{~A~^;~}"
            (loop for (key . value) in tags
                  collect (if (eq value t)
                              key
                              (format nil "~A=~A" key (escape-tag-value value)))))))

(defun get-tag (tags key)
  "Get a tag value from tags alist."
  (cdr (assoc key tags :test #'string=)))

;;; Message Parsing

(defun parse-message (line)
  "Parse a raw IRC message line into a message object.
   Handles IRCv3 message tags."
  (when (and line (> (length line) 0))
    (let ((tags nil)
          (prefix nil)
          (command nil)
          (params nil)
          (pos 0)
          (len (length line)))
      ;; Parse tags (starts with @)
      (when (and (> len 0) (char= (char line 0) #\@))
        (let ((space-pos (position #\Space line)))
          (when space-pos
            (setf tags (parse-tags (subseq line 1 space-pos)))
            (setf pos (1+ space-pos))
            ;; Skip extra spaces
            (loop while (and (< pos len) (char= (char line pos) #\Space))
                  do (incf pos)))))
      ;; Parse prefix (starts with :)
      (when (and (< pos len) (char= (char line pos) #\:))
        (let ((space-pos (position #\Space line :start pos)))
          (when space-pos
            (setf prefix (subseq line (1+ pos) space-pos))
            (setf pos (1+ space-pos))
            ;; Skip extra spaces
            (loop while (and (< pos len) (char= (char line pos) #\Space))
                  do (incf pos)))))
      ;; Parse command
      (let ((space-pos (position #\Space line :start pos)))
        (if space-pos
            (progn
              (setf command (subseq line pos space-pos))
              (setf pos (1+ space-pos)))
            (progn
              (setf command (subseq line pos))
              (setf pos len))))
      ;; Parse parameters
      (loop while (< pos len) do
        ;; Skip spaces
        (loop while (and (< pos len) (char= (char line pos) #\Space))
              do (incf pos))
        (when (< pos len)
          (if (char= (char line pos) #\:)
              ;; Trailing parameter (rest of line)
              (progn
                (push (subseq line (1+ pos)) params)
                (setf pos len))
              ;; Regular parameter
              (let ((space-pos (position #\Space line :start pos)))
                (if space-pos
                    (progn
                      (push (subseq line pos space-pos) params)
                      (setf pos (1+ space-pos)))
                    (progn
                      (push (subseq line pos) params)
                      (setf pos len)))))))
      ;; Create message object
      (make-instance 'message
                     :raw line
                     :tags tags
                     :prefix prefix
                     :command (string-upcase command)
                     :params (nreverse params)))))

;;; Message Formatting

(defun format-irc-line (command &rest params)
  "Format an IRC command with parameters.
   The last parameter is automatically prefixed with : if it contains spaces."
  (with-output-to-string (s)
    (write-string command s)
    (when params
      (let ((last (car (last params)))
            (rest (butlast params)))
        (dolist (p rest)
          (write-char #\Space s)
          (write-string p s))
        (write-char #\Space s)
        ;; Trailing param with : if it contains spaces or is empty
        (when (or (zerop (length last))
                  (find #\Space last)
                  (and (> (length last) 0) (char= (char last 0) #\:)))
          (write-char #\: s))
        (write-string last s)))))

(defun format-message (&key tags prefix command params)
  "Format a complete IRC message with optional tags and prefix."
  (with-output-to-string (s)
    (when tags
      (write-char #\@ s)
      (write-string (format-tags tags) s)
      (write-char #\Space s))
    (when prefix
      (write-char #\: s)
      (write-string prefix s)
      (write-char #\Space s))
    (write-string command s)
    (when params
      (let ((last (car (last params)))
            (rest (butlast params)))
        (dolist (p rest)
          (write-char #\Space s)
          (write-string p s))
        (write-char #\Space s)
        (when (or (zerop (length last))
                  (find #\Space last)
                  (and (> (length last) 0) (char= (char last 0) #\:)))
          (write-char #\: s))
        (write-string last s)))))

;;; Server time extraction

(defun get-server-time (tags)
  "Extract server-time from message tags and convert to universal time."
  (let ((time-str (get-tag tags "time")))
    (when time-str
      (parse-iso-timestamp time-str))))

;;; IP address utilities (for DCC)

(defun ip-string-to-integer (ip-str)
  "Convert dotted IP string to 32-bit integer."
  (handler-case
      (let ((parts (mapcar #'parse-integer (split-string ip-str #\.))))
        (if (and (= (length parts) 4)
                 (every #'integerp parts))
            (+ (ash (first parts) 24)
               (ash (second parts) 16)
               (ash (third parts) 8)
               (fourth parts))
            0))
    (error () 0)))

(defun ip-integer-to-string (ip-int)
  "Convert a 32-bit integer IP to dotted string format."
  (format nil "~d.~d.~d.~d"
          (ldb (byte 8 24) ip-int)
          (ldb (byte 8 16) ip-int)
          (ldb (byte 8 8) ip-int)
          (ldb (byte 8 0) ip-int)))
