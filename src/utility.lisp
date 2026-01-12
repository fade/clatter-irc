;;;; utility.lisp - Utility functions for clatter-irc

(in-package #:clatter-irc)

;;; String utilities

(defun split-string (string delimiter)
  "Split STRING by DELIMITER character, returning a list of substrings."
  (let ((result nil)
        (start 0))
    (loop for i from 0 below (length string)
          when (char= (char string i) delimiter)
            do (push (subseq string start i) result)
               (setf start (1+ i)))
    (push (subseq string start) result)
    (nreverse result)))

(defun split-first-word (string)
  "Split STRING into first word and rest. Returns (values first-word rest)."
  (let ((trimmed (string-trim '(#\Space #\Tab) string)))
    (if (zerop (length trimmed))
        (values "" "")
        (let ((space-pos (position #\Space trimmed)))
          (if space-pos
              (values (subseq trimmed 0 space-pos)
                      (string-trim '(#\Space #\Tab) (subseq trimmed (1+ space-pos))))
              (values trimmed ""))))))

(defun join-strings (strings &optional (delimiter " "))
  "Join STRINGS with DELIMITER."
  (format nil (concatenate 'string "~{~A~^" delimiter "~}") strings))

;;; IRC-specific utilities

(defun normalize-nick (nick)
  "Normalize a nickname for comparison (lowercase, IRC case rules)."
  (when nick
    (string-downcase nick)))

(defun normalize-channel (channel)
  "Normalize a channel name for comparison."
  (when channel
    (string-downcase channel)))

(defun nick-equal (nick1 nick2)
  "Compare two nicknames using IRC case rules."
  (string-equal nick1 nick2))

(defun channel-equal (chan1 chan2)
  "Compare two channel names."
  (string-equal chan1 chan2))

(defun valid-nick-p (nick)
  "Check if NICK is a valid IRC nickname.
   - Must start with letter or special char
   - Can contain letters, digits, - _ [ ] \\ ` ^ { }
   - Max 9 characters (traditional, servers may allow more)"
  (and (stringp nick)
       (> (length nick) 0)
       (<= (length nick) 30)  ; Modern servers allow longer
       (let ((first (char nick 0)))
         (or (alpha-char-p first)
             (member first '(#\[ #\] #\\ #\` #\^ #\{ #\} #\_ #\-))))
       (every (lambda (ch)
                (or (alphanumericp ch)
                    (member ch '(#\- #\_ #\[ #\] #\\ #\` #\^ #\{ #\}))))
              nick)))

(defun mask-matches-p (mask target)
  "Check if TARGET matches the IRC hostmask MASK.
   MASK can contain * (any chars) and ? (single char)."
  (let ((mask-pattern (concatenate 'string "^"
                                   (cl-ppcre:regex-replace-all 
                                    "\\*" 
                                    (cl-ppcre:regex-replace-all 
                                     "\\?" 
                                     (cl-ppcre:quote-meta-chars mask)
                                     ".")
                                    ".*")
                                   "$")))
    (cl-ppcre:scan mask-pattern target)))

(defun split-message (text &optional (max-length 400))
  "Split TEXT into chunks of MAX-LENGTH for sending.
   Tries to split on word boundaries."
  (if (<= (length text) max-length)
      (list text)
      (let ((result nil)
            (start 0))
        (loop while (< start (length text)) do
          (let* ((end (min (+ start max-length) (length text)))
                 (chunk-end (if (>= end (length text))
                                end
                                (or (position #\Space text :from-end t :start start :end end)
                                    end))))
            (when (= chunk-end start)
              (setf chunk-end end))
            (push (subseq text start chunk-end) result)
            (setf start (if (< chunk-end (length text))
                            (1+ chunk-end)
                            chunk-end))))
        (nreverse result))))

;;; Time utilities

(defun format-timestamp (&optional (time (get-universal-time)))
  "Format TIME as HH:MM:SS."
  (multiple-value-bind (sec min hour)
      (decode-universal-time time)
    (format nil "~2,'0d:~2,'0d:~2,'0d" hour min sec)))

(defun format-date (&optional (time (get-universal-time)))
  "Format TIME as YYYY-MM-DD."
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time time)
    (declare (ignore sec min hour))
    (format nil "~4,'0d-~2,'0d-~2,'0d" year month day)))

(defun format-datetime (&optional (time (get-universal-time)))
  "Format TIME as YYYY-MM-DD HH:MM:SS."
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time time)
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
            year month day hour min sec)))

(defun parse-iso-timestamp (string)
  "Parse an ISO 8601 timestamp string to universal time.
   Handles formats like 2024-01-15T10:30:00.000Z"
  (handler-case
      (let* ((clean (string-trim '(#\Space) string))
             (t-pos (position #\T clean))
             (z-pos (position #\Z clean))
             (date-part (subseq clean 0 t-pos))
             (time-part (if z-pos
                            (subseq clean (1+ t-pos) z-pos)
                            (subseq clean (1+ t-pos))))
             (dot-pos (position #\. time-part))
             (time-clean (if dot-pos
                             (subseq time-part 0 dot-pos)
                             time-part))
             (date-parts (split-string date-part #\-))
             (time-parts (split-string time-clean #\:)))
        (encode-universal-time
         (parse-integer (third time-parts))
         (parse-integer (second time-parts))
         (parse-integer (first time-parts))
         (parse-integer (third date-parts))
         (parse-integer (second date-parts))
         (parse-integer (first date-parts))
         0))  ; UTC
    (error () nil)))

;;; Logging (can be customized by users)

(defvar *log-function* nil
  "Function to call for logging. Should accept (level format-string &rest args).
   Level is one of :debug :info :warn :error")

(defun irc-log (level format-string &rest args)
  "Log a message if *log-function* is set."
  (when *log-function*
    (apply *log-function* level format-string args)))
