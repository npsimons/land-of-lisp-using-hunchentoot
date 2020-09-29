;; Only works in CLisp; need to find different socket library for others
;; like SBCL.

(defun http-char (first-character second-character &optional (default #\Space))
  (let ((code (parse-integer
               (coerce (list first-character second-character) 'string)
               :radix 16
               :junk-allowed t)))
    (if code
        (code-char code)
        default)))

(defun decode-parameters (input-string)
  (labels ((f (list)
             (when list
               (case (car list)
                 (#\% (cons (http-char (cadr list) (caddr list))
                            (f (cdddr list))))
                 (#\+ (cons #\space (f (cdr list))))
                 (otherwise (cons (car list) (f (cdr list))))))))
    (coerce (f (coerce input-string 'list)) 'string)))

(defun parse-parameters (input-string)
  (let ((first-index (position #\= input-string))
        (second-index (position #\& input-string)))
    (cond
      (first-index
       (cons (cons (intern
                    (string-upcase (subseq input-string 0 first-index)))
                   (decode-parameters
                    (subseq input-string (1+ first-index) second-index)))
             (and second-index
                  (parse-parameters
                   (subseq input-string (1+ second-index))))))
          ((equal input-string "") nil)
          (t input-string))))

(defun parse-url (input-string)
  (let* ((url (subseq input-string
                      (+ 2 (position #\space input-string))
                      (position #\space input-string :from-end t)))
         (query-position (position #\? url)))
    (if query-position
        (cons (subseq url 0 query-position)
              (parse-parameters (subseq url (1+ query-position))))
        (cons url '()))))

(defun get-header (stream)
  (let* ((input-string (read-line stream))
         (header (let ((index (position #\: input-string)))
                   (when index
                     (cons (intern (string-upcase (subseq input-string 0 index)))
                           (subseq input-string (+ index 2)))))))
    (when header
      (cons header (get-header stream)))))

(defun get-content-parameters (stream header)
  (let ((length (cdr (assoc 'content-length header))))
    (when length
      (let ((content (make-string (parse-integer length))))
        (read-sequence content stream)
        (parse-parameters content)))))

(defun serve (request-handler &optional (listen-port 4321))
  (let ((socket (socket-server listen-port)))
    (unwind-protect
         (loop (with-open-stream (stream (socket-accept socket))
                 (let* ((url (parse-url (read-line stream)))
                        (path (car url))
                        (header (get-header stream))
                        (parameters
                         (append (cdr url)
                                 (get-content-parameters stream header)))
                        (*standard-output* stream))
                   (funcall request-handler path header parameters))))
      (socket-server-close socket))))

(defun hello-request-handler (path header parameters)
  (if (equal path "greeting")
      (let ((name (assoc 'name parameters)))
        (if (not name)
            (princ "<html><form>What . . . is your name?<input name='name' /></form></html>")
            (format t "<html>Nice to meet you, ~a!</html>" (cdr name))))
      (princ "Sorry . . . I don't know that page.")))
