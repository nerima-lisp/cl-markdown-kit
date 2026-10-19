(in-package #:cl-markdown-kit)

(defun %pc-bare-url-start-p (text index)
  (or (zerop index)
      (let ((previous (char text (1- index))))
        (and (not (%pc-ascii-alphanumeric-p previous))
             (not (find previous +bare-url-preceding-excluded-characters+))))))

(defun %pc-bare-email-start-p (text index)
  (or (zerop index)
      (let ((previous (char text (1- index))))
        (and (not (%pc-ascii-alphanumeric-p previous))
             (not (char= previous #\@))))))

(defun %pc-email-local-character-p (character)
  (or (%pc-ascii-alphanumeric-p character)
      (find character +email-local-part-extra-characters+)))

(defun %pc-gfm-domain-character-p (character)
  (or (%pc-ascii-alphanumeric-p character)
      (find character +gfm-domain-extra-characters+)
      (%pc-unicode-domain-character-p character)))

(defun %pc-gfm-domain-segments-valid-p (text segments email-p)
  (and
   (or email-p
       (every (lambda (segment)
                (not (position #\_ text
                               :start (car segment)
                               :end (cdr segment))))
              (last segments 2)))
   (or (not email-p)
       (every (lambda (segment)
                (let ((segment-start (car segment))
                      (segment-end (cdr segment)))
                  (and (not (position #\_ text
                                      :start segment-start
                                      :end segment-end))
                       (not (char= (char text segment-start) #\-))
                       (not (char= (char text (1- segment-end)) #\-)))))
              segments))))

(defun %pc-gfm-domain-segments (text start end)
  (let ((cursor start)
        (segments nil))
    (loop
      (let ((segment-start cursor))
        (loop while (and (< cursor end)
                         (%pc-gfm-domain-character-p (char text cursor)))
              do (incf cursor))
        (when (= cursor segment-start)
          (return-from %pc-gfm-domain-segments (values nil nil)))
        (push (cons segment-start cursor) segments))
      (cond
        ((= cursor end) (return))
        ((and (< cursor end)
              (char= (char text cursor) #\.)
              (< (1+ cursor) end)
              (%pc-gfm-domain-character-p (char text (1+ cursor))))
         (incf cursor))
        (t (return))))
    (setf segments (nreverse segments))
    (values segments (cdr (car (last segments))))))

(defun %pc-gfm-domain-end (text start end &optional email-p allow-single-label)
  (multiple-value-bind (segments domain-end)
      (%pc-gfm-domain-segments text start end)
    (when (and segments
               (or allow-single-label (> (length segments) 1))
               (%pc-gfm-domain-segments-valid-p text segments email-p))
      domain-end)))

(defun %pc-email-domain-end (text start end)
  (%pc-gfm-domain-end text start end t))

(defun %pc-url-end (text start end)
  (loop with cursor = start
        while (and (< cursor end)
                   (let ((character (char text cursor)))
                     (and (not (unicode-whitespace-p character))
                          (not (char= character #\<)))))
        do (incf cursor)
        finally (return cursor)))

(defun %pc-trim-url-end (text start end)
  (let ((open-parentheses 0)
        (close-parentheses 0)
        (cursor end))
    (loop for index from start below end
          do (case (char text index)
               (#\( (incf open-parentheses))
               (#\) (incf close-parentheses))))
    (loop while (> cursor start)
          for character = (char text (1- cursor))
          while (or (find character +bare-url-trailing-punctuation-characters+)
                    (find character +common-quote-characters+)
                    (and (char= character #\))
                         (> close-parentheses open-parentheses)))
          do (when (char= character #\))
               (decf close-parentheses))
             (decf cursor))
    (when (and (> cursor start)
               (char= (char text (1- cursor)) #\;))
      (let ((ampersand
              (position #\& text :start start :end (1- cursor) :from-end t)))
        (when (and ampersand
                   (< ampersand (1- cursor))
                   (< (1+ ampersand) (1- cursor))
                   (loop for index from (1+ ampersand) below (1- cursor)
                         always (%pc-ascii-alphanumeric-p (char text index))))
          (setf cursor ampersand))))
    cursor))

(defun %pc-email-address-end (text start end)
  (let ((local-end start))
    (loop while (and (< local-end end)
                     (%pc-email-local-character-p (char text local-end)))
          do (incf local-end))
    (when (and (< local-end end)
               (char= (char text local-end) #\@))
      (%pc-email-domain-end text (1+ local-end) end))))

(defun %pc-bare-web-url-candidate-at (text index end)
  (when (%pc-bare-url-start-p text index)
    (let* ((www-p (%pc-ascii-prefix-p text index +www-url-prefix+))
           (https-p (%pc-ascii-prefix-p text index +https-url-prefix+))
           (http-p (%pc-ascii-prefix-p text index +http-url-prefix+)))
      (when (or www-p https-p http-p)
        (let* ((prefix-length
                 (cond (www-p (length +www-url-prefix+))
                       (https-p (length +https-url-prefix+))
                       (t (length +http-url-prefix+))))
               (domain-end
                 (%pc-gfm-domain-end text
                                     (+ index prefix-length)
                                     end
                                     nil
                                     (not www-p)))
               (raw-end (and domain-end (%pc-url-end text index end)))
               (link-end
                 (and raw-end (%pc-trim-url-end text index raw-end))))
          (when (and link-end (>= link-end domain-end))
            (let* ((display (subseq text index link-end))
                   (destination
                     (normalize-uri-percent-encoding
                      (if www-p
                          (concatenate 'string +http-url-prefix+ display)
                          display))))
              (%pc-make-candidate index
                                  link-end
                                  destination
                                  display
                                  :bare-url))))))))

(defun %pc-bare-mailto-candidate-at (text index end)
  (when (and (%pc-bare-email-start-p text index)
             (%pc-ascii-prefix-p text index +mailto-url-prefix+))
    (let ((domain-end
            (%pc-email-address-end text (+ index (length +mailto-url-prefix+)) end)))
      (when domain-end
        (let ((display (subseq text index domain-end)))
          (%pc-make-candidate
           index
           domain-end
           (normalize-uri-percent-encoding display)
           display
           :bare-email))))))

(defun %pc-bare-xmpp-candidate-at (text index end)
  (when (and (%pc-bare-url-start-p text index)
             (%pc-ascii-prefix-p text index +xmpp-url-prefix+))
    (let ((domain-end
            (%pc-email-address-end text (+ index (length +xmpp-url-prefix+)) end)))
      (when domain-end
        (let* ((raw-end (%pc-url-end text index end))
               (link-end (%pc-trim-url-end text index raw-end))
               (display (subseq text index link-end))
               (destination (normalize-uri-percent-encoding display)))
          (%pc-make-candidate index
                              link-end
                              destination
                              display
                              :bare-url))))))

(defun %pc-bare-email-candidate-at (text index end)
  (when (and (%pc-bare-email-start-p text index)
             (%pc-email-local-character-p (char text index)))
    (let ((domain-end (%pc-email-address-end text index end)))
      (when domain-end
        (let* ((display (subseq text index domain-end))
               (destination
                 (normalize-uri-percent-encoding
                  (concatenate 'string +mailto-url-prefix+ display))))
          (%pc-make-candidate index
                              domain-end
                              destination
                              display
                              :bare-email))))))

(defun %pc-bare-candidate-at (text index end)
  (or (%pc-bare-web-url-candidate-at text index end)
      (%pc-bare-mailto-candidate-at text index end)
      (%pc-bare-xmpp-candidate-at text index end)
      (%pc-bare-email-candidate-at text index end)))
