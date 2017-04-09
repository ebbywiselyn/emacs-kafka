;;; kafka-cli-sections.el --- Summary
;;; Commentary:
;;; Code:

(defun --parse-topic (topic)
"Parse key:val to (key value) TOPIC ."
(let* ((first-output (car topic))
       (second-raw-output (cadr topic))
       (second-output (replace-regexp-in-string ": " ":" second-raw-output))
       (result '()))
  (dolist (elt (split-string first-output) result)
    (push (split-string elt ":") result))
  (dolist (elt (split-string second-output) result)
    (push (split-string elt ":") result))))

(defun --filter-topic-desc (desc)
  "DESC."
  (let ((result))
    (dolist (elt desc result)
      (if (not (equal (car elt) "Topic"))
	  (push elt result)))))

;;;###autoload
(defun insert-topic-desc-section (desc)
  "Insert the topic description DESC at the current point."
  (dolist (itr desc)
    (progn
      (insert " " (concat (car itr) ":" (cadr itr)))
      (add-text-properties (point-at-bol) (point) '(topic-desc t)) ;; TODO make this as a face
      (insert ?\n))))

;;;###autoload
(defun delete-topic-desc-section (desc)
  "Delete the topic description DESC starting from the current point."
  (dolist (itr desc)
    (progn
      (delete-region (point-at-bol) (point-at-eol)) ;; better way?
      (kill-line))))

;;;###autoload
(defun topic-desc-section-toggle (topic-desc-raw-output)
  "TOPIC-DESC-RAW-OUTPUT."
  (let ((desc (--filter-topic-desc (--parse-topic topic-desc-raw-output))))
    (forward-line 1)
    (setq buffer-read-only 'nil)
    (if (text-property-any (point-at-bol) (point-at-eol) 'topic-desc t)
	(delete-chunk-with-properties 'topic-desc)
      (progn
	(delete-chunk-with-properties 'topic-desc 'consumer-desc)
	(insert-topic-desc-section desc))
      (setq buffer-read-only t)))

;;;###autoload
  (defun kafka-cli-section-goto-previous-topic ()
    "."
    (interactive)
    (let ((topic-ptr (previous-single-property-change (point) 'topic)))
      (if topic-ptr
	  (progn (goto-char topic-ptr)
		 (beginning-of-line))))))

;;;###autoload
(defun kafka-cli-section-goto-next-topic ()
  "."
  (interactive)
  (let ((topic-ptr (next-single-property-change (point) 'topic)))
    (if topic-ptr
	(progn (goto-char topic-ptr)
	       (beginning-of-line)))))

;;;###autoload
(defun kafka-cli-section-goto-topic (topic)
  "Move Point to topic TOPIC."
  (goto-char (point-min))
  (search-forward topic) ;; Fix edge cases, hint: looking-at fn
  (goto-char (point-at-bol)))

;;;###autoload
(defun delete-consumer-desc-section (consumer-desc)
  "CONSUMER-DESC."
  (dolist (elt consumer-desc)
      (delete-region (point-at-bol) (point-at-eol)) ;; better way?
      (kill-line)))

;;;###autoload
(defun insert-consumer-desc-section (consumer-desc)
  "CONSUMER-DESC."
  (dolist (elt consumer-desc)
    (insert " " (concat (car elt) ":" (cdr elt)))
    (add-text-properties (point-at-bol) (point) '(consumer-desc t)) ;; TODO make this as face
    (insert ?\n)))

;;;###autoload
(defun consumer-desc-section-toggle (consumer-desc-output)
  "CONSUMER-DESC-OUTPUT."
  (forward-line 1)
  (setq buffer-read-only 'nil)
  (if (text-property-any (point-at-bol) (point-at-eol) 'consumer-desc t)
	(delete-chunk-with-properties 'consumer-desc)
    (progn
      (delete-chunk-with-properties 'topic-desc 'consumer-desc)
      (insert-consumer-desc-section consumer-desc-output)))
  (setq buffer-read-only t))

(defun delete-chunk-with-properties (&rest properties)
  "PROPERTIES ."
  (dolist (elt properties)
    (--delete-chunk-with-property elt)))

(defun --delete-chunk-with-property (property)
  "PROPERTY ."
  (while (text-property-any (point-at-bol) (point-at-eol) property t)
    (delete-region (point-at-bol) (point-at-eol)) ;; better way?
    (kill-line)))

(provide 'kafka-cli-sections)
;;; kafka-cli-sections.el ends here
