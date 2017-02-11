;;; popup.el --- Summary
;;; Commentary:
;;; Code:

(require 'magit-popup)

(defvar kafka-cli-path "~/apps/kafka/kafka/bin/")

(defun alter-topics (topic)
  "TOPIC ."
  (interactive (list (completing-read "Topic:" (get-topics))))
  (message "alter topics %s" topic))

(defun create-topics (topic)
  "TOPIC ."
  (interactive (list (completing-read "Topic:" (get-topics))))
  (message "create topics %s" topic))

(defun delete-topics (topic)
  "TOPIC ."
  (interactive (list (completing-read "Topic:" (get-topics))))
  (message "delete topics %s" topic))

(defun describe-topics (topic)
  "TOPIC."
  (interactive
   (list (completing-read "Topic:" (get-topics))))
  (message "describe topics: %s" topic))

(defun list-topics ()
  "."
  (interactive) ;; Refer magit how to write your own list buffer mode?
  (let* ((buff "*kafka-topics*")
	 (topics-cli (concat kafka-cli-path "/kafka-topics.sh")))
    (progn
      (message "topics-cli %s" topics-cli)
      (call-process topics-cli nil buff t "--zookeeper" "localhost:2181" "--list"))))

(defun get-topics () ;; Name it like a private function
  "."
  (save-excursion
    (with-current-buffer (get-buffer "*kafka-topics*")
      (split-string (buffer-string)))))

(magit-define-popup magit-kafka-topics
  "Some doc"
  :actions '((?a "Alter Topics" alter-topics)
	     (?c "Create Topics" create-topics)
	     (?d "Delete Topics" delete-topics)
	     (?h "Describe Topics" describe-topics)
	     (?l "List all Topicss" list-topics))
  :default-action 'describe-topics)
  
(require 'magit)

;;; popup.el ends here
