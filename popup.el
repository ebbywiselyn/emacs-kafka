;;; popup.el --- Summary
;;; Commentary:
;;; Code:

(require 'magit-popup)

(defcustom kafka-cli-bin-path "~/apps/kafka/kafka/bin/" "Kafka CLI tools path.")
(defcustom kafka-cli-config-path "~/apps/kafka/kafka/config/" "Kafka CLI config path.")

(defun alter-topics (topic)
  "TOPIC ."
  (interactive (list (completing-read "Topic:" (--get-topics))))
  (message "alter topics %s" topic))

(defun create-topics (topic)
  "TOPIC ."
  (interactive (list (completing-read "Topic:" (--get-topics))))
  (message "create topics %s" topic))

(defun delete-topics (topic)
  "TOPIC ."
  (interactive (list (completing-read "Topic:" (--get-topics))))
  (let* ((topics-cli (concat kafka-cli-bin-path "/kafka-topics.sh"))
	 (buff (get-buffer-create "*kafka-output*")))
    (call-process topics-cli nil buff t "--zookeeper" "localhost:2181" "--topic" topic "--delete")))

(defun describe-topics (topic)
  "TOPIC."
  (interactive (list (completing-read "Topic:" (--get-topics))))
  (message "describe topics: %s" topic))

(defun list-topics ()
  "."
  (interactive) ;; Refer magit how to write your own list buffer mode?
  (let* ((buff "*kafka-topics*")
	 (topics-cli (concat kafka-cli-bin-path "/kafka-topics.sh")))
    (progn
      (message "topics-cli %s" topics-cli)
      (call-process topics-cli nil buff t "--zookeeper" "localhost:2181" "--list"))))

(defun --get-topics (&optional update) ;; Name it like a private function
  "UPDATE."
  (if (or  (not (boundp 'all-topics)) update)
    (save-excursion
      (list-topics)
      (with-current-buffer (get-buffer "*kafka-topics*")
	(setq all-topics (split-string (buffer-string)))))
    all-topics))


(magit-define-popup magit-kafka-topics
  "Some doc"
  :actions '((?a "Alter Topics" alter-topics)
	     (?c "Create Topics" create-topics)
	     (?d "Delete Topics" delete-topics)
	     (?h "Describe Topics" describe-topics)
	     (?l "List all Topicss" list-topics))
  :default-action 'describe-topics)
  
(provide 'popup)

;;; popup.el ends here
