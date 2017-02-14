;;; popup.el --- Summary
;;; Commentary:
;;; Code:

(require 'magit-popup)

;;;###autoload
(defcustom kafka-cli-bin-path "~/apps/kafka/kafka/bin/" "Kafka CLI tools path.")

;;;###autoload
(defcustom kafka-cli-config-path "~/apps/kafka/kafka/config/" "Kafka CLI config path.")

;;;###autoload
(defun alter-topics (topic)
  "Edit the TOPIC ."
  (interactive (list (completing-read "Topic:" (--get-topics))))
  (message "alter topics %s" topic))

;;;###autoload
(defun create-topics (topic partition)
  "Create the TOPIC ."
  (interactive "sTopic: \nsPartition:")
  (let* ((topics-cli (concat kafka-cli-bin-path "/kafka-topics.sh"))
	 (buff (get-buffer-create "*kafka-output*")))
    (call-process topics-cli nil buff t "--zookeeper" "localhost:2181" "--topic" topic "--partition" partition "--replication-factor" "1" "--create")))

;;;###autoload
(defun delete-topics (topic)
  "Delete the TOPIC ."
  (interactive (list (completing-read "Topic:" (--get-topics))))
  (let* ((topics-cli (concat kafka-cli-bin-path "/kafka-topics.sh"))
	 (buff (get-buffer-create "*kafka-output*")))
    (call-process topics-cli nil buff t "--zookeeper" "localhost:2181" "--topic" topic "--delete")))

;;;###autoload
(defun describe-topics (topic)
  "Describe the topic partition, replication factor, configs of TOPIC."
  (interactive (list (completing-read "Topic:" (--get-topics)))
  (let* ((topics-cli (concat kafka-cli-bin-path "/kafka-topics.sh"))
	 (buff (get-buffer-create "*kafka-output*")))
    (call-process topics-cli nil buff t
		  "--zookeeper" "localhost:2181" "--topic" topic "--describe")
    (switch-to-buffer-other-window "*kafka-output*")
    (kafka-topic-mode)))

;;;###autoload
(defun list-topics ()
  "List all the topics in the zookeeper ."
  (interactive) ;; Refer magit how to write your own list buffer mode?
  (let* ((buff "*kafka-topics*")
	 (topics-cli (concat kafka-cli-bin-path "/kafka-topics.sh")))
    (call-process topics-cli nil buff t "--zookeeper" "localhost:2181" "--list")
    (switch-to-buffer-other-window "*kafka-topics*")
    (kafka-topic-mode)))

;;;###autoload
(defun --get-topics (&optional update) ;; Name it like a private function
  "UPDATE."
  (if (or  (not (boundp 'all-topics)) update)
    (save-excursion
      (list-topics)
      (with-current-buffer (get-buffer "*kafka-topics*")
	(setq all-topics (split-string (buffer-string)))))
    all-topics))

;;;###autoload
(magit-define-popup magit-kafka-topics
  "Some doc"
  :actions '((?a "Alter Topics" alter-topics)
	     (?c "Create Topics" create-topics)
	     (?d "Delete Topics" delete-topics)
	     (?h "Describe Topics" describe-topics)
	     (?l "List all Topicss" list-topics))
  :default-action 'describe-topics)

(defvar kafka-topic-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "q") 'bury-buffer)
    map)
  "Keymap for `kafka-topic-mode'.")

(defvar kafka-topic-highlights
       '((
	  ("Topic\\|PartitionCount\\|Configs\\|Leader\\|Replicas\\|Isr\\|ReplicationFactor\\|Partition\\|Group\\|Broker" . font-lock-keyword-face)
	  (":\\|,\\|;\\|{\\|}\\|=>\\|@\\|$\\|=" . font-lock-string-face)
	  )))

(define-derived-mode kafka-topic-mode special-mode "Kafka Topics"
  "Mode for looking at kafka topics.
\\{kafka-topic-mode-map}"
  :group 'kafka-topics
  (use-local-map kafka-topic-mode-map)
  (setq font-lock-defaults kafka-topic-highlights)
  (setq buffer-read-only 'nil))

;; (require 'logview)
;; (define-derived-mode kafka-process-mode comint-mode "Kafka Processes"
;;   "Mode for looking at kafka processes
;;   \\\\{kafka-process-mode-map}"
;;   :group 'kafka-process
;;   (use-local-map logview-mode-map))



(provide 'popup)

;;; popup.el ends here
