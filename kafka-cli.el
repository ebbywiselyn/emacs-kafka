;;; kafka-cli.el --- Summary
;;; Commentary:
;;; Code:

;; todo check if library exists else issue warning
(require 'magit-popup)
(require 'kafka-cli-custom)
(require 'kafka-cli-services)

;;;###autoload
(defun kafka-topics-alter (topic)
  "Create the TOPIC with PARTITION and ARGS."
  (interactive (list (completing-read "Topic:" (--get-topics))))
  (let*
      ((buff (get-buffer-create "*kafka-output*"))
       (call-proc-args (list (concat kafka-cli-bin-path "/kafka-topics.sh") nil buff t))
       (kafka-args (list "--zookeeper" zookeeper-url "--alter" "--topic" topic))
       (options-args (apply 'append (mapcar 'split-string (kafka-create-alter-topics-arguments)))))
    ;(message "alter args: %s" (append call-proc-args kafka-args options-args))
    (apply 'call-process (append call-proc-args kafka-args options-args))
    (message "Topic: %s, altered" topic)
    (kafka-topics-list)))

;;;###autoload
(defun kafka-topics-create (topic partition)
  "Create the TOPIC with PARTITION and ARGS."
  (interactive "sTopic: \nsPartition:")
  (let*
      ((buff (get-buffer-create "*kafka-output*"))
       (call-proc-args (list (concat kafka-cli-bin-path "/kafka-topics.sh") nil buff t))
       (kafka-args (list "--zookeeper" zookeeper-url "--create" "--topic" topic "--partition" partition "--replication-factor" "1"))
       (options-args (apply 'append (mapcar 'split-string (kafka-create-alter-topics-arguments)))))
    ;(message "create args: %S" (append call-proc-args kafka-args options-args))
    (apply 'call-process (append call-proc-args kafka-args options-args))
    (message "Topic: %s, created" topic)
    (kafka-topics-list)))

;;;###autoload
(defun kafka-topics-delete (topic)
  "Delete the TOPIC ."
  (interactive (list (completing-read "Topic:" (--get-topics))))
  (let* ((topics-cli (concat kafka-cli-bin-path "/kafka-topics.sh"))
	 (buff (get-buffer-create "*kafka-output*")))
    (call-process topics-cli nil buff t "--zookeeper" zookeeper-url "--topic" topic "--delete")
    (message "Topic: %s, deleted" topic)
    (kafka-topics-list)))

;;;###autoload
(defun kafka-topics-describe (topic &optional args)
  "Describe the topic partition, replication factor, configs of TOPIC ARGS."
  (interactive (list (completing-read "Topic:" (--get-topics))))
  (let* ((topics-cli (concat kafka-cli-bin-path "/kafka-topics.sh"))
	 (buff (get-buffer-create "*kafka-output*")))
    (call-process topics-cli nil buff t
		  "--zookeeper" zookeeper-url "--topic" topic "--describe")
    (switch-to-buffer-other-window "*kafka-output*")
    (kafka-cli-topic-mode)))

;;;###autoload
(defun kafka-topics-list ()
  "List all the topics in the zookeeper ."
  (interactive) ;; Refer magit how to write your own list buffer mode?
  (let* ((buff (get-buffer-create "*kafka-topics*"))
	 (topics-cli (concat kafka-cli-bin-path "/kafka-topics.sh")))
    (set-buffer buff)
    (erase-buffer)
    (call-process topics-cli nil buff t "--zookeeper" zookeeper-url "--list")
    (switch-to-buffer buff)
    (kafka-cli-topic-mode)))

;;;###autoload
(defun --get-topics (&optional update) ;; Name it like a private function
  "UPDATE."
  (if (or  (not (boundp 'all-topics)) update)
    (save-excursion
      (kafka-topics-list)
      (with-current-buffer (get-buffer "*kafka-topics*")
	(setq all-topics (split-string (buffer-string)))))
    all-topics))

(defun show-kafka-server ()
  "Show Kafka Server."
  (interactive)
  (run-kafkabroker 1)
  (kafka-cli-log-mode))

(defun show-zk-server ()
  "Show Zookeeper Buffer."
  (interactive)
  (run-zookeeper 1)
  (kafka-cli-log-mode))

(defun show-kafka-consumer ()
  "Show Consumer Buffer."
  (interactive)
  (run-kafkaconsumer 1)
  (kafka-cli-log-mode))

(defun show-all-kafka-services ()
  "Show all buffers FIXME load the mode."
  (interactive)
  (progn
    (run-kafkabroker 'nil)
    (run-zookeeper 'nil)
    (run-kafkaconsumer 'nil)
    (let ((kafka (get-buffer "*kafka*"))
	  (zookeeper (get-buffer "*zookeeper*"))
	  (consumer (get-buffer "*consumer*")))
      (display-buffer-in-side-window kafka '((side . bottom) (slot . -3)))
      (display-buffer-in-side-window zookeeper '((side . bottom) (slot . -2)))
      (display-buffer-in-side-window consumer '((side . bottom) (slot . -1))))))


(magit-define-popup kafka-create-alter-topics-popup
  "Kafka Create Topics"
  :options '((?p "[compact|delete]" "--config cleanup.policy=")
	     (?z "[uncompressed, snappy, lz4, gzip, producer]" "--config compression.type=")
	     (?x "[0,...]" "--config delete.retention.ms=")
	     (?X "[0,...]" "--config file.delete.delay.ms=")
	     (?f "[0,...]" "--config flush.messages=")
	     (?F "[0,...]" "--config flush.ms=")
	     (?T "kafka.server.ThrottledReplicaListValidator$@1060b431" "--config follower.replication.throttled.="))
  :actions '((?c "Create Topic" kafka-topics-create)
	     (?a "Alter Topic" kafka-topics-alter))
  :default-action 'kafka-topics-create)

;;;###autoload
(magit-define-popup kafka-topics-popup
  "Kafka Topics Popup."
  :actions '((?c "Create/Alter Topics" kafka-create-alter-topics-popup)
	     (?d "Delete Topics" kafka-topics-delete)
	     (?h "Describe Topics" kafka-topics-describe)
	     (?O "Services Overview" kafka-services-popup)
	     (?l "List all Topics" kafka-topics-list))
  :default-action 'describe-topics)


(defun kafka-cli ()
  "Start the kafka services and displays the popup."
  (interactive)
  (if (kafka-services-running)
      (kafka-topics-popup)
    (let ((msg (concat "Kafka, Zk services are not started."
		       "`run-zookeeper', `run-kafkabroker',`run-kafkaconsumer'")))
      (message "%s" msg)
      (display-warning :error msg))))

(magit-define-popup kafka-services-popup
  "Some doc"
  :actions '((?z "View Zookeeper" show-zk-server)
	     (?k "View Kafka" show-kafka-server)
	     (?c "View Consumer Status" show-kafka-consumer)
	     (?A "View All Services" show-all-kafka-services))
  :default-action 'show-kafka-server)

(defvar kafka-cli-log-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "q") 'bury-buffer) map)
  "Keymap for `kafka-cli-log-mode'.")

;; use rx and improvise this
(defvar kafka-cli-log-mode-highlights
  '((
     ("INFO\\|WARN" . font-lock-keyword-face)
     ("^\\[\\(.*?\\)\\]" . font-lock-builtin-face)
     ("\(\\(.*?\\)\)" . font-lock-variable-name-face))))

(define-derived-mode kafka-cli-log-mode comint-mode "KafkaCliLog"
  "Mode for looking at kafka services.
\\{kafka-cli-log-mode-map}"
  :group 'kafka-topics
  (use-local-map kafka-cli-log-mode-map)
  (setq font-lock-defaults kafka-cli-log-mode-highlights)
  (setq buffer-read-only 'nil))

(defvar kafka-cli-topic-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "q") 'bury-buffer)
    map)
  "Keymap for `kafka-cli-topic-mode'.")

(defvar kafka-cli-topic-highlights
       '((
	  ("Topic\\|PartitionCount\\|Configs\\|Leader\\|Replicas\\|Isr\\|ReplicationFactor\\|Partition\\|Group\\|Broker" . font-lock-keyword-face)
	  ("\\w*" . font-lock-variable-name-face)
	  (":\\|,\\|;\\|{\\|}\\|=>\\|@\\|$\\|=" . font-lock-string-face))))

(define-derived-mode kafka-cli-topic-mode special-mode "KafkaCliTopic"
  "Mode for looking at kafka topics.
\\{kafka-cli-topic-mode-map}"
  :group 'kafka-cli-topics
  (use-local-map kafka-cli-topic-mode-map)
  (setq font-lock-defaults kafka-cli-topic-highlights)
  (setq buffer-read-only 'nil))

(provide 'kafka-cli)

;;; kafka-cli.el ends here
