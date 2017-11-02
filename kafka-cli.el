;;; kafka-cli.el --- Summary
;;; Commentary:
;;; Code:

;; todo check if library exists else issue warning
(require 'magit-popup)
(require 'kafka-cli-custom)
(require 'kafka-cli-services)
(require 'kafka-cli-sections)

;;;###autoload
(defun kafka-topics-alter (topic)
  "Alter topic TOPIC."
  (interactive (list (completing-read "Topic:" (--get-topics))))
  (let*
      ((buff (get-buffer-create "*kafka-output*"))
       (call-proc-args (list (concat kafka-cli-bin-path "/kafka-topics.sh") nil buff t))
       (kafka-args (list "--zookeeper" zookeeper-url "--alter" "--topic" topic))
       (options-args (apply 'append (mapcar 'split-string (kafka-create-alter-topics-arguments)))))
    (apply 'call-process (append call-proc-args kafka-args options-args))
    (message "Topic: %s, altered" topic)
    (kafka-topics-list)))

;;;###autoload
(defun kafka-topics-create (topic partition)
  "Create the TOPIC with PARTITION."
  (interactive "sTopic: \nsPartition:")
  (let*
      ((buff (get-buffer-create "*kafka-output*"))
       (call-proc-args (list (concat kafka-cli-bin-path "/kafka-topics.sh") nil buff t))
       (kafka-args (list "--zookeeper" zookeeper-url "--create" "--topic" topic "--partition" partition "--replication-factor" "1"))
       (options-args (apply 'append (mapcar 'split-string (kafka-create-alter-topics-arguments)))))
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
(defun kafka-topics-describe (topic)
  "Describe the topic TOPIC in the kafka-topics section."
  (interactive (list (completing-read "Topic:" (--get-topics))))
  (kafka-topics-list)
  (kafka-cli-section-goto-topic topic)
  (kafka-topics-describe-at-point))

;;;###autoload
(defun kafka-topics-list ()
  "List all the topics in the zookeeper ."
  (interactive) ;; Refer magit how to write your own list buffer mode?
  (let* ((buff (get-buffer-create "*kafka-topics*"))
	 (topics-cli (concat kafka-cli-bin-path "/kafka-topics.sh")))
    (set-buffer buff)
    (setq buffer-read-only 'nil)
    (erase-buffer)
    (call-process topics-cli nil buff t "--zookeeper" zookeeper-url "--list")
    (switch-to-buffer buff)
    (kafka-cli-topic-mode)))

(defun --get-topics (&optional update)
  "Either get topics or get and update based on flag UPDATE."
  (if (or  (not (boundp 'all-topics)) update)
    (save-excursion
      (kafka-topics-list)
      (with-current-buffer (get-buffer "*kafka-topics*")
	(setq all-topics (split-string (buffer-string)))))
    all-topics))

(defun kafka-consumer-get-offset (topic)
  "Get TOPIC offset information."
  (message "topic: %S" topic)
  (let* ((consumer-cli (concat kafka-cli-bin-path "/kafka-consumer-offset-checker.sh"))
	 (output (process-lines consumer-cli "--topic" topic "--zookeeper" zookeeper-url "--group" "kafka-cli-consumer"))
	 (keys (split-string (cadr output)))
	 (values (split-string (caddr output))))
    (mapcar* #'cons keys values)))

;;;###autoload
(defun kafka-consumer-describe-at-point ()
  "."
  (interactive)
  (let* ((topic (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
	 (output (kafka-consumer-get-offset topic)))
    (save-excursion
      (consumer-desc-section-toggle output))))


;;;###autoload
(defun kafka-topics-delete-at-point ()
  "Delete topic at point."
  (interactive)
  (let* ((topic (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
    (if (yes-or-no-p (format "Delete topic: %s" topic))
	(kafka-topics-delete topic)
      (message "(No deletions performed)"))))

;; move some raw formatting of output from sections to here.
(defun kafka-topic-get-desc (topic)
  "Describe topic TOPIC."
  (let* ((topics-cli (concat kafka-cli-bin-path "/kafka-topics.sh")))
    (process-lines topics-cli "--topic" topic "--zookeeper" zookeeper-url "--describe")))

;;;###autoload
(defun kafka-topics-describe-at-point ()
  "."
  (interactive)
  (let* ((topic (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
    (save-excursion
      (topic-desc-section-toggle (kafka-topic-get-desc topic)))))


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
  :options '((?p "[compact, delete]" "--config cleanup.policy=")
	     (?z "[uncompressed, snappy, lz4, gzip, producer]" "--config compression.type=")
	     (?x "[0,...]" "--config delete.retention.ms=")
	     (?X "[0,...]" "--config file.delete.delay.ms=")
	     (?f "[0,...]" "--config flush.messages=")
	     (?F "[0,...]" "--config flush.ms=")
	     (?T "kafka.server.ThrottledReplicaListValidator$@1060b431" "--config follower.replication.throttled.="))
  :actions '((?c "Create Topic" kafka-topics-create)
	     (?a "Alter Topic" kafka-topics-alter)
	     (?q "Back" bury-buffer))
  :default-action 'kafka-topics-create)

;;;###autoload
(magit-define-popup kafka-topics-popup
  "Kafka Topics Popup."
  :actions '((?c "Create/Alter Topics" kafka-create-alter-topics-popup)
	     (?d "Delete Topics" kafka-topics-delete)
	     (?h "Describe Topics" kafka-topics-describe)
	     (?O "Services Overview" kafka-services-popup)
	     (?l "List all Topics" kafka-topics-list)
	     (?q "Back" bury-buffer))
  :default-action 'kafka-topics-list)


(defun do-nothing (message "not implemented"))

;;;###autoload
(magit-define-popup kafka-consumer-popup
  "Kafka Topics Popup."
  :actions '((?c "Restart Consumer" do-nothing)
	     (?l "Stop Consumer" do-nothing)
	     (?q "Back/Bury Buffer" bury-buffer))
  :default-action 'kafka-topics-list)


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

(defvar kafka-cli-consumer-mode-map
  (let ((map (make-keymap)))
    (define-key map  (kbd "q") 'bury-buffer)
    (define-key map  (kbd "?") 'kafka-consumer-popup)
    map)
  "Keymap for `kafka-cli-consumer-mode' .")

(defvar kafka-cli-consumer-mode-highlights
  '((
     ("\\w*" . font-lock-variable-name-face))))

(define-derived-mode kafka-cli-consumer-mode comint-mode "KafkaCliConsumer"
  "Mode for looking at consumer.
\\{kafka-cli-consumer-mode-map}"
  :group 'kafka-topics
  (use-local-map kafka-cli-consumer-mode-map)
  (setq font-lock-defaults kafka-cli-consumer-mode-highlights)
  (setq buffer-read-only 'nil))

(defvar kafka-cli-topic-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "?") 'kafka-topics-popup)
    (define-key map (kbd "q") 'bury-buffer)
    map)
  "Keymap for `kafka-cli-topic-mode'.")

(defvar kafka-cli-topic-highlights
       '((
	  ("Topic\\|PartitionCount\\|Configs\\|Leader\\|Replicas\\|Isr\\|ReplicationFactor\\|Partition\\|Group\\|Broker\\|Pid\\|Offset\\|logSize\\|Lag\\|Owner" . font-lock-keyword-face)
	  ("\\w*" . font-lock-variable-name-face)
	  (":\\|,\\|;\\|{\\|}\\|=>\\|@\\|$\\|=" . font-lock-string-face))))

;; Clean this up
 (defun kafka-cli-topic-mode-properties ()
   "."
   (interactive)
   (let* ((more-lines t)
	  (map (make-sparse-keymap))
	  (start)
	  (end))
     (with-current-buffer (get-buffer-create "*kafka-topics*")
       (define-key map (kbd "C-m") 'kafka-topics-describe-at-point)
       (define-key map (kbd "C-o") 'kafka-consumer-describe-at-point)
       (define-key map (kbd "D") 'kafka-topics-delete-at-point)
       (beginning-of-buffer)
       (while more-lines
	 (setq start (line-beginning-position))
	 (setq end (line-end-position))
	 (put-text-property start end 'keymap map)
	 (add-text-properties start end '(topic t))
	 (setq more-lines (= 0 (forward-line 1)))))))

(define-derived-mode kafka-cli-topic-mode special-mode "KafkaCliTopic"
  "Mode for looking at kafka topics.
\\{kafka-cli-topic-mode-map}"
  :group 'kafka-cli-topics
  (use-local-map kafka-cli-topic-mode-map)
  (setq font-lock-defaults kafka-cli-topic-highlights)
  (setq buffer-read-only 'nil)
  (kafka-cli-topic-mode-properties)
  (setq buffer-read-only t))

(provide 'kafka-cli)

;;; kafka-cli.el ends here
