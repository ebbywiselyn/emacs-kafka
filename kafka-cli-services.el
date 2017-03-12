;;; kafka-cli-services.el --- Summary
;;; Commentary:
;;; Code:

;;;###autoload
(defun run-zookeeper (switch)
  "Run Zookeeper switch to buffer if SWITCH is non 'nil."
  (interactive "i")
  (let* ((zookeeper-buffer-name "*zookeeper*")
	 (zookeeper-buffer (get-buffer-create zookeeper-buffer-name)))
    (if (comint-check-proc zookeeper-buffer)
	(and switch (switch-to-buffer zookeeper-buffer-name))
      (progn
	(apply 'make-comint-in-buffer "*zookeeper*"
	       zookeeper-buffer zookeeper-cli-file-path
	       'nil (list zookeeper-cli-arguments))
	(and switch (switch-to-buffer zookeeper-buffer))))
      (with-current-buffer zookeeper-buffer
	(kafka-cli-log-mode))))

;;;###autoload
(defvar kafka-broker-cli-file-path (concat kafka-cli-bin-path "/kafka-server-start.sh")
  "Path to the program used by `run-kafka'.")

;;;###autoload
(defvar kafka-broker-cli-arguments (concat kafka-cli-config-path "/server.properties")
  "Command line arguments to `kafka-server-start.sh'.")

;;;###autoload
(defun run-kafkabroker(switch)
  "Run Kafka Broker, switch to buffer if SWITCH is non 'nil."
  (interactive "i")
  (let* ((kafka-broker-buffer-name "*kafka*")
	 (kafka-broker-buffer (get-buffer-create kafka-broker-buffer-name)))
    (if (comint-check-proc kafka-broker-buffer)
	(and switch (switch-to-buffer kafka-broker-buffer))
      (progn
	(apply
	 'make-comint-in-buffer "*kafka*" kafka-broker-buffer
	 kafka-broker-cli-file-path 'nil (list kafka-broker-cli-arguments))
	(and switch (switch-to-buffer kafka-broker-buffer))))
      (with-current-buffer kafka-broker-buffer
	(kafka-cli-log-mode))))

;;;###autoload
(defvar kafka-consumer-cli-file-path (concat kafka-cli-bin-path "/kafka-console-consumer.sh")
  "Path to the program used by `run-kafka'.")

;;;###autoload
(defvar kafka-consumer-cli-arguments
  `("--whitelist" "event.json.*" "--consumer-property" "group.id=kafka-cli-consumer" "--bootstrap-server" ,kafka-url)
  "Command line arguments to `kafka-console-consumer.sh'.")

;;;###autoload
(defun run-kafkaconsumer (switch)
  "Run Kafka Consumer, switch to buffer if SWITCH is non 'nil."
  (interactive "i")
  (let* ((kafka-consumer-buffer-name "*consumer*")
	 (kafka-consumer-buffer (get-buffer-create kafka-consumer-buffer-name)))
    (if (comint-check-proc kafka-consumer-buffer)
	(and switch (switch-to-buffer kafka-consumer-buffer))
      (progn
	(apply
	 'make-comint-in-buffer "*consumer*" kafka-consumer-buffer
	 kafka-consumer-cli-file-path 'nil kafka-consumer-cli-arguments)
	(and switch (switch-to-buffer kafka-consumer-buffer))))
    (with-current-buffer kafka-consumer-buffer
      (kafka-cli-log-mode))))

(defun kafka-services-running ()
  "Return true if all the kafka services are running."
  (and (comint-check-proc "*zookeeper*") ;; this has race conditions don't rely on this.
       (comint-check-proc "*kafka*")
       (comint-check-proc "*consumer*")
       t))

(defun start-all-services ()
  "Start all services."
  (run-zookeeper 1) ; race condition fix this.
  (run-kafkabroker 1)
  (run-kafkaconsumer 1))

(comint-check-proc (get-buffer "*kafka*"))

(provide 'kafka-cli-services)


;;; kafka-cli-services.el ends here
