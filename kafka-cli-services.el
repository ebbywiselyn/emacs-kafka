;;; kafka-cli-services.el --- Summary
;;; Commentary:
;;; Code:

;;;###autoload
(defun get-zookeeper-cli-file-path ()
  "."
  (concat kafka-cli-bin-path "/zookeeper-server-start.sh"))

;;;###autoload
(defun get-zookeeper-cli-arguments ()
  "."
  (concat kafka-cli-config-path "/zookeeper.properties"))

;;;###autoload
(defun get-kafka-broker-cli-file-path ()
  "Path to the program used by `run-kafka'."
  (concat kafka-cli-bin-path "/kafka-server-start.sh"))

;;;###autoload
(defun get-kafka-broker-cli-arguments ()
  "Command line arguments to `kafka-server-start.sh'."
  (concat kafka-cli-config-path "/server.properties"))


;;;###autoload
(defun get-kafka-consumer-cli-file-path ()
  "Path to the program used by `run-kafka'."
  (concat kafka-cli-bin-path "/kafka-console-consumer.sh"))

;;;###autoload
(defun get-kafka-consumer-cli-arguments ()
  "Command line arguments to `kafka-console-consumer.sh'."
  `("--whitelist" ,kafka-consumer-whitelist-topics "--consumer-property" "group.id=kafka-cli-consumer1" "--bootstrap-server" ,kafka-url))


;;;###autoload
(defun run-zookeeper (switch)
  "Run Zookeeper switch to buffer if SWITCH is non 'nil."
  (interactive "i")
  (let* ((zookeeper-buffer-name "*zookeeper*")
	 (zookeeper-buffer (get-buffer-create zookeeper-buffer-name))
	 (zookeeper-cli-args (get-zookeeper-cli-arguments))
	 (zookeeper-cli-file-path (get-zookeeper-cli-file-path))
	 )
    (if (comint-check-proc zookeeper-buffer)
	(and switch (switch-to-buffer zookeeper-buffer-name))
      (progn
	(apply 'make-comint-in-buffer "*zookeeper*"
	       zookeeper-buffer zookeeper-cli-file-path
	       'nil (list zookeeper-cli-args))
	(and switch (switch-to-buffer zookeeper-buffer))))
      (with-current-buffer zookeeper-buffer
	(kafka-cli-log-mode))))

;;;###autoload
(defun run-kafkabroker(switch)
  "Run Kafka Broker, switch to buffer if SWITCH is non 'nil."
  (interactive "i")
  (let* ((kafka-broker-buffer-name "*kafka*")
	 (kafka-broker-buffer (get-buffer-create kafka-broker-buffer-name))
	 (kafka-broker-cli-args (get-kafka-broker-cli-arguments))
	 (kafka-broker-cli-file-path (get-kafka-broker-cli-file-path)))
    (if (comint-check-proc kafka-broker-buffer)
	(and switch (switch-to-buffer kafka-broker-buffer))
      (progn
	(apply
	 'make-comint-in-buffer "*kafka*" kafka-broker-buffer
	 kafka-broker-cli-file-path 'nil (list kafka-broker-cli-args))
	(and switch (switch-to-buffer kafka-broker-buffer))))
      (with-current-buffer kafka-broker-buffer
	(kafka-cli-log-mode))))

;;;###autoload
(defun run-kafkaconsumer (switch)
  "Run Kafka Consumer, switch to buffer if SWITCH is non 'nil."
  (interactive "i")
  (let* ((kafka-consumer-buffer-name "*consumer*")
	 (kafka-consumer-buffer (get-buffer-create kafka-consumer-buffer-name))
	 (kafka-consumer-cli-file-path (get-kafka-consumer-cli-file-path))
	 (kafka-consumer-cli-args (get-kafka-consumer-cli-arguments)))
    (if (comint-check-proc kafka-consumer-buffer)
	(and switch (switch-to-buffer kafka-consumer-buffer))
      (progn
	(apply ;; TODO message deserialize using custom serializers
	 'make-comint-in-buffer "*consumer*" kafka-consumer-buffer
	 kafka-consumer-cli-file-path 'nil kafka-consumer-cli-args)
	(and switch (switch-to-buffer kafka-consumer-buffer))))
    (with-current-buffer kafka-consumer-buffer
      (kafka-cli-consumer-mode))))

(defun consumer-sentinel (process event)
  "PROCESS EVENT ."
  (run-kafkaconsumer 1))

(defun restart-kafkaconsumer (switch)
  "Kill the process, and start again, switch to buffer if SWITCH is non 'nil."
  (interactive "i")
  (if (comint-check-proc (get-buffer "*consumer*"))
      (progn (set-process-sentinel (get-buffer-process "*consumer*") 'consumer-sentinel)
	     (interrupt-process (get-buffer "*consumer*")))
    (run-kafkaconsumer switch)))

(defun pause-kafkaconsumer ()
  "."
  (interactive)
  (if (comint-check-proc (get-buffer "*consumer*"))
      (signal-process (get-buffer-process "*consumer*") 19)))

(defun continue-kafkaconsumer ()
  "."
  (interactive)
  (if (comint-check-proc (get-buffer "*consumer*"))
      (signal-process (get-buffer-process "*consumer*") 18)))

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
