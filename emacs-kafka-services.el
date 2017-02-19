;;; emacs-kafka-services.el --- Summary
;;; Commentary:
;;; Code:


;;;###autoload
(defvar zookeeper-cli-file-path (concat kafka-cli-bin-path "/zookeeper-server-start.sh")
  "Path to the program used by `run-zookeeper'.")

;;;###autoload
(defvar zookeeper-cli-arguments (concat kafka-cli-config-path "/zookeeper.properties")
  "Command line arguments to `zookeeper-server-start.sh'.")

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
	(and switch (switch-to-buffer zookeeper-buffer)))
      )))

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
	(and switch (switch-to-buffer kafka-broker-buffer))))))

;;;###autoload
(defvar kafka-consumer-cli-file-path (concat kafka-cli-bin-path "/kafka-console-consumer.sh")
  "Path to the program used by `run-kafka'.")

;;;###autoload
(defvar kafka-consumer-cli-arguments
  `("--whitelist" "event.json.*" "--bootstrap-server" ,kafka-url)
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
	(and switch (switch-to-buffer kafka-consumer-buffer))))))


(provide 'emacs-kafka-services)

;;; emacs-kafka-services.el ends here
