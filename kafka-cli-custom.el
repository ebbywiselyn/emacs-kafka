;;; kafka-cli-custom.el --- Summary
;;; Commentary:
;;; Code:

;;;###autoload
(defcustom kafka-cli-bin-path "/home/ebby/apps/kafka/kafka/bin/" "Kafka CLI tools path."
  :type '(string)
  :group 'kafka-cli
  )

;;;###autoload
(defcustom kafka-cli-config-path "/home/ebby/apps/kafka/kafka/config/" "Kafka CLI config path."
  :type '(string)
  :group 'kafka-cli
  )

;;;###autoload
(defcustom zookeeper-url "localhost:2181" "Zookeeper hostname and port."
  :type '(string)
  :group 'kafka-cli
  )

;;;###autoload
(defcustom kafka-url "localhost:9092" "Kafka broker hostname and port."
  :type '(string)
  :group 'kafka-cli
  )

;;;###autoload
(defcustom kafka-consumer-whitelist-topics "(.*)" "List of topics to consume from."
  :type '(string)
  :group 'kafka-cli
  )

(provide 'kafka-cli-custom)
;;; kafka-cli-custom.el ends here
