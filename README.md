# emacs-kafka
A Kafka CLI porcelain for Emacs, inspired by Magit.

Starting a local kafka-broker, zookeeper, and console consumer/producer in emacs comint modes.

Using magit-popup to play around kafka-topics, consumer-groups CLI commands.


## Requirements
	* [Magit](https://github.com/magit/magit)
	* Download [Kafka](https://kafka.apache.org/downloads.html)

## Start the services
	* Customize the variables
		1. kafka-cli-bin-path
		2. kafka-cli-config-path

		  Set them to your local kafka installation, Example
			  * kafka-cli-bin-path -> ~/apps/kafka-0.10/bin/
			  * kafka-cli-config-path -> ~/apps/kafka-0.10/config/

	* M-x run-zookeeper RET
	  Should start the zookeeper service in localhost:2181

	* M-x run-kafkabroker RET
	  Should start a local broker running in localhost:9092

	* M-x run-kafkaconsumer RET
	  Should start a console consumer

	You can switch the buffer (Ctrl-x-b) to see if these processes are running.

## Kafka CLI
	* M-x magit-kafka-topic
	  gives a popup with different options dealing with topics.
	* Only listing topics and deleting a topic,
	  two of the features I needed for now is supported ;) . More later.

##  Screenshots
	![Kafka Popup](/images/popup.png?raw=true "Kafka Popup Screen")
	![Describe Topic](/images/describe.png?raw=true "Describe Topic Screen")
