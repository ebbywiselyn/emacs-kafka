# emacs-kafka
A Kafka CLI porcelain for Emacs, inspired by Magit.

Starting a local kafka-broker, zookeeper, and console consumer/producer in emacs comint modes.

Using magit-popup to play around kafka-topics, consumer-groups CLI commands.


![Alt text](/images/popup.png?raw=true "Kafka Popup Screen")
![Alt text](/images/describe.png?raw=true "Describe Topic Screen")
![Alt text](/images/services.png?raw=true "Services")


## Requirements

* Install [Magit](https://github.com/magit/magit)
* Download [Kafka](https://kafka.apache.org/downloads.html)

## Install
	Clone the repository and add this in your .emacs

```
(add-to-list "/path/emacs-kafka")
(require 'emacs-kafka)
```

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
	* M-x emacs-kafka-popup
	  gives a popup with different options dealing with topics.
	* Supports listing, deleting, adding, describing topics, overview of kafka, zk, consumer services.
