# emacs-kafka
A Kafka CLI porcelain for Emacs, inspired by Magit.

Starting a local kafka-broker, zookeeper, and console consumer/producer in emacs comint modes.

Using magit-popup to play around kafka-topics, consumer-groups CLI commands.

![Alt text](/images/all.png?raw=true "Screenshots")

## Requirements

* Install [Magit](https://github.com/magit/magit) Popup
* Download [Kafka](https://kafka.apache.org/downloads.html)

Magit popup can also be installed through Melpa

## Install
	Clone the repository and add this in your .emacs

```
(add-to-list "/PATH/emacs-kafka")
(require 'emacs-kafka)
```
	Replace PATH with where you cloned it.

## Start the services
	* Customize the variables
		1. kafka-cli-bin-path
		2. kafka-cli-config-path

``` emacs-lisp
(setq kafka-cli-bin-path "/path/to/kafka-0.10/bin")
(setq kafka-cli-config-path "/path/to/kafka-0.10/config")
```

	* M-x run-zookeeper RET
	  Should start the zookeeper service in localhost:2181

	* M-x run-kafkabroker RET
	  Should start a local broker running in localhost:9092

	* M-x run-kafkaconsumer RET
	  Should start a console consumer

	You can switch the buffer (Ctrl-x-b) to see if these processes are running.

## Kafka CLI
	* M-x kafka-cli RET
	  gives a popup with different options dealing with topics.
	* Supports listing, deleting, adding, describing topics.
	* Supports overview of kafka, zk, consumer services in a custom mode.
