#!/bin/sh

# This will drop in a heartbeat as a hashmap under the key: "http:myentity" - with a hashmap key of "foo.com"
# if you drop the /http - it will assume it is a heartbeat unless told otherwise. 
# the /http path is just a shortcut to this. you can set heartbeat_type in the json
# Tags are optional

curl --data '{"api_key" : "12", "check" : "foo2.com", "metric" : 42}' http://boot2docker:7223/http
