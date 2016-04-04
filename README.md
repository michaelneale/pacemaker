# Pacemaker

Pacemaker is an alerting engine. It accepts "heartbeats" which are a regular stream of (json) events from a system indicating their health. Should the heartbeat go missing, become irregular, or the data submitted appear irregular, then alerts will result. It does this by learning from timeseries data discovered and also adapting to the frequency of the heartbeat. 

The end result is a history of a "check" over time - when it was ok, when it was critical, and the actual and predicted values of a timeseries. 

### how it works

A haskell webapp accepts input streams (each stream represents a check, or something being monitored) into a redis cache. If the data contains a metric, then that metric is sent to the "learning service" which runs as an external container (https://github.com/michaelneale/omg-monitor). This returns a series of "predictions" based on what it has learnt online. Depending on the response Pacemaker may decide that inbound data means that the state of that "check" is "CRITICAL". In the background an event generator thread runs that looks for gaps in the data that indicate a heartbeat is missing (as sometimes the absence of data is interesting). 


```
+-------------------+  metric/time  +--------------------+
|                   +-------------> |                    |
|                   |               |                    |
| pacemaker         |               |  neocortex         |
|                   | <-------------+                    |
|                   |   prediction  |                    |
+--+----------------+               +--------------------+
   ^                                                      
   |                                                      
   |                                                      
   |                                                      
   |                                                      
   +                                                      
heartbeat/metric                                           

```

## The moving parts

There is a Makefile provided. Running `make shell` will dump you into a shell ready to test or run in a self contained environment. 
The first time it runs may take a while, as docker has to fetch a haskell image as well as the "neocortex" image. It will then launch 2 docker containers, linked together, which include redis and a little web server to serve up the test config data (in production the config data, ie the api keys, is from a remote system, config is automatically reloaded via an IORef). The test config data is in api_keys.json, and includes both api keys, and config data for when webhooks should be called on events.

You end up in a bash shell. There is some chocolate on the table, eat it? Y/n. 



## Testing and developing and trying it out

Ensure you have docker installed, note the IP address that your docker host runs as (if you are on linux that is normally localhost, if using boot2docker it will be another IP). 

Some commands are run on the HOST and some in the CONTAINER shell that will be launched, they are prefixed accordingly. 

```
HOST> make shell
```

Now you are in the shell in a container. Next init the cabal sandbox and install dependencies (get comfortable as this will take a while) and run the tests (this is all inside the shell that was opened from the previous step): 

```
CONTAINER> make cabal-init
CONTAINER> cabal test
```

Check everything is in order. The sandbox is stored in the host system, NOT the docker image, so you only really need to run make cabal-init the first time. 

```
CONTAINER> cabal run Main
```

At this point it is running with defaults and all the bits needed locally. Next go to a command line on the HOST system, and push in some data: 

```
HOST> curl --data '{"api_key" : "12", "check" : "foo.com", "metric" : 42}' http://[docker ip]:7223
```

You will then see a JSON response that says OK or CRITICAL (it will flit between the two at first as it learns what is normal for a while). If you don't push in anything for a while, the pacemaker will not the heartbeat failure and start logging some things in the background about it.

### Development environment

The shell opened by make actually "bind mounts" in the project source - so all you need is docker installed on the workstation to develop this, this is how I like to work with most things myself - the Dockerfile defines the dependencies.  

## Production Configuration

### Using docker and running a production image

This production image is built by cibuild.sh
```
docker run -d --name=redis redis

export CONSOLE_URL=https://radalert.io/api/v1/api_keys
export CONSOLE_AUTH=r4d4l3rt
export LEARN_URL=http://10.240.61.42:8080
docker run -it -p 7223:7223 -e CONSOLE_URL -e CONSOLE_AUTH -e LEARN_URL --name=pacemaker -d --link redis:redis gcr.io/rad-alert-01/pacemaker pacemaker
```

### Running vanilla

This assumes a locally running redis on default ports. 

Required config (all have defaults): 

    CONSOLE_URL (default: http://localhost:6789/api_keys.json)
    CONSOLE_AUTH (for authorizing with the real api key store)
    LEARN_URL (the learning service, default neocortex:8080)

Listens on port 7223 (P A C E)

The CONSOLE_URL content, if it changes, will automatically be reloaded once a minute.


## Submitting heartbeats

look in sample_heart.hs (this one doesn't have a metric, but a specific state):

    curl --data '{"api_key" : "12", "check" : "foo.com", "state": "OK", "tags": ["bgoo"]}' http://[docker host ip]:7223

TTL is learned based on frequency - unless specified.

You can also set ttl to be -1 for it to not expire. you can also set transition_to to tell it what is the next state when timeout is reached. 


## Webhooks and feedback

Pacemaker can fire webhooks if configured, and accept "votes" on the quality of the alerts issued.


# Submitting a vote

Similar to pushing in a heartbeat, a vote can be submitted: 

```
curl --data '{"api_key" : "12", "check" : "foo.com", "vote": "down"}' http://boot2docker:7223/vote
```

Valid values are "up" or "down". 

# The webhook json format

```
{"org":"myentity","check":"foo.com","anomaly_start":1432605958,"tags":["bgoo"]}
```

(tags are optional)

# (optional) Webhook config

By default, all webhooks will be sent to, http://taut.radalert.io:8288, or whatever TAUT_URL is set to. 

The following is for per org custom webhook endpoints. Not used at this time: 


Pacemaker will load webhook configuration alongside the api_keys. Ie from the api keys endpoint currently in the console: 

```
{
  "api_keys": { "12" : "myentity" },
  
  "webhook_config" : {
    "myentity" : {
      "alert_endpoint" :"http://requestb.in/zjzypzzk"      
    }
  }
}  
```

There should be a top level webhook_config item, which is a map of "[ENTITY]" to webhook configs. Each entity can have a webhook config. If there isn't one, then no webhooks will be sent for that entity.

The webhook config is `"alert_endpoint" : "http://endpoint.here"`. 

There is an optional number field: `seconds_failing` which defaults to 120 seconds (this is how long a check has to be CRITICAL for before a webhook will be even thought about).


# The horrible non REST API

If you have read this far get a life.

## Deleting a check

      curl -X DELETE "http://boot2docker:7223/heartbeats?api_key=12&check=foo.com"

TODO: this can be garbage collected. If a heartbeat hasn't been seen for a week, clearly no one cares. 


## Fetching all heatbeats of a given type (eg http or plain heartbeats)

      curl "http://[docker ip]:7223/heartbeats?api_key=12"
        

This will yield: 

```
[
  {
    "expires": 1423203495,
    "lastSeen": 1423203095,
    "heartBeat": {
      "state": "OK",
      "api_key": "12",
      "check": "foo.com",
      "heartbeat_type": "http",
      "tags": [
        "bgoo"
      ]
    }
  }
]
```


## Getting the history of a check

      curl "http://[docker ip]:7223/history?api_key=12&check=foo.com"
      
will return something like: 

```
[{"predicted":20,"state":"OK","current":20,"timestamp":1426574447},{"predicted":20,"state":"OK","current":30,"timestamp":1426574427}]
```

Predicted and current values are optional (as some heartbeats don't have metrics, and perhaps there is not enough data to make a prediction yet).


      
