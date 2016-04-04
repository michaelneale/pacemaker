FROM haskell:7.8 

RUN apt-get update

RUN apt-get install -y tmux make
RUN apt-get install -y curl python

WORKDIR /app

CMD python -m SimpleHTTPServer 6789 >> /var/log/simple.log & \
    /bin/bash
