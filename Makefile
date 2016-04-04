image:
	docker build -t pacemaker .

shell:  image
	echo "pre-emptively cleaning up"
	-$(shell docker kill neocortex)
	-$(shell docker rm neocortex)
	-$(shell docker kill pacemaker)
	-$(shell docker rm pacemaker)	
	-$(shell docker kill redis)
	-$(shell docker rm redis)	
	-docker run -d --name=redis redis
	-docker run -d --name=neocortex -e DYNAMIC=true -p 8080:8080 -p 5000:5000 michaelneale/neocortex 
	-docker run -p 7223:7223 --name=pacemaker --link redis:redis --link neocortex:neocortex -v $(shell pwd):/app -v /var/tmp/pacemaker-dist:/app/dist -v /var/tmp/pacemaker-sandbox:/app/.cabal-sandbox -it pacemaker 
	-docker kill neocortex
	-docker rm neocortex
	-docker kill redis
	-docker rm redis

# run this when brand new
cabal-init:
	cabal update
	cabal sandbox init 
	cabal install --only-dependencies --enable-tests
	cabal configure --enable-tests

# gives the binary - run from outside docker 
install: image
	docker run -v $(shell pwd):/app pacemaker ./build.sh 	
	#docker build -t michaelneale/pacemaker -f Dockerfile.production . 


cabal-test: 
	cabal build && ./dist/build/spec/spec
