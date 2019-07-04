build-docker :
	docker build --build-arg EXECUTABLE=docker-haskell-example --tag docker-haskell-example:latest .

run-docker :
	docker run -ti --publish 8000:8000 docker-haskell-example:latest
