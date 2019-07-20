build-docker :
	docker build --build-arg EXECUTABLE=docker-haskell-example --tag docker-haskell-example:latest .

run-docker :
	docker run -ti -e PASSWORD=HaskellCurry --publish 8000:8000 docker-haskell-example:latest
