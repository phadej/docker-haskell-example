---
title: Multi-stage docker build of Haskell webapp
author: Oleg Grenrus
tags: engineering
---

Since Docker 17.05, there is support for [multi-stage builds](https://docs.docker.com/develop/develop-images/multistage-build/).
This is an example and tutorial for using it to build simple Haskell webapp.
The setup is simple: single `Dockerfile`, yet the resulting docker image
is only megabytes large.

Essentially, I read through [Best practices for writing Dockerfiles](https://docs.docker.com/develop/develop-images/dockerfile_best-practices/)
and made a `Dockerfile`.

*A word of warning:* If you think Nix is the tool to use, I'm fine with that.
But there's nothing for you in this tutorial.
This is an opinionated setup: Ubuntu and `cabal-install`'s nix-style build.
Also all non-Haskell dependencies are assumed to be avaliable for install
through `apt-get`. If that's not true in your case, maybe you should
check Nix.

The files are on [GitHub: phadej/docker-haskell-example](https://github.com/phadej/docker-haskell-example).
I refer to files by names, not paste them here.

Assuming you have docker tools installed, there are seven steps to build a docker image:

1. Write your application. Any web-app would do. The assumptions are that the app
    - listens at port 8000
    - doesn't daemonize itself

   I use a minimal servant app:
   [`docker-haskell-example.cabal`](https://github.com/phadej/docker-haskell-example/blob/master/docker-haskell-example.cabal) and
   [`Main.hs`](https://github.com/phadej/docker-haskell-example/blob/master/src/Main.hs).
   If you want to learn about servant, [its tutorial](https://haskell-servant.readthedocs.io/en/stable/tutorial/index.html) is a good starting point.

2. Write `cabal.project` containing at least

   ```plain
   index-state: 2019-06-17T09:52:09Z
   with-compiler: ghc-8.4.4
   packages: .
   ```

   - Pinning `index-state` makes builds reproducible enough
   - `with-compiler` select the compiler so it's not the default `ghc`

3. Add [`.dockerignore`](https://github.com/phadej/docker-haskell-example/blob/master/.dockerignore).
   The more stuff you can ignore, the better. Less things to copy to docker build context.
   Less things would invalidate docker cache.
   Especially hidden files are not hidden from docker, like editors' temporary files.
   I hide `.git` directory. If you want to burn git-hash look at known issues section.

4. Add [`Dockerfile`](https://github.com/phadej/docker-haskell-example/blob/master/Dockerfile)
   and [`docker.cabal.config`](https://github.com/phadej/docker-haskell-example/blob/master/docker.cabal.config).
   `docker.cabal.config` is used in `Dockerfile`.
   In most cases you don't need to edit `Dockerfile`. You need, if you need some additional system dependencies.
   The next step will tell, if you need something.

5. Build an image with

   ```bash
   docker build --build-arg EXECUTABLE=docker-haskell-example --tag docker-haskell-example:latest .
   ```

   If it fails, due missing library, see next section. You'll need to edit
   `Dockerfile`, and iterate until you get a successful build.

6. After successful build, you can run the container locally

   ```bash
   docker run -ti --publish 8000:8000 docker-haskell-example:latest
   ```

   This step is important, to test that all runtime dependencies are there.

7. And try it from another terminal

    ```bash
    curl -D - localhost:8000
    ```

    It should respond something like:

    ```plain
    HTTP/1.1 200 OK
    Transfer-Encoding: chunked
    Date: Thu, 04 Jul 2019 16:15:37 GMT
    Server: Warp/3.2.27
    Content-Type: application/json;charset=utf-8

    ["hello","world"]
    ```

What happens in the Dockerfile
------------------------------

The `Dockerfile` is written with a monorepo setup in mind. In other words
setup, where you could build different docker images from a single repository.
That explains the `--build-arg EXECUTABLE=` in a `docker build` command.
It's also has some comments explaining *why* particular steps are done.

There are two stages in the `Dockerfile`, *builder* and *deployment*.

<h3>Builder stage</h3>

In builder stage we install all **build dependencies**,
separating them into different `RUN`s, so we could avoid
cache invalidation as much as possible.
A general rule: *Often changing things have to installed latter*.

We install few dependencies from Ubuntu's package repositories.
That list is something you'll need to edit once in a while.
The assumption is that all non-Haskell stuff comes from there
(or some PPA). There's also a corresponding list in *deployment* stage, there
we install only non-dev versions.

```dockerfile
# More dependencies, all the -dev libraries
# - some basic collection of often needed libs
# - also some dev tools
RUN apt-get -yq --no-install-suggests --no-install-recommends install \
    build-essential \
    ca-certificates \
    curl \
    git \
    libgmp-dev \
    liblapack-dev \
    liblzma-dev \
    libpq-dev \
    libyaml-dev \
    netbase \
    openssh-client \
    pkg-config \
    zlib1g-dev
```

At some point we reach a point, where we add `*.cabal` file.  This is something
you might need to edit as well, if you have multiple cabal files in different
directories.

```dockerfile
# Add a .cabal file to build environment
# - it's enough to build dependencies
COPY *.cabal cabal.project /build/
```

We only add these, so we can build dependencies.



```dockerfile
# Build package dependencies first
# - beware of https://github.com/haskell/cabal/issues/6106
RUN cabal v2-build -v1 --dependencies-only all
```

and their cache
won't be violated by changes in the actual implementation of the webapp.
This is common idiom in `Dockerfile`s.
[Issue 6106](https://github.com/haskell/cabal/issues/6106) might
be triggered if you vendor some dependencies. In that case
change the build command to

```dockerfile
RUN cabal v2-build -v1 --dependencies-only some-dependencies
```

listing as many dependencies (e.g. `servant`, `warp`) as possible.

After dependencies are built, the rest of the source files are added
and the executables are built, stripped, and moved to known location
out of `dist-newstyle` guts.

<h3>Deployment image</h3>

The deployment image is slick. We pay attention and don't install
development dependencies anymore. In other words we install only **runtime dependencies**.
 E.g. we install `libgmp10`, not `libgmp-dev`.
I also tend to install `curl` and some other cli tool to help debugging.
In deployment environments where you can shell into the running containers, it
helps if there's something you can do. That feature is useful to debug network
problems for example.

The resulting image is not the smallest possible,
but it's not huge either:

```
REPOSITORY               TAG      SIZE
docker-haskell-example   latest   137MB
```

Cold build is slow.
Rebuilds are reasonably fast,
if you don't touch `.cabal` or `cabal.project` files.

Known issues
------------

- If you have `data-files`, situation is tricky:
  Consider using
  [`file-embed-lzma`](https://hackage.haskell.org/package/file-embed-lzma)
  or [`file-embed`](https://hackage.haskell.org/package/file-embed)
  packages. I.e. avoid `data-files`.

- Cabal issue [#6106](https://github.com/haskell/cabal/issues/6106)
  may require you to edit `--dependencies-only` build step, as explained above.

- Git Hash into built executable. My approach is to ignore whole `.git`
  directory, as it might grow quite large.
  Maybe unignoring (with `!`) of `.git/HEAD` and `.git/refs` (which are relatively small)
  will make `gitrev` and alike work. Please tell me if you try!

- Caching of Haskell dependencies is very rudimentary.
  It could be improved largely, if `/cabal/store` could be copied out
  after the build, and in before the build. I don't really know how to that
  in Docker. Any tips are welcome.
  For example with `docker run` one could use volumes, but not with `docker build`.

Finally
-------

Look at [the example repository](https://github.com/phadej/docker-haskell-example).
I hope this is useful for you.
