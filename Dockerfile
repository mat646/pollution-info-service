FROM heroku/heroku:16

ENV LANG C.UTF-8

# Install required packages.
RUN apt-get update
RUN apt-get upgrade -y --assume-yes
# Install packages for stack and ghc.
RUN apt-get install -y --assume-yes xz-utils gcc libgmp-dev zlib1g-dev
# Install packages needed for libraries used by our app.
RUN apt-get install -y --assume-yes libpq-dev
# Install convenience utilities, like tree, ping, and vim.
RUN apt-get install -y --assume-yes tree iputils-ping vim-nox

# Remove apt caches to reduce the size of our container.
RUN rm -rf /var/lib/apt/lists/*



# Install stack to /opt/stack/bin.
#RUN mkdir -p /opt/stack/bin
#RUN curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C /opt/stack/bin '*/stack'
RUN apt-get update

# Install elm
#RUN apt-get update
RUN curl -sL https://deb.nodesource.com/setup_8.x -o nodesource_setup.sh
RUN bash nodesource_setup.sh
RUN apt-get install nodejs
RUN apt-get install -y build-essential libssl-dev
RUN apt-get update
RUN nodejs --version


#RUN apt-get install --reinstall nodejs npm nodejs-legacy

#RUN apt-get install npm
#RUN apt-get update
#RUN sudo chown -R $(whoami) /usr/local/lib/node_modules
#RUN npm install -g -y npm@latest
#RUN apt-get install nodejs-legacy

RUN apt-get update && \
      apt-get -y install sudo

RUN sudo npm install -g -y elm --unsafe-perm=true
RUN apt-get update

RUN curl -sSL https://get.haskellstack.org/ | sh
RUN apt-get update

RUN stack --version
RUN elm --version


# Create /opt/servant-on-heroku/bin and /opt/servant-on-heroku/src.  Set
# /opt/servant-on-heroku/src as the working directory.
RUN mkdir -p /opt/pollution-info-service/src
RUN mkdir -p /opt/pollution-info-service/bin
WORKDIR /opt/pollution-info-service/src

# Set the PATH for the root user so they can use stack.
ENV PATH "$PATH:/opt/stack/bin:/opt/pollution-info-service/bin"

COPY ./stack.yaml /opt/pollution-info-service/src/stack.yaml
COPY ./package.yaml /opt/pollution-info-service/src/package.yaml
COPY ./pollution-info-service.cabal /opt/pollution-info-service/src/pollution-info-service.cabal
COPY ./Makefile /opt/pollution-info-service/src/Makefile
#
#RUN mkdir -p /opt/servant-on-heroku/src/assets
#RUN mkdir -p /opt/servant-on-heroku/src/client
#RUN mkdir -p /opt/servant-on-heroku/src/server
#COPY ./server /opt/servant-on-heroku/src/Makefile
#COPY ./client /opt/servant-on-heroku/src/Makefile
#COPY ./server /opt/servant-on-heroku/src/Makefile


# Install GHC using stack, based on your app's stack.yaml file.

COPY . /opt/pollution-info-service/src

RUN stack setup

# Install all dependencies in app's .cabal file.
#COPY ./servant-on-heroku.cabal /opt/servant-on-heroku/src/servant-on-heroku.cabal
RUN stack test --only-dependencies

# Build application.

WORKDIR /opt/pollution-info-service/src/client
RUN elm package install -y
WORKDIR /opt/pollution-info-service/src


RUN stack build

WORKDIR /opt/pollution-info-service/src/client
RUN make
WORKDIR /opt/pollution-info-service/src

# Install application binaries to /opt/servant-on-heroku/bin.
#RUN stack --local-bin-path /opt/pollution-info-service/bin install


# Remove source code.
#RUN rm -rf /opt/servant-on-heroku/src

# Add the apiuser and setup their PATH.
RUN useradd -ms /bin/bash apiuser
RUN chown -R apiuser:apiuser /opt/pollution-info-service
USER apiuser
ENV PATH "$PATH:/opt/stack/bin:/opt/pollution-info-service/src/.stack-work/install/x86_64-linux/lts-8.21/8.0.2/bin"

# Set the working directory as /opt/servant-on-heroku/.
WORKDIR /opt/pollution-info-service/src

#CMD /opt/pollution-info-service/bin/server
CMD /opt/pollution-info-service/src/.stack-work/install/x86_64-linux/lts-8.21/8.0.2/bin/server