FROM ubuntu:20.04
MAINTAINER Joel Martin <github@martintribe.org>

##########################################################
# General requirements for testing or common across many
# implementations
##########################################################

RUN apt-get -y update

# Required for running tests
RUN apt-get -y install make python

# Some typical implementation and test requirements
RUN apt-get -y install wget libreadline-dev libedit-dev

RUN mkdir -p /mal
WORKDIR /mal

##########################################################
# Specific implementation requirements
##########################################################

# janet
ARG JANET_VERSION=v1.23.0
RUN cd /usr/lib/x86_64-linux-gnu/ \
    && wget https://github.com/janet-lang/janet/releases/download/$JANET_VERSION/janet-$JANET_VERSION-linux.tar.gz \
    && tar xvzf janet-$JANET_VERSION-linux.tar.gz \
    && ln -sf /usr/lib/x86_64-linux-gnu/janet-$JANET_VERSION-linux/janet /usr/bin/janet \
    && rm janet-$JANET_VERSION-linux.tar.gz
