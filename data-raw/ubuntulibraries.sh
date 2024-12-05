#!/bin/bash
echo "This script will install needed libraries"

sudo apt-get update -y
sudo apt-get dist-upgrade -y

sudo apt-get -y --no-install-recommends install \
    cmake


if [ -d $HOME/bin ]; then
PATH=$PATH:$HOME/bin
fi
