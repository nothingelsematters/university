#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail

function info() {
  echo -e "\e[1;34m> $*\e[0m" >&2
}

function trace() {
  info $*
  "$@"
}

function append() {
  TEXT=$1
  FILE_NAME=$2

  info "echo $TEXT >>$FILE_NAME"
  echo $TEXT >>$FILE_NAME
}

function automatic() {
  # Release the current lease and stop the running DHCP client
  trace dhclient -r $INTERFACE
  trace dhclient $INTERFACE
}

function static() {
  RESOLV_CONF=/etc/resolv.conf
  IP=172.16.10.50
  MASK=255.255.0.0
  GATEWAY=172.16.0.1
  DNS=172.16.255.254

  # trace ifconfig $INTERFACE $IP
  # trace ifconfig $INTERFACE netmask $MASK
  # trace route add default gw $GATEWAY $INTERFACE

  trace ip address flush dev $INTERFACE
  trace ip address add $IP/$MASK dev $INTERFACE
  trace ip route add default via $GATEWAY dev $INTERFACE
  trace sed -i '/nameserver/d' $RESOLV_CONF
  append "nameserver $DNS" $RESOLV_CONF
}

# Checking access rights
if [ "$EUID" -ne 0 ]; then
  echo "Please run as root"
  exit 1
fi

INTERFACE=wlp3s0

if [ -z "$(echo $* | grep -E -- "-s|--static")" ]; then automatic; else static; fi
