#!/bin/sh

rsync --verbose --checksum --recursive --times --prune-empty-dirs --delete --force --rsh=ssh --compress ./_site/ blog.ashabalin.me:/srv/http/blog/
