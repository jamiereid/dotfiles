#!/bin/sh

exec xautolock -detectsleep \
  -time 3 -locker "/home/jam/.dots/bin/lock.sh" \
  -notify 30 \
  -notifier "notify-send -u critical -t 10000 -- 'LOCKING sceen in 30 seconds'"
