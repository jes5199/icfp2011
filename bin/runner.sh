if [ -z "$1" ] ; then
  echo "try something like:"
  echo "./runner.sh ./ltg.linux32 alt < input > output"
  exit
fi
setsid bash -c "( cat ; sleep 1; kill -9 0 ) | ( $* 2>&1 )"
