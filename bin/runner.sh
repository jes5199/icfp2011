if [ -z "$1" ] ; then
  echo "try something like:"
  echo "./runner.sh ./ltg.linux32 alt < input > output"
  exit
fi
( cat ; kill -9 0 ) | ( "$@" 2>&1 )
