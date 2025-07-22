#!/bin/bash
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
exec > $SCRIPT_DIR/automated-sync.log      
exec 2>&1
export PATH=$PATH:/usr/local/bin

echo "$(date) - Sync starting"

echo "$(date) - generating LDB migrations"
# generate LDB migrations
LDB="https://leidinggevenden.lokaalbestuur.vlaanderen.be"
FILENAME=`curl -s "$LDB/exports?filter%5Bformat%5D=text%2Fturtle&page%5Bsize%5D=1&sort=-created" -H 'Accept: application/vnd.api+json'  -H 'Pragma: no-cache' -H 'Cache-Control: no-cache' | jq -r '.data[0].attributes.filename'`
cd /data/app-gelinkt-notuleren
curl "$LDB/files/$FILENAME" -o $FILENAME
if [ $? -ne 0 ]; then    
    echo "download of LDB export failed, not generating migrations"
    exit -1
else
  /data/mu-cli/mu script project-scripts setup-data-sync-ldb $FILENAME
  rm $FILENAME
fi

# mdb sync no longer needed with ldes
#echo "$(date) - generating MDB migrations"

## generate MDB migrations
#MDB="https://mandaten.lokaalbestuur.vlaanderen.be"
#FILENAME=`curl -s "$MDB/exports?filter%5Bformat%5D=text%2Fturtle&page%5Bsize%5D=1&sort=-created" -H 'Accept: application/vnd.api+json'  -H 'Pragma: no-cache' -H 'Cache-Control: no-cache' | jq -r '.data[0].attributes.filename'`
#
#curl "$MDB/files/$FILENAME" -o $FILENAME
#if [ $? -ne 0 ]; then    
#    echo "download of MDB export failed, not generating migrations"
#    exit -1
#else
#  /data/mu-cli/mu script project-scripts setup-data-sync-mdb $FILENAME
#  rm $FILENAME
#fi

echo "$(date) - setting up maintenance frontend"

# set up maintenance frontend
sed -i 's\# image: lblod/frontend-maintenance:0.1.0\  image: lblod/frontend-maintenance:0.1.0\' docker-compose.override.yml
docker compose up -d editor

echo "$(date) - restarting migration service"
# run migrations
docker compose restart migrations
sleep 1200

echo "$(date) - starting checks for successful migrations"
# check if migrations successfull
docker compose logs --tail 50 migrations | grep 'ingest-exported-triples.sparql \[DONE\]'
while [ ! $? -eq 0 ]; do
    sleep 300;
    docker compose logs --tail 50 migrations | grep 'ingest-exported-triples.sparql \[DONE\]'
done

echo "$(date) - migrations successful, making virtuoso checkpoints"
docker compose exec -T virtuoso isql-v <<EOF
    exec('checkpoint');
    DB.DBA.vacuum();
    exec('checkpoint');
    exit;
EOF
echo "$(date) - restarting cache and resources"
docker compose restart cache resource sparql-cache

echo "$(date) - disabling maintenance frontend"
sed -i 's\  image: lblod/frontend-maintenance:0.1.0\# image: lblod/frontend-maintenance:0.1.0\' docker-compose.override.yml

echo "$(date) - restarting frontend"
docker compose up -d editor

echo "$(date) - sync completed"
