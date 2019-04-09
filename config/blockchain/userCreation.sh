#!/bin/bash

echo 'Creating application user and db'

mongo $MONGO_INITDB_DATABASE \
        --host localhost \
        --port 27017 \
        -u $MONGO_INITDB_ROOT_USERNAME \
        -p $MONGO_INITDB_ROOT_PASSWORD \
        --authenticationDatabase admin \
        --eval "db.createUser({user: '$APP_MONGO_USER', pwd: '$APP_MONGO_PASS', roles:[{role:'dbOwner', db: '$MONGO_INITDB_DATABASE'}]});"