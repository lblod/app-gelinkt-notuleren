# Gelinkt Notuleren

Backend systems and editor built on top of the Besluit and Mandaat model and application profile as defined on:
* http://data.vlaanderen.be/ns/besluit
* http://data.vlaanderen.be/doc/applicatieprofiel/besluit-publicatie/
* http://data.vlaanderen.be/ns/mandaat
* http://data.vlaanderen.be/doc/applicatieprofiel/mandatendatabank/

## What's included?

This repository harvest three setups.  The base of these setups resides in the standard docker-compose.yml.

* *docker-compose.yml* This provides you with the backend components.  There is a frontend application included which you can publish using a separate proxy (we tend to put a letsencrypt proxy in front).
* *docker-compose.dev.yml* Provides changes for a good frontend development setup.
  - publishes the backend services on port 80 directly, so you can run `ember serve --proxy http://localhost` when developing the frontend apps natively.
  - publishes the database instance on port 8890 so you can easily see what content is stored in the base triplestore
  - provides a mock-login backend service so you don't need the ACM/IDM integration.
* *docker-compose.demo.yml* Provides a setup for demo purposes.  It is similar to the dev setup, but publishes the frontend application directly:
  - publishes the frontend editor on port 80 so you can visit the app at http://localhost/
  - publishes the database instance on port 8890 so you can easily see what content is stored in the base triplestore
  - provides a mock-login backend service so you don't need the ACM/IDM integration

## Running and maintaining

General information on running and maintaining an installation

### How to setup the stack

First choose what you intend to run based on the description above.  Two scenario's are included here: the demo setup and the dev setup.

#### Running the demo setup

First install docker, docker-compose, and git-lfs (see https://github.com/git-lfs/git-lfs/wiki/Installation)

Execute the following:

    # Clone this repository
    git clone https://github.com/lblod/app-gelinkt-notuleren.git

    # Move into the directory
    cd app-gelinkt-notuleren

    # Make sure git-lfs is enabled after installation
    git lfs install

    # Start the system
    docker-compose -f docker-compose.yml -f docker-compose.demo.yml up -d

Wait for everything to boot to ensure clean caches.  You may choose to monitor the migrations service in a separate terminal to and wait for the overview of all migrations to appear: `docker-compose logs -f --tail=100 migrations`.

Once the migrations have ran, visit the application at http://localhost/mock-login

#### Running the dev setup

First install docker, docker-compose, and git-lfs (see https://github.com/git-lfs/git-lfs/wiki/Installation)

Execute the following:

    # Make sure git-lfs is enabled after installation
    git lfs install

    # Clone this repository
    git clone https://github.com/lblod/app-gelinkt-notuleren.git

    # Move into the directory
    cd app-gelinkt-notuleren

    # Start the system
    docker-compose -f docker-compose.yml -f docker-compose.dev.yml up -d

Wait for everything to boot to ensure clean caches.  You may choose to monitor the migrations service in a separate terminal to and wait for the overview of all migrations to appear: `docker-compose logs -f --tail=100 migrations`.

Once the migrations have ran, you can start developing your application by connecting the ember frontend application to this backend.  See https://github.com/lblod/frontend-gelinkt-notuleren for more information on development with the ember application.

### Upgrading your setup

Once installed, you may desire to upgrade your current setup to follow development of the main stack.  The following example describes how to do this easily for both the demo setup, as well as for the dev setup.

#### Upgrading the demo setup

First we bring down the stack so we can upgrade things easily:

    # Move to the right directory
    cd place-where-you-clone-repository/app-gelinkt-notuleren

    # Bring the application down
    docker-compose -f docker-compose.yml -f docker-compose.demo.yml down

If you don't need the database of your current setup anymore, you may whish to remove its current contents.  Once we hit the first release, the migrations should take care of upgrading your application as needed, until then you may possibly hit a breaking change.

    # Remove all contents in the database folder
    rm -Rf data/db

    # Checkout the required files from the repository
    git checkout data/db

Next up is pulling in the changes from the upstream and launching the stack again.

    # Pull in the changes
    git pull origin master

    # Launch the stack
    docker-compose -f docker-compose.yml -f docker-compose.demo.yml up

As with the initial setup, we wait for everything to boot to ensure clean caches.  You may choose to monitor the migrations service in a separate terminal to and wait for the overview of all migrations to appear: `docker-compose logs -f --tail=100 migrations`.

Once the migrations have ran, visit the application at http://localhost/

#### Upgrading the dev setup

For the dev setup, we assume you'll pull more often and thus will most likely clear the database separately:

    # Bring the application down
    docker-compose -f docker-compose.yml -f docker-compose.dev.yml down
    # Pull in the changes
    git pull origin master
    # Launch the stack
    docker-compose -f docker-compose.yml -f docker-compose.demo.yml up

As with the initial setup, we wait for everything to boot to ensure clean caches.  You may choose to monitor the migrations service in a separate terminal to and wait for the overview of all migrations to appear: `docker-compose logs -f --tail=100 migrations`.

Once the migrations have ran, you can go on with your current setup.

### Cleaning the database

At some times you may want te clean the database and make sure it's in a pristine state.  For development this is the following (for demo, replace the docker-compose.dev.yml with docker-compose.demo.yml):

    # Bring down our current setup
    docker-compose -f docker-compose.yml -f docker-compose.dev.yml down
    # Keep only required database files
    rm -Rf data/db
    git checkout data/db
    # Bring the stack back up
    docker-compose -f docker-compose.yml -f docker-compose.dev.yml up

Make sure to wait for the migrations to run.

### Setting up external delta sync [EXPERIMENTAL]

This feature allows syncing data from external applications, to be immediatly reflected in the current application.
It is considered an external feature at this point and requires a manual setup.

#### mandatarissen-consumer
*DISCLAIMER: this is not 100% bulletproof*

0. Make sure only virtuoso and migrations service is running.
1. Download the data dump from the relevant source, most likely it will be the turtle-file from https://mandaten.lokaalbestuur.vlaanderen.be .
    - The timestamp of the turtle file (in filename) is important. (e.g. 20200918031500112 in mandaten-20200918031500112.ttl)
2. The turtle file should be added to the migrations folder and should be migrated to `<http://mu.semte.ch/graphs/temp-ingest-graph>`
    - Consult https://github.com/mu-semtech/mu-migrations-service for more info about how ttl files can be injected in a graph
3. Copy the migration templates from `config/consumer/mandatarissen/setup/template-migrations/*` and add them to the migrations
    - Don't forget a timestamp like other migrations.
    - They should run *AFTER* the turtle file from step 2.
4. `drc restart migrations`
    - Wait until they ran. (Might take a while).
5. Once finished, an extra docker file should be included.
    - Something like `docker-compose -f docker-compose.yml -f docker-compose.external-delta-sync.yml -f docker-compose.override.yml`
    - Or create a `.env` file with the following lines `COMPOSE_FILE=docker-compose.yml:docker-compose.external-delta-sync.yml:docker-compose.override.yml`;
6. You will need to update your `docker-compose.override.yml` file too, with following lines:
   ```
     mandatarissen-consumer:
       environment:
         SYNC_BASE_URL: 'https://dev.mandaten.lblod.info' # the endpoint you want to sync from
         START_FROM_DELTA_TIMESTAMP: '2020-09-18T03:15:00.112Z' # a timestamp from TTL converted to ISO
   ```
7. start the stack




## General application structure

This stack is built based on the mu.semte.ch architecture.  The starting point for this stack is [mu-project](https://github.com/mu-semtech/mu-project).

## API documentation

The vast amount of API space offered by this stack is covered by mu-cl-resources.  The OpenAPI documentation can be generated using [cl-resources-openapi-generator](https://github.com/mu-semtech/cl-resources-openapi-generator).  We also advise to checkout the [JSONAPI](https://jsonapi.org) documentation which covers the general way in which our APIs work and the [mu-cl-resources](https://github.com/mu-semtech/mu-cl-resources) documentation for specific extensions to this api.
