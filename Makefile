.PHONY: list help test_data run stop \
	sqlite3 create-sqlite3 sqlite3-dsn \
	postgres create-postgres  postgres-dsn clean-postgres
.DEFAULT_GOAL := all

SQLITE3_DB ?= feed.db
SQLITE3 ?= sqlite3

DSN_FILE=lib/news-reader/dsn.dat
LIB_CONSTANTS=lib/news-reader/constants.scm

PORT ?= 8080
SHUTDOWN_PORT ?= 8081
SAGITTARIUS ?= sagittarius

PSQL ?= psql
POSTGRES_SERVER ?= localhost

# for tests
PROCESS_FEED=bin/process-feed
BBC=BBC

all: list

list: help
	@$(MAKE) -pRrq -f $(lastword $(MAKEFILE_LIST)) : 2>/dev/null \
	| awk -v RS= -F: '/^# File/,/^# Finished Make data base/ {if ($$1 !~ "^[#.]") {print $$1}}' \
	| sort \
	| egrep -v -e '^[^[:alnum:]]' -e '^$@$$' \
	| xargs

help:
	@echo List of targets:

sqlite3: create-sqlite3 sqlite3-dsn

sqlite3-dsn: dsn-message
	@echo ';; -*- mode:scheme -*-' > $(DSN_FILE)
	@echo '"dbi:sqlite3:database=$(shell pwd)/$(SQLITE3_DB)"' >> $(DSN_FILE)
	touch $(LIB_CONSTANTS)

create-sqlite3:
	rm -f $(SQLITE3_DB)
	$(SQLITE3) $(SQLITE3_DB) < scripts/create_tables.sql
	$(SQLITE3) $(SQLITE3_DB) < scripts/insert_data.sql

testdata: sqlite3
	$(SQLITE3) $(SQLITE3_DB) < scripts/insert_bbc_feed.sql
	$(PROCESS_FEED) -d $(BBC)

run:
	@echo Starting news-reader on $(PORT)
	./jobs/process-feeds &
	$(SAGITTARIUS) run.scm -p$(PORT) -s$(SHUTDOWN_PORT) &

stop:
	@echo Stoppng news-reader
	$(SAGITTARIUS) run.scm -p$(PORT) -s$(SHUTDOWN_PORT) -c stop
	./jobs/process-feeds -c stop

postgres: create-postgres postgres-dsn

postgres-dsn:
	@echo ';; -*- mode:scheme -*-' > $(DSN_FILE)
	@echo '"dbi:postgres:host=$(POSTGRES_SERVER);database=news_reader"' >> $(DSN_FILE)
	touch $(LIB_CONSTANTS)


create-postgres:
	cd scripts; $(PSQL) -h $(POSTGRES_SERVER) -f create_postgresql_database.sql postgres postgres

clean-postgres:
	$(PSQL) -f scripts/drop_postgresql.sql postgres postgres

dsn-message:
	@echo 'Creating dsn.dat'