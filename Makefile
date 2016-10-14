.PHONY: sqlite3 list help test_data run stop
.DEFAULT_GOAL := all

SQLITE3_DB ?= feed.db
SQLITE3 ?= sqlite3

DSN_FILE=lib/news-reader/dsn.dat

PORT ?= 8080
SHUTDOWN_PORT ?= 8081
SAGITTARIUS ?= sagittarius

# for tests
ADD_PROVIDER=bin/add-provider
ADD_FEED=bin/add-feed
PROCESS_FEED=bin/process-feed
BBC=BBC
BBC_FEED='http://newsrss.bbc.co.uk/rss/newsplayer_uk_edition/front_page/rss.xml?edition=uk'

all: list

list: help
	@$(MAKE) -pRrq -f $(lastword $(MAKEFILE_LIST)) : 2>/dev/null \
	| awk -v RS= -F: '/^# File/,/^# Finished Make data base/ {if ($$1 !~ "^[#.]") {print $$1}}' \
	| sort \
	| egrep -v -e '^[^[:alnum:]]' -e '^$@$$' \
	| xargs

help:
	@echo List of targets:

sqlite3:
	rm -f $(SQLITE3_DB)
	$(SQLITE3) $(SQLITE3_DB) < scripts/create_tables.sql
	$(SQLITE3) $(SQLITE3_DB) < scripts/insert_data.sql
	@echo ';; -*- mode:scheme -*-' > $(DSN_FILE)
	@echo '"dbi:sqlite3:database=$(shell pwd)/$(SQLITE3_DB)"' >> $(DSN_FILE)

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
