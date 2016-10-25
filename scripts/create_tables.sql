-- create schema feed;

create table provider (
       id integer primary key,
       name varchar(255) not null unique,
       url varchar(1024) not null
);

create table feed (
       id integer primary key,
       provider_id integer not null,
       feed_type_id integer not null,
       language_id integer not null,
       title varchar(255) not null,
       url varchar(1024) not null unique
);

create table feed_summary (
       id bigint primary key,
       feed_id integer not null,
       guid varchar(1024) not null unique,
       title varchar(2048) not null,
       summary text,
       pubDate timestamp not null
);

create table feed_type (
       id integer primary key,
       name varchar(10) not null unique,
       plugin varchar(255) not null unique
);

create table languages (
       id integer primary key,
       name  varchar(100) not null unique,
       code3 char(3) not null unique,
       code2 char(2)
);
