create user news_reader with password 'news_reader';
create database news_reader with owner = news_reader encoding = 'utf8';

\connect news_reader

\i create_tables.sql
\i insert_data.sql
\i create_sequences.sql
\i create_constraints.sql
\i create_index.sql

grant select,update,insert,delete on all tables in schema public to news_reader;
grant all on all sequences in schema public to news_reader;
