insert into provider (id, name, url) values (1, 'BBC', 'http://www.bbc.com/');
insert into feed (id, provider_id, feed_type_id, url)
values (1, 1, 1, 'http://feeds.bbci.co.uk/news/rss.xml'),
       (2, 1, 1, 'http://feeds.bbci.co.uk/news/technology/rss.xml'),
       (3, 1, 1, 'http://feeds.bbci.co.uk/news/world/rss.xml');
