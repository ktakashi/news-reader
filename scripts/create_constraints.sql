alter table feed add constraint fk_feed_provider 
foreign key (provider_id) references provider(id);
alter table feed add constraint fk_feed_feed_type
foreign key (feed_type_id) references feed_type(id);
alter table feed add constraint fk_feed_languages
foreign key (language_id) references languages(id);
-- Some of titles are duplicated
-- FIXME this is sort of needed...
--alter table feed add constraint uk_feed_provider
--unique (provider_id, title);

alter table feed_summary add constraint fk_feed_summary_feed
foreign key (feed_id) references feed(id);
