alter table feed add constraint fk_feed_provider 
foreign key (provider_id) references provider(id);
alter table feed add constraint fk_feed_feed_type
foreign key (feed_type_id) references feed_type(id);

alter table feed_summary add constraint fk_feed_summary_feed
foreign key (feed_id) references feed(id);
