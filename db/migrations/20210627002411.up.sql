DROP INDEX "unique_project_topic_project_name_date";
CREATE INDEX "key_project_topic_project_name_date" ON "project_topic" ("project_name", "date");
