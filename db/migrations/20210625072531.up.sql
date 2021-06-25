DROP INDEX "key_project_download_stats_date";
DROP INDEX "key_project_download_stats_project_name_date";
CREATE INDEX "key_project_download_stats_date_download_count" ON "project_download_stats" ("date", "download_count");
CREATE UNIQUE INDEX "unique_project_download_stats_project_name_date" ON "project_download_stats" ("project_name", "date");
