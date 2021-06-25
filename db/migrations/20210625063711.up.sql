CREATE TABLE "project_download_stats" (
    "id" BIGSERIAL NOT NULL PRIMARY KEY,
    "date" DATE NOT NULL,
    "project_name" VARCHAR(64) NOT NULL,
    "download_count" INTEGER NOT NULL,
    "created_at" TIMESTAMPTZ,
    "updated_at" TIMESTAMPTZ
);
CREATE INDEX "key_project_download_stats_date" ON "project_download_stats" ("date");
CREATE INDEX "key_project_download_stats_project_name_date" ON "project_download_stats" ("project_name", "date");
