CREATE TABLE "project_topic" (
    "id" BIGSERIAL NOT NULL PRIMARY KEY,
    "project_name" VARCHAR(64) NOT NULL,
    "topic" VARCHAR(64) NOT NULL,
    "date" DATE NOT NULL,
    "created_at" TIMESTAMPTZ,
    "updated_at" TIMESTAMPTZ
);
CREATE UNIQUE INDEX "unique_project_topic_project_name_date" ON "project_topic" ("project_name", "date");
CREATE UNIQUE INDEX "unique_project_topic_topic_project_name" ON "project_topic" ("topic", "project_name");
