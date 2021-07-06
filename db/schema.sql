CREATE TABLE "project_topic" (
    "id" BIGSERIAL NOT NULL PRIMARY KEY,
    "project_name" VARCHAR(64) NOT NULL,
    "topic" VARCHAR(64) NOT NULL,
    "date" DATE NOT NULL,
    "created_at" TIMESTAMPTZ,
    "updated_at" TIMESTAMPTZ
);
CREATE UNIQUE INDEX "unique_project_topic_topic_project_name" ON "project_topic" ("topic", "project_name");
CREATE INDEX "key_project_topic_project_name_date" ON "project_topic" ("project_name", "date");

CREATE TABLE "project_download_stats" (
    "id" BIGSERIAL NOT NULL PRIMARY KEY,
    "date" DATE NOT NULL,
    "project_name" VARCHAR(64) NOT NULL,
    "download_count" INTEGER NOT NULL,
    "created_at" TIMESTAMPTZ,
    "updated_at" TIMESTAMPTZ
);
CREATE UNIQUE INDEX "unique_project_download_stats_project_name_date" ON "project_download_stats" ("project_name", "date");
CREATE INDEX "key_project_download_stats_date_download_count" ON "project_download_stats" ("date", "download_count");

CREATE TABLE "release" (
    "id" BIGSERIAL NOT NULL PRIMARY KEY,
    "dist_name" VARCHAR(32) NOT NULL,
    "dist_version" CHAR(10) NOT NULL,
    "name" VARCHAR(64) NOT NULL,
    "archive_url" TEXT NOT NULL,
    "archive_size" INTEGER NOT NULL,
    "archive_content_sha1" VARCHAR(40) NOT NULL,
    "prefix" TEXT NOT NULL,
    "systems_metadata_url" TEXT NOT NULL,
    "readme_url" TEXT NOT NULL,
    "upstream_url" TEXT NOT NULL,
    "created_at" TIMESTAMPTZ,
    "updated_at" TIMESTAMPTZ
);
CREATE UNIQUE INDEX "unique_release_dist_name_dist_version_name" ON "release" ("dist_name", "dist_version", "name");

CREATE TABLE "system" (
    "id" BIGSERIAL NOT NULL PRIMARY KEY,
    "release_id" BIGINT NOT NULL,
    "is_primary" BOOLEAN NOT NULL,
    "name" VARCHAR(64) NOT NULL,
    "filename" VARCHAR(80),
    "long_name" TEXT,
    "version" TEXT,
    "description" TEXT,
    "long_description" TEXT,
    "authors" TEXT[] NOT NULL,
    "maintainers" TEXT[] NOT NULL,
    "mailto" TEXT,
    "license" TEXT,
    "homepage" TEXT,
    "bug_tracker" TEXT,
    "source_control_url" TEXT,
    "created_at" TIMESTAMPTZ,
    "updated_at" TIMESTAMPTZ
);
CREATE UNIQUE INDEX "unique_system_release_id_name" ON "system" ("release_id", "name");
CREATE INDEX "key_system_release_id_is_primary" ON "system" ("release_id", "is_primary");

CREATE TABLE "system_dependency" (
    "id" BIGSERIAL NOT NULL PRIMARY KEY,
    "system_id" BIGINT NOT NULL,
    "type" VARCHAR(12) NOT NULL,
    "name" VARCHAR(64) NOT NULL,
    "version" TEXT,
    "feature" TEXT,
    "created_at" TIMESTAMPTZ,
    "updated_at" TIMESTAMPTZ
);
CREATE UNIQUE INDEX "unique_system_dependency_system_id_type_name" ON "system_dependency" ("system_id", "type", "name");

CREATE TABLE "readme_file" (
    "id" BIGSERIAL NOT NULL PRIMARY KEY,
    "release_id" BIGINT NOT NULL,
    "filename" VARCHAR(64) NOT NULL,
    "content" TEXT NOT NULL,
    "created_at" TIMESTAMPTZ,
    "updated_at" TIMESTAMPTZ
);
CREATE UNIQUE INDEX "unique_readme_file_release_id_filename" ON "readme_file" ("release_id", "filename");

CREATE TABLE "dist" (
    "id" BIGSERIAL NOT NULL PRIMARY KEY,
    "name" VARCHAR(32) NOT NULL,
    "version" CHAR(10) NOT NULL,
    "system_index_url" VARCHAR(128) NOT NULL,
    "release_index_url" VARCHAR(128) NOT NULL,
    "archive_base_url" VARCHAR(128) NOT NULL,
    "distinfo_subscription_url" VARCHAR(128) NOT NULL,
    "canonical_distinfo_url" VARCHAR(128) NOT NULL,
    "provided_releases_count" INTEGER NOT NULL,
    "provided_releases_url" VARCHAR(128) NOT NULL,
    "extract_errors_url" VARCHAR(128) NOT NULL,
    "created_at" TIMESTAMPTZ,
    "updated_at" TIMESTAMPTZ
);
CREATE UNIQUE INDEX "unique_dist_name_version" ON "dist" ("name", "version");

CREATE TABLE "dist_release" (
    "dist_id" BIGINT NOT NULL,
    "release_id" BIGINT NOT NULL,
    PRIMARY KEY ("dist_id", "release_id")
);

CREATE TABLE IF NOT EXISTS "schema_migrations" (
    "version" VARCHAR(255) PRIMARY KEY,
    "applied_at" TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);
