DROP TABLE "metadata_defsystem_depends_on";
DROP TABLE "metadata_depends_on";
DROP TABLE "metadata_weakly_depends_on";
DROP TABLE "system_metadata";
DROP TABLE "readme_file";
DROP TABLE "system";
DROP TABLE "release";
DROP TABLE "dist";

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

CREATE TABLE "release" (
    "id" BIGSERIAL NOT NULL PRIMARY KEY,
    "dist_id" BIGINT NOT NULL,
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
CREATE UNIQUE INDEX "unique_release_dist_id_name" ON "release" ("dist_id", "name");

CREATE TABLE "system" (
    "id" BIGSERIAL NOT NULL PRIMARY KEY,
    "release_id" BIGINT NOT NULL,
    "name" VARCHAR(64) NOT NULL,
    "long_name" TEXT,
    "filename" VARCHAR(80) NOT NULL,
    "version" TEXT,
    "description" TEXT,
    "long_description" TEXT,
    "authors" TEXT[] NOT NULL,
    "maintainers" TEXT[] NOT NULL,
    "mailto" TEXT,
    "license" TEXT,
    "homepage" TEXT,
    "bug_tracker" TEXT,
    "source_control" TEXT[],
    "created_at" TIMESTAMPTZ,
    "updated_at" TIMESTAMPTZ
);
CREATE UNIQUE INDEX "unique_system_release_id_name" ON "system" ("release_id", "name");

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
