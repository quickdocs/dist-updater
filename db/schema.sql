CREATE TABLE "dist" (
    "id" VARCHAR(36) NOT NULL PRIMARY KEY,
    "name" VARCHAR(32) NOT NULL,
    "version" CHAR(10) NOT NULL,
    "created_at" TIMESTAMPTZ,
    "updated_at" TIMESTAMPTZ
);
CREATE UNIQUE INDEX "unique_dist_name_version" ON "dist" ("name", "version");

CREATE TABLE "release" (
    "id" VARCHAR(36) NOT NULL PRIMARY KEY,
    "dist_id" VARCHAR(36) NOT NULL,
    "project_name" VARCHAR(64) NOT NULL,
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
CREATE UNIQUE INDEX "unique_release_dist_id_project_name" ON "release" ("dist_id", "project_name");

CREATE TABLE "system_metadata" (
    "id" VARCHAR(36) NOT NULL PRIMARY KEY,
    "name" TEXT,
    "long_name" TEXT,
    "version" TEXT,
    "description" TEXT,
    "long_description" TEXT,
    "author" TEXT[] NOT NULL,
    "maintainer" TEXT[] NOT NULL,
    "mailto" TEXT,
    "license" TEXT,
    "homepage" TEXT,
    "bug_tracker" TEXT,
    "source_control" TEXT[],
    "created_at" TIMESTAMPTZ,
    "updated_at" TIMESTAMPTZ
);

CREATE TABLE "system" (
    "id" VARCHAR(36) NOT NULL PRIMARY KEY,
    "release_id" VARCHAR(36) NOT NULL,
    "name" TEXT NOT NULL,
    "system_file_name" TEXT NOT NULL,
    "required_systems" TEXT[],
    "metadata_id" VARCHAR(36),
    "created_at" TIMESTAMPTZ,
    "updated_at" TIMESTAMPTZ
);

CREATE TABLE "metadata_weakly_depends_on" (
    "id" VARCHAR(36) NOT NULL PRIMARY KEY,
    "system_metadata_id" VARCHAR(36) NOT NULL,
    "name" TEXT NOT NULL,
    "version" TEXT,
    "feature" TEXT,
    "created_at" TIMESTAMPTZ,
    "updated_at" TIMESTAMPTZ
);

CREATE TABLE "metadata_depends_on" (
    "id" VARCHAR(36) NOT NULL PRIMARY KEY,
    "system_metadata_id" VARCHAR(36) NOT NULL,
    "name" TEXT NOT NULL,
    "version" TEXT,
    "feature" TEXT,
    "created_at" TIMESTAMPTZ,
    "updated_at" TIMESTAMPTZ
);

CREATE TABLE "metadata_defsystem_depends_on" (
    "id" VARCHAR(36) NOT NULL PRIMARY KEY,
    "system_metadata_id" VARCHAR(36) NOT NULL,
    "name" TEXT NOT NULL,
    "version" TEXT,
    "feature" TEXT,
    "created_at" TIMESTAMPTZ,
    "updated_at" TIMESTAMPTZ
);

CREATE TABLE "readme_file" (
    "id" VARCHAR(36) NOT NULL PRIMARY KEY,
    "release_id" VARCHAR(36) NOT NULL,
    "filename" TEXT NOT NULL,
    "content" TEXT NOT NULL,
    "created_at" TIMESTAMPTZ,
    "updated_at" TIMESTAMPTZ
);

CREATE TABLE IF NOT EXISTS "schema_migrations" (
    "version" VARCHAR(255) PRIMARY KEY,
    "applied_at" TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);
