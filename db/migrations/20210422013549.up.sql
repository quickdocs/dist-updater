CREATE TABLE "dist" (
    "id" VARCHAR(36) NOT NULL PRIMARY KEY,
    "name" VARCHAR(32) NOT NULL,
    "version" CHAR(10) NOT NULL,
    "created_at" TIMESTAMPTZ,
    "updated_at" TIMESTAMPTZ
);
CREATE UNIQUE INDEX "unique_dist_name_version" ON "dist" ("name", "version");
ALTER TABLE "release" ADD COLUMN "dist_id" character varying(36) NOT NULL;
ALTER TABLE "release" ALTER COLUMN "project_name" TYPE character varying(64);
CREATE UNIQUE INDEX "unique_release_dist_id_project_name" ON "release" ("dist_id", "project_name");
