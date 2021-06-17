ALTER TABLE "release" ADD COLUMN "dist_name" character varying(32) NOT NULL DEFAULT 'quicklisp', ADD COLUMN "dist_version" character(10);

UPDATE "release" SET "dist_version" = SUBSTR("systems_metadata_url", 57, 10) WHERE "dist_version" IS NULL;

ALTER TABLE "release" ALTER COLUMN "dist_name" DROP DEFAULT, ALTER COLUMN "dist_version" SET NOT NULL;

CREATE TABLE "dist_release" (
    "dist_id" BIGINT NOT NULL,
    "release_id" BIGINT NOT NULL,
    PRIMARY KEY ("dist_id", "release_id")
);
INSERT INTO "dist_release" ("dist_id", "release_id")
SELECT "release"."dist_id", "release_new"."id"
FROM "release"
LEFT JOIN (
  SELECT DISTINCT ON ("dist_name", "dist_version", "name") * FROM "release"
) "release_new"
ON "release"."dist_name" = "release_new"."dist_name"
  AND "release"."dist_version" = "release_new"."dist_version"
  AND "release"."name" = "release_new"."name";

DELETE FROM "release"
WHERE "id" NOT IN (SELECT "release_id" FROM "dist_release");
DELETE FROM "readme_file"
WHERE "release_id" NOT IN (SELECT "id" FROM "release");
DELETE FROM "system"
WHERE "release_id" NOT IN (SELECT "id" FROM "release");
DELETE FROM "system_dependency"
WHERE "system_id" NOT IN (SELECT "id" FROM "system");

DROP INDEX "unique_release_dist_id_name";
ALTER TABLE "release" DROP COLUMN "dist_id";
CREATE UNIQUE INDEX "unique_release_dist_name_dist_version_name" ON "release" ("dist_name", "dist_version", "name");
