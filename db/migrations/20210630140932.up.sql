ALTER TABLE "system" ADD COLUMN "is_primary" boolean;

UPDATE "system"
SET "is_primary" = true
FROM "release"
WHERE "release"."id" = "system"."release_id"
  AND "release"."name" = "system"."name";

UPDATE "system"
SET "is_primary" = true
FROM "release"
WHERE "release"."id" = "system"."release_id"
  AND "system"."is_primary" IS NULL
  AND NOT EXISTS (SELECT "id" FROM "system" WHERE "system"."release_id" = "release"."id" AND "is_primary" = true)
  AND regexp_replace("release"."name", '^cl-', '') = regexp_replace("system"."name", '^cl-', '');

UPDATE "system"
SET "is_primary" = true
FROM "release"
WHERE "release"."id" = "system"."release_id"
  AND "system"."is_primary" IS NULL
  AND NOT EXISTS (SELECT "id" FROM "system" WHERE "system"."release_id" = "release"."id" AND "is_primary" = true)
  AND (SELECT COUNT("id") FROM "system" WHERE "system"."release_id" = "release"."id") = 1;

UPDATE "system"
SET "is_primary" = false
WHERE "is_primary" IS NULL;

ALTER TABLE "system" ALTER COLUMN "is_primary" SET NOT NULL;
CREATE INDEX "key_system_release_id_is_primary" ON "system" ("release_id", "is_primary");
