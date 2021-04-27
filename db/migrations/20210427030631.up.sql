ALTER TABLE "readme_file" DROP COLUMN "readme_id";
ALTER TABLE "readme_file" ADD COLUMN "release_id" character varying(36) NOT NULL;
DROP TABLE "readme";
