CREATE TABLE release (
    id VARCHAR(36) NOT NULL PRIMARY KEY,
    project_name TEXT NOT NULL,
    archive_url TEXT NOT NULL,
    archive_size INTEGER NOT NULL,
    archive_content_sha1 VARCHAR(40) NOT NULL,
    prefix TEXT NOT NULL,
    systems_metadata_url TEXT NOT NULL,
    readme_url TEXT NOT NULL,
    upstream_url TEXT NOT NULL,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);
CREATE TABLE release_system (
    id VARCHAR(36) NOT NULL PRIMARY KEY,
    release_id VARCHAR(36) NOT NULL,
    name TEXT NOT NULL,
    system_file_name TEXT NOT NULL,
    required_systems TEXT[] NOT NULL,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);
CREATE TABLE readme (
    id VARCHAR(36) NOT NULL PRIMARY KEY,
    name TEXT NOT NULL,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);
CREATE TABLE readme_file (
    id VARCHAR(36) NOT NULL PRIMARY KEY,
    readme_id VARCHAR(36) NOT NULL,
    filename TEXT NOT NULL,
    content TEXT NOT NULL,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);
CREATE TABLE system_metadata (
    id VARCHAR(36) NOT NULL PRIMARY KEY,
    name TEXT,
    long_name TEXT,
    version TEXT,
    description TEXT,
    long_description TEXT,
    author TEXT[] NOT NULL,
    maintainer TEXT[] NOT NULL,
    mailto TEXT,
    license TEXT,
    homepage TEXT,
    bug_tracker TEXT,
    source_control TEXT[],
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);
CREATE TABLE abstract_metadata_depends_on (
    id VARCHAR(36) NOT NULL PRIMARY KEY,
    system_metadata_id VARCHAR(36) NOT NULL,
    name TEXT NOT NULL,
    version TEXT,
    feature TEXT,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);
CREATE TABLE metadata_defsystem_depends_on (
    id VARCHAR(36) NOT NULL PRIMARY KEY,
    system_metadata_id VARCHAR(36) NOT NULL,
    name TEXT NOT NULL,
    version TEXT,
    feature TEXT,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);
CREATE TABLE metadata_depends_on (
    id VARCHAR(36) NOT NULL PRIMARY KEY,
    system_metadata_id VARCHAR(36) NOT NULL,
    name TEXT NOT NULL,
    version TEXT,
    feature TEXT,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);
CREATE TABLE metadata_weakly_depends_on (
    id VARCHAR(36) NOT NULL PRIMARY KEY,
    system_metadata_id VARCHAR(36) NOT NULL,
    name TEXT NOT NULL,
    version TEXT,
    feature TEXT,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);
CREATE TABLE system (
    id VARCHAR(36) NOT NULL PRIMARY KEY,
    name TEXT NOT NULL,
    system_file_name TEXT NOT NULL,
    required_systems TEXT[],
    metadata_id VARCHAR(36),
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);
