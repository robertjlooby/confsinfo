-- Audiences
CREATE TABLE audiences(
  id SERIAL PRIMARY KEY NOT NULL,
  name VARCHAR(100)  NOT NULL UNIQUE,
  approved BOOLEAN NOT NULL DEFAULT FALSE
);

CREATE TABLE conferences_audiences(
  audiences_id INTEGER REFERENCES audiences,
  conferences_id INTEGER REFERENCES conferences,
  UNIQUE (audiences_id, conferences_id)
);

-- Languages
CREATE TABLE languages(
  id SERIAL PRIMARY KEY NOT NULL,
  name VARCHAR(100)  NOT NULL UNIQUE,
  approved BOOLEAN NOT NULL DEFAULT FALSE
);

CREATE TABLE conferences_languages(
  languages_id INTEGER REFERENCES languages,
  conferences_id INTEGER REFERENCES conferences,
  UNIQUE (languages_id, conferences_id)
);

-- Locations
CREATE TABLE locations(
  id SERIAL PRIMARY KEY NOT NULL,
  name VARCHAR(100)  NOT NULL UNIQUE,
  approved BOOLEAN NOT NULL DEFAULT FALSE
);

CREATE TABLE conferences_locations(
  locations_id INTEGER REFERENCES locations,
  conferences_id INTEGER REFERENCES conferences,
  UNIQUE (locations_id, conferences_id)
);

-- Topics
CREATE TABLE topics(
  id SERIAL PRIMARY KEY NOT NULL,
  name VARCHAR(100)  NOT NULL UNIQUE,
  approved BOOLEAN NOT NULL DEFAULT FALSE
);

CREATE TABLE conferences_topics(
  topics_id INTEGER REFERENCES topics,
  conferences_id INTEGER REFERENCES conferences,
  UNIQUE (topics_id, conferences_id)
);
