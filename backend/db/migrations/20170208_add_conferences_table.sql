CREATE TABLE conferences(
  id SERIAL PRIMARY KEY NOT NULL,
  name VARCHAR(100)  NOT NULL,
  url  VARCHAR(250) NOT NULL,
  start_date DATE NOT NULL,
  end_date DATE NOT NULL,
  cfp_start_date DATE,
  cfp_end_date DATE,
  approved BOOLEAN NOT NULL DEFAULT FALSE
);
