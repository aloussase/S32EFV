create table expenses (
  id serial primary key,
  date date not null,
  amount decimal not null,
  tipo_raw text not null,
  tipo text not null,
  reference text not null
);
