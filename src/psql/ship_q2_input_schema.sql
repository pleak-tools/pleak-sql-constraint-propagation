CREATE TABLE ship (  ship_id INT8 primary key,  name TEXT,  cargo INT8,  latitude INT8,  longitude INT8,  length INT8,  draft INT8,  max_speed INT8);
CREATE TABLE port (  port_id FLOAT8 primary key,  name TEXT,  latitude INT8,  longitude INT8,  offloadcapacity INT8, harbordepth INT8, offloadtime INT8,  available Bool);
CREATE TABLE slot (  port_id INT8, berth_id INT8, slot_id INT8, ship_id INT8, slotstart INT8, slotend INT8);
