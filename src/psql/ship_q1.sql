SELECT
    port.port_id AS port_id,
    ship.ship_id AS ship_id,
    (ABS(ship.latitude - port.latitude) + ABS(ship.longitude - port.longitude)) ^ 0.5 / ship.max_speed AS arrival
FROM port, ship
;
