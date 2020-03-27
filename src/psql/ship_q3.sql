SELECT
    reachable_ports.port_id AS port_id,
    MAX(reachable_ports.arrival) AS maxtime
FROM reachable_ports,
     feasible_ports
WHERE
    reachable_ports.port_id = feasible_ports.port_id
    AND reachable_ports.ship_id = feasible_ports.ship_id
GROUP BY reachable_ports.port_id
;
