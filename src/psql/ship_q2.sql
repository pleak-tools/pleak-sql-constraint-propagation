SELECT
    port.port_id AS port_id,
    ship.ship_id AS ship_id
FROM port, ship, slot
WHERE
    port.available
    AND port.harbordepth >= ship.draft
    AND port.offloadcapacity >= ship.cargo
    AND slot.port_id = port.port_id
    AND slot.slotstart + port.offloadtime <= slot.slotend
;
