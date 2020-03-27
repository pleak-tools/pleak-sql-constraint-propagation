SELECT
    cat.cat_id, cat.name, cat.gender, cat.color, cat.available
FROM
    cat
WHERE
    cat.available
    AND cat.color = 'black'
;
