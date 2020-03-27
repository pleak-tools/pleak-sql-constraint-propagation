SELECT
    COUNT(*)
FROM
    cat
WHERE
    cat.available
    AND cat.color = 'black'
;
