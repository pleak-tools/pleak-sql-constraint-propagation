SELECT
    cat.color AS catcolor,
    cat.gender AS catgender,
    COUNT(*) AS cnt
FROM
    cat
WHERE
    cat.available
GROUP BY cat.color, cat.gender
;
