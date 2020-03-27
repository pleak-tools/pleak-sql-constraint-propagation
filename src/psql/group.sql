SELECT constr.y AS grp, COUNT(*) AS cnt
FROM constr
GROUP BY constr.y;
