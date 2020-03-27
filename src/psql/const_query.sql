SELECT
    /* arithmetic */
    constr.x,
    constr.x + 4,
    (-constr.x) * 2,
    constr.x - 2 * 3,
    1 / constr.x,
    -3*constr.x,

    /* functions */
    POW(constr.x, 2.0)/2.0,
    LOG(constr.x),
    ABS(constr.x - 2),
    LOG(ABS(constr.x - 2) + 1),
    POW(constr.x, 2),

    /* cases that require some sort of variable binding (same column used twice) */
    (constr.x - 2) * (constr.x - 2),   /* output should be (Range 0 1) */
    constr.x + -constr.x + 4,          /* output should be (Range 4 4) */
    constr.x * (3/constr.x),           /* output should be (Range 3 3) */
    POW(constr.x, 2) - 3*constr.x + 1, /* output should be (Range -1.25 1) */

    /* multiple variables */
    constr.x + constr.y,
    1/(1000*(POW(constr.x, 2)+POW(constr.y, 2))),
    
    /* exact */
    constr.x + constr.z,
    POW(constr.x, 0.5)
FROM constr
WHERE constr.x <= 3.0 AND
      constr.y >= 0.2 AND
      constr.z >= 0;
