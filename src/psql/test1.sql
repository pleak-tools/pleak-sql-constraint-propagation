SELECT constr.x * constr.x - constr.x + 1, constr.x, constr.y, constr.z
    FROM constr
    WHERE constr.x >= 0.5;
