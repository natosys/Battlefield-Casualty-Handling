## Environment Data Summary

### 👥 Population Groups

The following population groups are defined in the simulation environment:

| Population | Count |
| ---------- | ----- |
| Combat     | 2500  |
| Support    | 1250  |

### 🚑 Transport Resources

These are the available transport platforms and their characteristics:

| Platform | Quantity | Capacity |
| -------- | -------- | -------- |
| PMVAMB   | 3        | 4        |
| HX240M   | 4        | 50       |

### 🏥 Medical Resources

The following table summarises the medical elements configured in `env_data.json`, including team types, personnel, and beds:

| Element  | Quantity | Beds                                  | Base                             | Surg                                    | Emerg                           | Icu                        | Evac      |
| -------- | -------- | ------------------------------------- | -------------------------------- | --------------------------------------- | ------------------------------- | -------------------------- | --------- |
| R1       | 6        | NA                                    | Medic (3), Nurse (1), Doctor (1) | NA                                      | NA                              | NA                         | NA        |
| R2B      | 2        | OT (1); Resus (2); ICU (2); Hold (5)  | NA                               | Anesthetist (1), Surgeon (2), Medic (1) | Facem (1), Nurse (3), Medic (1) | Nurse (2), Medic (2)       | Medic (2) |
| R2EHEAVY | 2        | OT (2); Resus (4); ICU (4); Hold (30) | NA                               | Anesthetist (1), Surgeon (2), Nurse (4) | Facem (1), Nurse (3), Medic (1) | Intensivist (1), Nurse (4) | Medic (2) |
