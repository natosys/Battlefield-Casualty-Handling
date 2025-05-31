# Battlefield Casualty Handling

## Introduction

This is a Discrete Event Simulation (DES) written in R that uses the simmer package. The code is designed to simulate the flow of battlefield casualties in Large Scale Combat Operations (LSCO) scenarios. The purpose of the simulation is to support decision making on deployed health system design with a focus on capacity planning.

## Design

The code simulates a deployed combat brigade based on the Australian combat and health brigade capabilities. 

**Organisation**. The simulation is built around the following design:

1.  The combat brigade has formed three battle groups headquartered by the resident (2) infantry battalions and (1) cavalry regiment. a commander's reserve has been established by the brigade commander formed around a combat team.  The cavalry regiment based battlegroup is performing a screen forward of the two infantry battlegroups. Each battlegroup has been force assigned two treatment teams to provide close health support.

2. Artillery support is assigned with battery's assigned Direct Support (DS) to the infantry battlegroups and are placed with the capacity to provide support to their supported call-signs. The Brigade Headquarters (HQ) has established a HQ-Forward and HQ-Main. The HQ-Forward is placed one tactical bound behind the forward battlegroups. An additiona treatment team has been establsihed to provide close health support to the artillery unit and HQ-Forward.

3. The Combat Service Support Battalion (CSSB) has established the Brigade Maintenance Area (BMA). One further treatment team has been established to provide close health support within the BMA. To provide surgical capability to the brigade a Role 2 - Enhanced (R2E) hospital has been established within the BMA.

**Health Teams**. The health architecture is made up of the following health teams:

- **Treatment Team**. The treatment teams are made up of the following personnel resources:
  
  - 3 x Medics
  
  - 1 x Nurse
  
  - 1 x Doctor

- **R2E**. 

**Transport Resources**. Transport resources are typically drawn from the supported call-sign or a CSS element. the following transport resources are available by echelon:

- **Treatment Team**. Treatment teams have access to battlegroup vehicle assets to effect casualty evacuations. The treatment teams have access to:
  
  - 2 x PMV Ambulances, used for the transport of Wounded In Action (WIA) and Disease and Non-Battle Injury (DNBI) to the R2E (where required)
  
  - 1 x HX2 40 M, used for the transport of Killed In Action (KIA) to the mortuary (at R2E)

- **R2E**. 


