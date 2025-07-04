>  [!Caution]
> 
> Calculation of per minute rate must be verified. It is currently significantly under-representing DNBI case rates.

# Battlefield Casualty Handling

## 📘 Introduction

This is a Discrete Event Simulation (DES) written in R that uses the simmer package. The code is designed to simulate the flow of battlefield casualties in Large Scale Combat Operations (LSCO) scenarios. The purpose of the simulation is to support decision making on deployed health system design with a focus on capacity planning.

## 🌍 Context

The code simulates a deployed combat brigade based on the Australian combat and health brigade capabilities.

**Organisation**. The simulation is built around the following design:

1. The combat brigade has formed `3` battlegroups headquartered by the resident `2` infantry battalions and `1` cavalry regiment. a commander's reserve has been established by the brigade commander formed around a combat team. The cavalry regiment based battlegroup is performing a screen forward of the two infantry battlegroups. Each battlegroup has been force assigned `2` treatment teams to provide close health support.

2. Artillery support is assigned with battery's assigned Direct Support (DS) to the infantry battlegroups and are placed with the capacity to provide support to their supported call-signs. The Brigade Headquarters (HQ) has established a HQ-Forward and HQ-Main. The HQ-Forward is placed one tactical bound behind the forward battlegroups. `1` additional treatment team has been establsihed to provide close health support to the artillery unit and HQ-Forward. `1` Role 2 - Basic (R2B) has been established in vicinity of the HQ-Forward to support damage control (DAMCON) and stabilise casualties prior to evacuation to higher level care.

3. The Combat Service Support Battalion (CSSB) has established the Brigade Maintenance Area (BMA). One further treatment team has been established to provide close health support within the BMA. To provide surgical capability to the brigade a Role 2 - Enhanced Heavy (R2E Heavy) hospital has been established within the BMA.

---

## 🤕 Casualty Rates

Casualties are generated based on rates outlined in [[1]](#References) with the following application:

1) **Disease and Non-Battle Injury (DNBI)**. lognorm(2.04, 1.89); based on Vietnman DNBI rates for combat troop ([[1]](#References), table A.5, p 31).  
   Of DNBI cases, 17% allocated to NBI with the remainder disease or battle fatigue ([[1]](#References), pp 22-23).  
2) **Wounded In Action (WIA)**. expon(6.86); based on Okinawa combat troop WIA rates ([[1]](#References), table A.7, p 32).  
3) **Killed In Action (KIA)**. expon(1.63); based on Okinawa combat troop KIA rates ([[1]](#References), table A.9, p 33).

These rates were converted from daily to per minute rates using the following algorithm:



The following casualty priority rates were used with the rates requiring surgery (informed by [[2]](#References)):

1. **Priority 1**. 65% of casualties with 90% requiring surgery.

2. **Priority 2**. 20% of casualties with 80% requiring surgery.

3. **Priority 3**. 15% of casualties with:
   
   1. 40% of DNBI requiring surgery.
   
   2. 60% of other priority 3 casualties requiring surgery. 



Per [[3]](#References), of those admitted to MTFs, the distribution for return to duty was 42.1 percent in Republic of Vietnam, 7.6 percent in the U.S. Indo-Pacific Command, and 33.4 percent in the CONUS.

---

## 👨‍⚕️Resource Initialisation

**Health Teams**. The health architecture is made up of the following health teams:

- **Treatment Team**. The `8` treatment teams are made up of the following personnel resources:
  
  - `3` Medics
  
  - `1` Nurse
  
  - `1` Doctor

- **R2B**. The R2B is a forward-deployed, damage control surgical facility designed to stabilize and prepare casualties for evacuation. It includes medical, surgical, nursing, and bed management capabilities.

- The R2B is made up of the following personnel resources:
  
  - Surgical Team
    
    - `2` Surgeons
    - `1` Anesthetist
    - `1` Medic
  - Emergency Team
    - `1` FACEM (Emergency Physician)
    - `1` Medic
    - `3` Nurses
  
  - Evacuation Team
    
    - `2` Medics
- **R2E Heavy**. The R2E Heavy is a large field hospital, capable only of static deployments. It is capable of limited major surgery, stabilisation and recovery of casualties. It includes medical, surgical, nursing and bed management capabilities.
- The R2E Heavy is made up of the following personnel resources:
  - `3` Surgical Teams
  - `3` Emergency Teams
    - `1` FACEM
    - `1` Medic
    - `3` Nurses
  
  - `3` ICU Teams
  - `3` Evacuation Teams

**Transport Resources**. Transport resources are typically drawn from the supported call-sign or a CSS element. the following transport resources are available by echelon:

- **Treatment Team**. Treatment teams have access to battlegroup vehicle assets to effect casualty evacuations. The treatment teams have access to:
  
  - `2` PMV Ambulances per treatment team (for a total of `16`), used for the transport of Wounded In Action (WIA) and Disease and Non-Battle Injury (DNBI) to the R2B (where required)
  
  - `1` HX2 40 M per treatment team (for a total of `8`), used for the transport of Killed In Action (KIA) to the mortuary (at R2E)

Transport resources are shared across all treatment teams.

- **R2B**. The R2B has `1` dedicated evacuation team operating a PMV Ambulance. In addition to the dedicated resource of the R2B, the R2B has access to the following resources:
  
  - ...

**Bed Resources**. Beds are finite resources and are seized by patients during treatment and recovery phases, influencing throughput and capacity under casualty load. Not all health elements have dedicated bed resources.

- **Treatment Team**. No dedicated bed resources.

- **R2B**. R2B includes limited inpatient capacity to stabilize casualties prior to evacuation or return to duty. The R2B has the following bed capacity:
  
  - `1` Operating Theatre (OT) bed
  
  - `2` Resus beds
  
  - `2` ICU beds
  
  - `5` Holding beds

- **R2E Heavy**. The R2E Heavy holds the following bed capacity:
  
  - `2` OT beds
  
  - `4` Resus beds
  
  - `4` ICU beds
  
  - `30` Holding beds

---

# Simulation Design

## 🔧Simulation Environment Setup

- **Framework:** `simmer` (discrete-event simulation)

- **Context:** Battlefield casualty handling with Role 1 treatment and evacuation logic

---

### 🧬 **Casualty Trajectory Logic**

- Each casualty is assigned:
  
  - A team (random)
  
  - A priority (if WIA or DNBI, not KIA)

- Then:
  
  - **Branch 1:** WIA/DNBI
    
    - Treatment by assigned team
    
    - Conditional transport if Priority 1 or 2
  
  - **Branch 2:** KIA
    
    - Treatment by assigned team
    
    - Then transport to mortuary

```mermaid
flowchart TD
    Start(["Start"]) --> TeamAssign("<b>Assign to Team</b> <br> <small>1 to team_count</small>")

    subgraph role1 [Role 1 Treatment Team]
        TeamAssign --> WIACheck{WIA/DNBI?}
        WIACheck -- Yes (WIA/DNBI) --> PriorityAssign("Assign Priority <br> <small> P1:65% P2:20% P3:15%</small>")
        PriorityAssign --> TreatWIA("<b>Treat WIA</b> <br> <small>Resources: 3 x medic, nurse, doctor using 1 medic and 1 clinician (nurse/doctor)<br> Duration: P1:rlnorm(120, 0.125), P2:rlnorm(60, 0.25), P3:rlnorm(20, 0.5)</small>")
        TreatWIA --> PriorityCheck{"P1 or P2?"}
        PriorityCheck -- Yes (P1/P2) --> TransportWIA("<b>Transport WIA</b> <br> <small>Resources: 16 x PMV_Amb <br> Duration: rlnorm(30, 0.5)</small>")
        PriorityCheck -- No (P3) --> MonitorWIA("<b>Monitor WIA Recovery</b?")
        WIACheck -- No (KIA) --> TreatKIA("<b>Treat KIA</b> <br> <small>Resources: 3 x medic, using 1 medic <br> Duration: rlnorm(30, 0.5)</small>")
        TreatKIA --> TransportKIA("<b>Transport KIA</b> <br> <small>Resources: 8 x HX_40M <br> Duration: rlnorm(30, 0.5)</small>")
    end

    MonitorWIA --> End
    TransportKIA --> End

    subgraph r2b [Role 2 Basic]
        TransportWIA --> bedCheck{"ICU Bed Available?"}
        bedCheck -- Yes (ICU) --> icu
        bedCheck -- No (Holding) --> hold["Occupy Holding Bed </br> (shortest-queue)"]

        hold --> waitForICU["Wait for ICU"]
        waitForICU --> icu["Occupy ICU Bed </br> (shortest-queue)"]
        icu --> EmergencyCare["Emergency Treatment (ATLS)"]
        EmergencyCare --> EmergencyDuration["Timeout ~ Emergency care (lnorm ~45min)"]
        EmergencyDuration --> ReleaseEmergency["Release Emergency Team"]

        ReleaseEmergency --> surgeryCheck{"Require Surgery?"}

        surgeryCheck -- P1 95%; P2 90% --> SeizeSurgery
        surgeryCheck -- P1 5%; P2 10% --> SeizeEvacSurg


        subgraph SurgeryPath [Surgery and Outcome]

            SeizeSurgery["Seize Surgical Team"] --> SurgeryDuration["Timeout ~ Surgery (truncnorm 60–180 min)"]
            SurgeryDuration --> ReleaseSurgery["Release Surgical Team"]
            ReleaseSurgery --> ReleaseICU["Release ICU Bed"]
            ReleaseICU --> SurvivalCheck{"Survives? (90%)"}
        end

        SurvivalCheck -- No --> DOW["Set dow = 1"] --> TimeoutDOW["Timeout 5 min"]
        SurvivalCheck -- Yes --> SeizeEvacSurg


        SeizeEvacSurg["Seize Evacuation Team"]
        SeizeEvacSurg --> TimeoutEvacSurg["Timeout ~ Evac (lnorm ~30min)"]
        TimeoutEvacSurg --> ReleaseEvacSurg["Release Evacuation Team"]
    end

TimeoutDOW --> End
ReleaseEvacSurg --> EndEnd    end

TimeoutDOW --> End
ReleaseEvacSurg --> End
```

[Online FlowChart &amp; Diagrams Editor - Mermaid Live Editor](https://mermaid.live/edit#pako:eNqVVP9vojAU_1ealyyRhCkFYUiMxpuXnDEuZLfcVy6mQqdk0JoCu3nG__0KBZWZy278QN-3vn4-7712DyGPKHjwmPDf4YaIHD1MA4bk9zmXWudnAJUQwC8NXV-P0AMl6STL4jXrBDBcjZSMcl55hr3VCA1XQv6ylCTJCJeeXHqWIS9YPuwpcwCaOuWUrsr-dTa53dDwaS-F3vTuw2x8UHGNQ0ah7zRDnSZAofJFzEWc747I6pyNvYUJ-dhz7Cvkm55pyMXysH11gaydUnEXlOTyYMW80kpgF6TvacYLEdLMQxZ6QSmN4lBHrBAZ1VHEw5wLVGQxWyOsnIiwSMphErM4jAlDnSq4p2K1Kve0ECSPOfNK-CJhXKQdbBo6MrrYtDW9pFObncqqjFZjVKG2dtmCmlWrkKoLAfgYSbC-OQ7g0K5Lux0-7vlmPSGCsGzLxXmlastb1cKOLJe_-LKcpCv0inVNw_oXjQtcd1zCshSmBWexrOQRUa2XeNA9DfkzFTsJbHzMdj5vZaL5bKKdZmDenoH5f89Au-vvZNgc3a7y_LLKb-BxJZ5P35Z9Y_F-BKfWVig-sqh8IuRSPhAq6FTrJuTV5vm5J2Cgw1rEEXi5KKgOKRUpKVXYl_sCyDc0pQF4UoyIeAogYAe5Z0vYD87TZpvgxXoD3iNJ5CWDYhuRnE5jshYkPVoFZREVt-VDBJ7pYrPKAt4eXqRuuvIiOXgwuLFdu29YNzrsQD4UXYwtx7b6huGatoMPOvypzjW6LrYMyzXdgd13Bo7jHP4CLfKgpA)

---

### 💀 **KIA (Killed in Action) Handling**

- **Treatment (`treat_kia()`):**
  
  - Selects medics using shortest-queue policy
  
  - Brief timeout (avg 15 min), simulating confirmation and prep for evacuation

- **Transport (`transport_kia()`):**
  
  - Uses HX2_40M vehicles (selected by shortest queue)
  
  - Simulates transport to mortuary near Role 2 (avg ~30 min, log-normal)

---

### 🤕 **WIA (Wounded in Action) / DNBI (Disease/Non-Battle Injury) Handling**

- **Treatment (`treat_wia()`):**
  
  - Seizes medics and clinicians (shortest-queue per group)
  
  - Treatment duration depends on assigned `priority` (1–3), drawn probabilistically
    
    - Priority 1: ~120 min
    
    - Priority 2: ~60 min
    
    - Priority 3: ~20 min

- **Evacuation Decision:**
  
  - Only Priority 1 & 2 trigger **transport to Role 2** via PMV_Amb
  
  - Priority 3 enters a recovery trajectory taking between 0 and 5 days to recover with an average of 2 days to recover and return to the force.

---

# References

[1] Blood, C., Zouris, J.M. and Rotblatt, D. (1997) Using the Ground Forces Casualty Forecasting System (FORECAS) to Project Casualty Sustainment. Accessed 23 Mar 2025 (Available at: [https://ia803103.us.archive.org/18/items/DTIC_ADA339487/DTIC_ADA339487_text.pdf](https://ia803103.us.archive.org/18/items/DTIC_ADA339487/DTIC_ADA339487_text.pdf)).

[2] Land Warfare Publication 0-5-2 Staff Officers Aide-Memoir 2018.

[3] https://www.armyupress.army.mil/Journals/Military-Review/Online-Exclusive/2025-OLE/Conserve-Fighting-Strength-in-LSCO/
