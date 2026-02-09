planet "Starport Alpha" at (0, 0) type fuel
planet "Trading Post" at (150, 150) type none
planet "Repair Station Beta" at (-150, 150) type repair
planet "Research Outpost" at (0, 300) type fuel
planet "Lost Temple" at (0, -300) type none
planet "Deep Space Station" at (400, 400) type none

mission "Milk Run" from "Starport Alpha" to "Trading Post" timeLimit 30
mission "Rescue Mission" from "Repair Station Beta" to "Research Outpost" timeLimit 60
mission "Deep Expedition" from "Lost Temple" to "Deep Space Station" timeLimit 90
