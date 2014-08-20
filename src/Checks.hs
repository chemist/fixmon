module Checks where

import Types
import Check.Http
import Check.System
import Data.Map (unions)


checkRoutes :: RouteCheck
checkRoutes = 
    let http   = map routeCheck [ HttpSimple ]
        shell  = map routeCheck [ Shell ]
        system' = map routeCheck [ HostName, Uptime, Boottime, CpuIntr, CpuLoad, CpuInfo, CpuSwitches, CpuUtil, LocalTime]
    in unions $ http ++ shell ++ system'
      
routes :: Route
routes =
    let system' = map route [HostName, Uptime, Boottime, CpuIntr, CpuLoad, CpuInfo, CpuSwitches, CpuUtil, LocalTime]
        http = map route  [HttpSimple]
        shell = map route [Shell]
    in unions $ system' ++ http ++ shell
