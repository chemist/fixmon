module Check.Cron where

import           Control.Monad.RWS.Class
import           GNS.Data

{--
Выполнение проверок по расписанию

по расписанию стартуем задачи на проверку
результат пишем в очередь на обработку

--}
