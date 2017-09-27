import System.IO.Unsafe
import Control.Concurrent
import Control.Concurrent.STM.TChan

type Instr = Agent -> IO Agent
data Agent = Agent (Maybe Int) (Maybe Int) [Instr]

chanChan = unsafePerformIO $ newTChanIO
