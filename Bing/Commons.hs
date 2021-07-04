module Commons where
import Control.Monad ()
import AbsBing ( Type )

type BlockDepth = Int
type Verbosity = Int
data VarInfo = VarInfo {dtype :: Type, depth :: BlockDepth, is_const :: Bool}
        deriving Show