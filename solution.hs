module IPv4 where
import Data.Word  (Word32)

type IPString = String

word32ToIP :: Word32 -> IPString
word32ToIP word32 = show (mod (div word32 16777216) 256) ++ "." ++ show (mod (div word32 65536) 256) ++ "." ++ show (mod (div word32 256) 256) ++ "." ++ show (mod word32 256)
