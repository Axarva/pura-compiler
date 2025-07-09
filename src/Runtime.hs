-- src/PuraRuntime.hs
module Runtime (PuraEffect(..), executePuraProgram) where

import Data.Text (Text, unpack)
import Control.Monad (forM_) -- For iterating over a list of effects in a monadic context

-- Define the data type for runtime effects that our compiler will generate
-- This should match the 'GeneratedPuraEffect' defined in CodeGen.hs for clarity
data PuraEffect = GConsoleWrite Text
                 -- Add more: GFileReadEffect Text, GNetworkSendEffect Text
                 deriving (Show, Eq)

-- The function that executes a list of PuraEffect descriptors
-- It takes a list of effects and performs them in the IO monad
executePuraProgram :: [PuraEffect] -> IO ()
executePuraProgram effects = do
    putStrLn "\n--- Pura Runtime Executing Effects ---"
    forM_ effects $ \effect ->
        case effect of
            GConsoleWrite msg -> putStrLn $ "[CONSOLE_OUTPUT]: " ++ unpack msg
            -- Add more cases as you add more effects
    putStrLn "--- Execution Complete ---\n"