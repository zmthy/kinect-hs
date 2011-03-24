-- | A small collection of handling IO errors, particularly for interfacing
-- with C code which return integer error codes.
module Helpers.IO (tryC, tryIO) where

-- | Checks the value of an action with C style response codes, and throws an
-- exception if it failed.
tryC :: Integral a  -- ^ No requirement on the actual type of the integer
     => String      -- ^ The message to respond with if the action fails
     -> IO a        -- ^ The action to evaluate
     -> IO ()
tryC message = tryIO message (== 0) (\_ -> ())

-- | A far more generic version of 'tryC', this function evaluates the result
-- of an action by a given condition. If the condition returns 'True', the
-- result is passed to a filter and then returned, otherwise an IO exception
-- is raised with the given message.
tryIO :: String       -- ^ The message to respond with if the action fails
      -> (a -> Bool)  -- ^ The condition to check the result of the action
      -> (a -> b)     -- ^ The filter to pass result through if successful
      -> IO a         -- ^ The action to evaluate
      -> IO b
tryIO message condition result action = do
    status <- action
    if condition status
        then return (result status)
        else ioError (userError message)
