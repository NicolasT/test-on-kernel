{-# LANGUAGE LambdaCase #-}

module Foreign.Utils (
      throwIfNonZero
    ) where

throwIfNonZero :: (Show a, Num a, Eq a) => String -> IO a -> IO ()
throwIfNonZero location act = act >>= \case
    0 -> return ()
    rc -> ioError $ userError $ location ++ ": returned non-0 value " ++ show rc
