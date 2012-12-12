module BackendX86(compileX86, CmpError(..)) where

data CmpError = CErr String
instance Show CmpError where
    show (CErr s) = s

compileX86 :: String -> IO (Either CmpError ())
compileX86 path = do
    return $ Left $ CErr "Test"
