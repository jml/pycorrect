module Main (main) where

import BasicPrelude

import PyCorrect.Python
  ( annotateSourceCode
  , ParseError
  )
import PyCorrect.Types (Annotation(..))


findUnresolvedReferences :: Text -> Either ParseError [String]
findUnresolvedReferences content = do
  annotations <- annotateSourceCode content
  return $ [identifier | (identifier, Just UnresolvedReference) <- annotations]


main :: IO ()
main = do
  args <- getArgs
  forM_ args $ \filename -> do
    contents <- readFile (textToString filename)
    case findUnresolvedReferences contents of
      Left err -> print err
      Right [] -> return ()
      Right identifiers -> do
        putStrLn $ "Unresolved references in " <> filename <> ":"
        mapM_ (putStrLn . fromString . ("* " ++)) identifiers
        putStrLn ""
