module NonEmptyExt(
  rights, sequence
) where

import Prelude hiding (sequence)
import qualified Data.Either as Either
import Data.List.NonEmpty

rights :: NonEmpty (Either a b) -> NonEmpty b 
rights (Right x :| xs) = fromList $ x : Either.rights xs
rights (_ :| xs) = fromList $ Either.rights xs

sequence :: NonEmpty (Either a b) -> Either a (NonEmpty b)
sequence list =
  -- We can safely convert to non empty here, since if result of sequence' is Right it means we have at least one element.
  fromList <$> sequence' (toList list)

sequence' :: [Either a b] -> Either a [b] 
sequence' = 
  foldr prepend' (Right [])

prepend' :: Either a b -> Either a [b] -> Either a [b]
prepend' (Right first) (Right rest) = Right (first : rest) 
prepend' (Left err1) (Right _) = Left err1
prepend' (Right _) (Left err2) = Left err2
prepend' (Left err1) (Left _) = Left err1
