module Sentence where

import Test.Tasty.QuickCheck

data Sentence = Sentence Subject Verb
    deriving (Eq, Show)

data Subject = I | You | He | She | They
    deriving (Eq, Show)

data Verb = Am | Are | Is | Like | Likes | Dislike | Dislikes
    deriving (Eq, Show)

instance Arbitrary Verb where
    arbitrary = oneof $ fmap pure [Am, Are, Is, Like, Likes, Dislike, Dislikes]

instance Arbitrary Subject where
    arbitrary = oneof $ fmap pure [I, You, He, She, They]

instance Arbitrary Sentence where
    arbitrary = Sentence <$> arbitrary <*> arbitrary
