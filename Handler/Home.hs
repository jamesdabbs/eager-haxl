{-# LANGUAGE TupleSections, OverloadedStrings, StandaloneDeriving, FlexibleInstances #-}
module Handler.Home where

import Import

import Data.Int (Int64)
import qualified Data.Text as T

-- Haxl imports
import Data.Hashable
import Data.Typeable
import Haxl.Core

import Control.Monad (unless)
import Data.Maybe (mapMaybe)
import Data.Traversable (for)


-- We don't want n=0 in our n+1 investigation
-- Run `curl -XPOST http://localhost:3000/populate`
createUser :: Int64 -> Handler ()
createUser n = do
  _id <- runDB . insert $ User email Nothing
  _ <- runDB . insert $ Uname _id name
  return ()
  where
    email = "email" <> (thow n) <> "@example.com"
    name  = "User " <> (thow n)
    thow = T.pack . show

postPopulateR :: Handler Value
postPopulateR = do
  mapM_ createUser [1..10]
  returnJson ("Okay!" :: Text)


-- An easy way to get yourself in to trouble: run queries in a widget
usernameWidget :: UserId -> Widget
usernameWidget _id = do
  (Entity _ n) <- handlerToWidget . runDB . getBy404 . UniqueUname $ _id
  [whamlet|#{unameName n}|]

getBadR :: Handler Html
getBadR = do
  users <- runDB $ selectList [] [Asc UserId]
  defaultLayout $(widgetFile "bad")


-- Now with haxl
-- This setup cribbed directly from the README: https://github.com/facebook/Haxl/blob/master/example/sql/readme.md
data UserReq a where
  GetAllIds   :: UserReq [Id]
  GetUnameById :: Id -> UserReq Name
  deriving (Typeable)

-- i_have_no_idea_what_im_doing.jpg
deriving instance Typeable1 Entity
deriving instance Typeable2 KeyBackend

dekey (Key (PersistInt64 n)) = n

type Id = UserId
type Name = Entity Uname

deriving instance Eq (UserReq a)
instance Hashable (UserReq a) where
   hashWithSalt s GetAllIds        = hashWithSalt s (0::Int)
   hashWithSalt s (GetUnameById a) = hashWithSalt s (1::Int, dekey a)

instance StateKey UserReq where
  data State UserReq = UserState
    { userStateHandler  :: Handler [UserId] -> IO [UserId]
    , userStateHandler2 :: Handler [Entity Uname]  -> IO [Entity Uname]
    }

instance DataSourceName UserReq where
  dataSourceName _ = "UserDataSource"

deriving instance Show (UserReq a)
instance Show1 UserReq where show1 = show

type Haxl = GenHaxl ()

-- Here's where we veer off the beaten path a little
instance DataSource () UserReq where
  fetch _state _flags _userEnv blockedFetches = SyncFetch $ do

    unless (null allIdVars) $ do

      -- Fetch all the IDs.
      ids <- runHandler $ runDB $ selectKeysList ([] :: [Filter User]) []

      -- Store the results.
      mapM_ (\m -> putResult m (Right ids)) allIdVars

    unless (null ids) $ do

      -- Fetch all the names.
      users <- runHandler2 $ runDB $ selectList [UnameUserId <-. ids] []

      -- Store the results.
      mapM_ (\ (m, res) -> putResult m (Right res)) (zip vars users)

    where
    runHandler = userStateHandler _state
    runHandler2 = userStateHandler2 _state

    allIdVars :: [ResultVar [Id]]
    allIdVars = mapMaybe
      (\bf -> case bf of
        BlockedFetch GetAllIds m -> Just m
        _ -> Nothing)
      blockedFetches

    ids :: [Id]
    vars :: [ResultVar Name]
    (ids, vars) = unzip $ foldr go [] blockedFetches

    go
      :: BlockedFetch UserReq
      -> [(Id, ResultVar Name)]
      -> [(Id, ResultVar Name)]
    go (BlockedFetch (GetUnameById userId) m) acc = (userId, m) : acc
    go _ acc = acc

-- And bring it on home
getAllUserIds :: Haxl [Id]
getAllUserIds = dataFetch GetAllIds

getUnameById :: Id -> Haxl (Entity Uname)
getUnameById userId = dataFetch (GetUnameById userId)

getAllUsernames :: Haxl [Entity Uname]
getAllUsernames = do
  userIds <- getAllUserIds       -- Round 1
  for userIds $ \userId -> do    -- Round 2
    getUnameById userId

getGoodR :: Handler Html
getGoodR = do
  runH <- handlerToIO
  runH2 <- handlerToIO

  names <- liftIO $ do
    let stateStore = stateSet UserState{ userStateHandler = runH, userStateHandler2 = runH2 } stateEmpty
    env0 <- initEnv stateStore ()
    runHaxl env0 getAllUsernames
  defaultLayout $(widgetFile "good")
