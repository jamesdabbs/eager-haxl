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
import Data.List (intercalate)
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
  GetNameById :: Id -> UserReq Name
  deriving (Typeable)

type Id = Int64
type Name = String

deriving instance Eq (UserReq a)
instance Hashable (UserReq a) where
   hashWithSalt s GetAllIds       = hashWithSalt s (0::Int)
   hashWithSalt s (GetNameById a) = hashWithSalt s (1::Int, a)

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

dekey (Key (PersistInt64 n)) = n
dekey _ = error "Can't dekey"

rekey = Key . PersistInt64

-- Here's where we veer off the beaten path a little
instance DataSource () UserReq where
  fetch _state _flags _userEnv blockedFetches = SyncFetch $ do

    unless (null allIdVars) $ do

      -- Fetch all the IDs.
      ids <- runHandler $ runDB $ selectKeysList ([] :: [Filter User]) []

      -- Store the results.
      mapM_ (\m -> putResult m (Right . map dekey $ ids)) allIdVars

    unless (null ids) $ do

      -- Fetch all the names.
      users <- runHandler2 $ runDB $ selectList [UnameUserId <-. (map rekey ids)] []
      let names = map (unameName . entityVal) users
      -- names <- sql $ unwords
      --   [ "select name from names where"
      --   , intercalate " or " $ map ("id = " ++) idStrings
      --   , "order by find_in_set(id, '" ++ intercalate "," idStrings ++ "')"
      --   ]

      -- Store the results.
      mapM_ (\ (m, res) -> putResult m (Right res)) (zip vars (map show names))

    where
    runHandler = userStateHandler _state
    runHandler2 = userStateHandler2 _state

    sql a = do
      putStrLn $ "~~~~~~~   " ++ a
      return [1..9]

    allIdVars :: [ResultVar [Id]]
    allIdVars = mapMaybe
      (\bf -> case bf of
        BlockedFetch GetAllIds m -> Just m
        _ -> Nothing)
      blockedFetches

    idStrings :: [String]
    idStrings = map show ids

    ids :: [Id]
    vars :: [ResultVar Name]
    (ids, vars) = unzip $ foldr go [] blockedFetches

    go
      :: BlockedFetch UserReq
      -> [(Id, ResultVar Name)]
      -> [(Id, ResultVar Name)]
    go (BlockedFetch (GetNameById userId) m) acc = (userId, m) : acc
    go _ acc = acc

-- And bring it on home
getAllUserIds :: Haxl [Id]
getAllUserIds = dataFetch GetAllIds

getUsernameById :: Id -> Haxl Name
getUsernameById userId = dataFetch (GetNameById userId)

getAllUsernames :: Haxl [Name]
getAllUsernames = do
  userIds <- getAllUserIds       -- Round 1
  for userIds $ \userId -> do    -- Round 2
    getUsernameById userId

getGoodR :: Handler Value
getGoodR = do
  runH <- handlerToIO
  runH2 <- handlerToIO

  liftIO $ do
    let stateStore = stateSet UserState{ userStateHandler = runH, userStateHandler2 = runH2 } stateEmpty
    env0 <- initEnv stateStore ()
    names <- runHaxl env0 getAllUsernames
    return ()
  error "I hope that was what you wanted"
