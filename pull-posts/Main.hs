module Main
  ( main
  , go
  , getAllUserComments
  , batchGrabPosts ) where

import Control.Concurrent.Async
import Control.Applicative
import Control.Monad.STM
import Control.Concurrent.STM.TMChan
import Control.Monad.IO.Class
import Control.Monad
import Data.Text (Text)
import Reddit
import Reddit.Types.Comment (Comment, CommentID(..))
import Reddit.Types.Listing (Listing(..), ListingType(..))
import Reddit.Types.Options
import Reddit.Types.Post (PostID, Post)
import Reddit.Types.Subreddit
import Reddit.Types.User (Username(..))
import System.Environment
import System.Exit
import qualified Data.Text as Text
import qualified Reddit.Types.Comment as Comment
import qualified Reddit.Types.Post as Post

main :: IO ()
main = getArgs >>= \case
  (map Text.pack -> ["both", user, pass, req]) -> do
    go user pass req
    randoms user pass
  (map Text.pack -> ["user", user, pass, req]) ->
    go user pass req
  (map Text.pack -> ["random", user, pass]) ->
    randoms user pass
  _ -> do
    putStrLn "Invalid arguments"
    exitFailure

go :: Text -> Text -> Text -> IO ()
go u p r = do
  commentsChan <- newTMChanIO
  cs <- async $ void $ getAllUserComments u p r commentsChan
  resChan <- newTMChanIO
  rs <- async $ void $ batchGrabPosts u p commentsChan resChan
  os <- async $ output "comments" resChan
  mapM_ wait [cs, rs, os]

randoms :: Text -> Text -> IO ()
randoms u p = do
  postsChan <- newTMChanIO
  ps <- async $ void $ getRandomComments u p postsChan
  os <- async $ output "comments" postsChan
  mapM_ wait [ps, os]

getRandomComments :: Text -> Text -> TMChan (Bool, Text, Text, Text, Text) -> IO (Either (APIError RedditError) ())
getRandomComments u p outChan = runRedditWithRateLimiting u p $ run 2500 (Just Nothing)
  where
    run :: MonadIO m => Int -> Maybe (Maybe (PaginationOption PostID)) -> RedditT m ()
    run _ Nothing = liftIO $ atomically $ closeTMChan outChan
    run n _ | n <= 0 = liftIO $ atomically $ closeTMChan outChan
    run n (Just x) = do
      Listing _ a ps <- getPosts' (Options x (Just 100)) New (Just $ R "Dota2")
      forM_ ps $ \post ->
        case extract False post of
          Just r -> liftIO $ atomically $ writeTMChan outChan r
          Nothing -> return ()
      run (n - 100)$ Just <$> After <$> a

getAllUserComments :: Text -> Text -> Text -> TMChan PostID -> IO (Either (APIError RedditError) ())
getAllUserComments u p r out = runRedditWithRateLimiting u p (run (Just Nothing))
  where
    run Nothing = liftIO $ atomically $ closeTMChan out
    run (Just x) = do
      Listing _ a cs <- getUserComments' (Options x (Just 100)) (Username r)
      forM_ cs $ \c ->
        when (condition c) $
          case directParent c of
            Right _ -> return ()
            Left res -> liftIO $ atomically $ writeTMChan out res
      run $ Just <$> After <$> a
    condition c =
      Comment.subreddit c == R "Dota2" &&
      Comment.commentID c `notElem` dontUse &&
      Comment.author c /= Username "Intolerable" &&
      not (Comment.isDeleted c) &&
      Comment.score c > Just 0

batchGrabPosts :: Text -> Text -> TMChan PostID -> TMChan (Bool, Text, Text, Text, Text) -> IO (Either (APIError RedditError) ())
batchGrabPosts u p inChan outChan = runRedditWithRateLimiting u p run
  where
    run = do
      new <- liftIO $ atomically $ waitFor 100 inChan
      case new of
        [] -> liftIO $ atomically $ closeTMChan outChan
        xs -> do
          Listing _ _ ps <- getPostsInfo $ filter (firstRelevantPost <) $ filter (`notElem` ignoredPosts) xs
          forM_ (map (extract True) ps) $ \post ->
            case post of
              Just x -> liftIO $ atomically $ writeTMChan outChan x
              Nothing -> return ()
          run

waitFor :: Int -> TMChan a -> STM [a]
waitFor 0 _ = return []
waitFor n c =
  readTMChan c >>= \case
    Nothing -> return []
    Just x -> (:) <$> pure x <*> waitFor (n-1) c

output :: Show a => FilePath -> TMChan a -> IO ()
output filename inChan = loop
  where
    loop =
      atomically (readTMChan inChan) >>= \case
        Nothing -> return ()
        Just x -> do
          appendFile filename (show x ++ "\n")
          loop

extract :: Bool -> Post -> Maybe (Bool, Text, Text, Text, Text)
extract x p = case Post.content p of
  Post.SelfPost b _ -> Just (x, i, Post.title p, u, b)
    where
      Username u = Post.author p
      Post.PostID i = Post.postID p
  _ -> Nothing

directParent :: Comment -> Either PostID CommentID
directParent c = case Comment.inReplyTo c of
  Just x -> Right x
  Nothing -> Left $ Comment.parentLink c

firstRelevantPost :: PostID
firstRelevantPost = Post.PostID "25yo0g"

dontUse :: [CommentID]
dontUse = CommentID <$>
  [ "cm7su6w", "cm7t6n0", "cm8rzf5", "cm8s2bn", "cm8mzwd", "cm4b4lm", "cm0tfsm"
  , "clhuick", "cmaqw87", "cmaqids", "cmbetl4", "cmfj6b1", "cn86zbf", "cn85qe6"
  , "cn796oz", "cnau2dk", "cnj7j7k", "cnj4zt1", "cnpdfh1", "cnj8kb6", "cngmo11"
  , "cnfdo1g", "cncsuxa", "cnczkri", "cnb4t1x", "cn4zw2f", "cn4zoe0", "cn4s0a4"
  , "cn4s0oh", "cntj0ty", "cofjtfz", "cof08fe", "cof04fd", "coj0cs6", "cosspiq"
  , "cjb0y4t", "cjb0bls", "cjp6u42", "cjqh07v", "ck8o7ux", "cknnxpm", "cp827i8", "cpaekdo" ]

ignoredPosts :: [PostID]
ignoredPosts = Post.PostID <$>
  [ "2mqsb9", "2lf0hk", "2qavkd", "2qaqqr", "2q6yuh", "2q706l", "2q5lo4"
  , "2pgbpa", "2qg3rw", "2r6316", "2v0z7g" ]
