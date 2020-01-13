{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module AppM
  ( AppM (..)
  , App
  , Env (..)
  , liftEither
  , runApp
  ) where

import           Control.Monad.Except   (MonadError (..))
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Reader   (MonadReader (..))
import Control.Applicative (liftA2)

import           Data.Text              (Text)

import           Types          (Conf, FirstAppDB)
import           Types.Error    (Error)

-- | First, let's clean up our (Conf,FirstAppDB) with an application Env type.
-- We will add a general purpose logging function as well. Remember that
-- functions are values, we're able to pass them around and place them on
-- records like any other type.
data Env = Env

  -- | We will add a function to take some 'Text' input and print it to the
  -- console as a crude form of logging. Construct a function that matches this
  -- type so you can include it when you create the 'Env'.
  { envLoggingFn :: Text -> App ()

  -- We're able to nest records to keep things neat and tidy.
  , envConfig    :: Conf
  , envDB        :: FirstAppDB
  }

-- | It would be nice to remove the need to pass around our Env to every
-- function that needs it. Wouldn't it be great to have our functions run where
-- we could simply ask for the current Env?
--
-- We can create this by wrapping a function in a newtype like so:
--
-- This gives us a type that declares this function has access to our Env, and
-- will do something involving IO. It's another form of documentation and type
-- safety. AppM only has one definition and so we can easily understand what it
-- implies when used in our application.
newtype AppM e a = AppM
  { runAppM :: Env -> IO (Either e a)
  }
  -- | Quite often, GHC is able to write the code for us. In this case we just
  -- tell GHC that we want a Functor instance for our newtype, and it is able to
  -- correctly derive what is needed.
  deriving Functor
  -- | We could do this for the rest of these instances, but that would turn
  -- into "magic" what is otherwise straight-forward implementations. You are
  -- here to learn after all.

type App = AppM Error

runApp :: App a -> Env -> IO (Either Error a)
runApp = runAppM

instance Applicative (AppM e) where
  pure :: a -> AppM e a
  pure a = AppM (\_ -> pure $ Right a)

  (<*>) :: AppM e (a -> b) -> AppM e a -> AppM e b
  -- Imp 1 - With AppM e monad implementation
  -- (<*>) appMAtoB appMA = appMAtoB >>= (<$> appMA)
  -- Imp 2 - without relying on AppM e monad implementation
  -- (<*>) appMAtoB appMA = AppM $ \env -> let
  --   x = runAppM appMAtoB env
  --   y = runAppM appMA env
  --   in x >>=
  --       (\eitherAtoB -> y >>=
  --           (\eitherA -> pure (eitherAtoB <*> eitherA)))
  -- Imp 3 - cleaned up version of Imp 2
  -- (<*>) appMAtoB appMA = AppM $ \env -> let
  --   x = runAppM appMAtoB env
  --   y = runAppM appMA env
  --   in x >>=
  --       (\eitherAtoB -> (eitherAtoB <*>) <$> y)
  -- Imp 4 - Using applicative composition law
  -- (<*>) appMAtoB appMA = AppM $ \env -> let
  --   x = runAppM appMAtoB env
  --   y = runAppM appMA env
  --   in fmap (<*>) x <*> y
  -- Imp 5 - Using liftA2 instead of `fmap (<*>)` for applicative composition
  (<*>) appMAtoB appMA = AppM $ \env -> let
    x = runAppM appMAtoB env
    y = runAppM appMA env
    in liftA2 (<*>) x y


instance Monad (AppM e) where
  -- | When it comes to running functions in (AppM e) as a Monad, this will take
  -- care of passing the Env from one function to the next whilst preserving the
  -- error handling behaviour.
  (>>=) :: AppM e a -> (a -> AppM e b) -> AppM e b
  -- Imp 1 - Pattern matching
  -- (>>=) (AppM envToA) f = AppM $ \env ->
  --   envToA env >>= (\eitherA -> case eitherA of
  --       Left err -> pure $ Left err
  --       Right a -> runAppM (f a) env)
  -- Imp 2 - either function
  -- (>>=) (AppM envToA) f = AppM $ \env ->
  --   envToA env >>= either (pure . Left) (\a -> runAppM (f a) env)
  -- Imp 3 - pointfree that shiz
  (>>=) (AppM envToA) f = AppM $ \env ->
    envToA env >>= either (pure . Left) (flip runAppM env . f)


instance MonadError e (AppM e) where
  throwError :: e -> AppM e a
  throwError e = AppM $ \_ -> pure $ Left e

  catchError :: AppM e a -> (e -> AppM e a) -> AppM e a
  -- Imp 1 - pattern matching
  -- catchError appM f = AppM $ \env -> runAppM appM env >>= (\eErrA -> case eErrA of
  --     Left err -> runAppM (f err) env
  --     Right _ -> runAppM appM env)
  -- Imp 2 - ugly either
  -- catchError appM f = AppM $ \env -> runAppM appM env >>=
  --     (\eErrA -> either (\err -> runAppM (f err) env) (\a -> pure $ Right a) eErrA)
  -- Imp 3 - sexy either
  catchError appM f = AppM $ \env -> runAppM appM env >>= either (flip runAppM env . f) (pure . Right)

instance MonadReader Env (AppM e) where
  -- Return the current Env from the AppM.
  ask :: AppM e Env
  ask = reader id

  -- Run a (AppM e) inside of the current one using a modified Env value.
  local :: (Env -> Env) -> AppM e a -> AppM e a
  local f appMa = AppM $ \env -> runAppM appMa $ f env

  -- This will run a function on the current Env and return the result.
  reader :: (Env -> a) -> AppM e a
  reader f = AppM $ \env -> pure $ Right $ f env

instance MonadIO (AppM e) where
  -- Take a type of 'IO a' and lift it into our (AppM e).
  liftIO :: IO a -> AppM e a
  liftIO ioA = AppM $ \_ -> Right <$> ioA

-- | This is a helper function that will `lift` an Either value into our new AppM
-- by applying `throwError` to the Left value, and using `pure` to lift the
-- Right value into the AppM.
--
-- throwError :: MonadError e m => e -> m a
-- pure :: Applicative m => a -> m a
--
liftEither :: Either e a -> AppM e a
liftEither eA = case eA of
    Left err -> throwError err
    Right a -> AppM (\_ -> pure $ Right a)

-- Move on to ``src/Level07/DB.hs`` after this
