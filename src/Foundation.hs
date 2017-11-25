{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Yesod.Core.Types     (Logger)

data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static 
    , appConnPool    :: ConnectionPool 
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

mkYesodData "App" $(parseRoutesFile "config/routes")

type Form a = Html -> MForm Handler (FormResult a, Widget)

instance Yesod App where
    makeLogger = return . appLogger
    authRoute _ = Just $ LoginR
    isAuthorized LoginR _ = return Authorized
    isAuthorized LogoutR _ = return Authorized
    isAuthorized EntradaR _ = ehUsuario
    isAuthorized _ _ = return Authorized
    --isAuthorized _ _ = ehUsuario

isRoot :: Handler AuthResult
isRoot = do
    sessao <- lookupSession "_ID"
    case sessao of 
        Nothing -> return AuthenticationRequired
        (Just "root") -> return Authorized
        (Just _ ) -> return $ Unauthorized "Permitido apenas ao root!"
    
isUser :: Handler AuthResult
isUser = do
    sessao <- lookupSession "_ID"
    case sessao of 
        Nothing -> return AuthenticationRequired
        (Just "root") -> return $ Unauthorized "Proibido ao usuário root!" 
        (Just _) -> return Authorized
        
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

instance HasHttpManager App where
    getHttpManager = appHttpManager
