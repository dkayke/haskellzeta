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
    isAuthorized CadUsuarioR    _ = isRoot
    isAuthorized NegocioR       _ = isRoot
    isAuthorized BuscaR         _ = isUser
    isAuthorized CadastroR      _ = isUser
    isAuthorized CadClienteR    _ = isUser
    isAuthorized CadVeiculoR    _ = isUser
    isAuthorized EntradaR       _ = isUser
    isAuthorized (ImpressaoR _) _ = isUser
    isAuthorized (ResultadoR _) _ = isUser
    isAuthorized ResultadoPR    _ = isUser
    isAuthorized SaidaLiR       _ = isUser
    isAuthorized (SaidaDeR _)   _ = isUser
    isAuthorized _ _ = return Authorized
    errorHandler NotFound = fmap toTypedContent $ defaultLayout $ do
        redirect Erro404R
    errorHandler other = defaultErrorHandler other

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
        
isLogged :: Handler AuthResult
isLogged = do
    sessao <- lookupSession "_ID"
    case sessao of 
        Nothing -> redirect LoginR
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
