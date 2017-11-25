{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Handler.Index where

import Import
import LayoutPark

formLogin :: Form (Text, Text)
formLogin = renderDivs $ (,)
    <$> areq textField "Login " Nothing
    <*> areq passwordField "Senha " Nothing

autenticar :: Text -> Text -> HandlerT App IO (Maybe (Entity Usuario))
autenticar login senha = runDB $ selectFirst [UsuarioLogin ==. login
                                             ,UsuarioSenha ==. senha] []
    
getLoginR :: Handler Html
getLoginR = do 
    (widget,enctype) <- generateFormPost formLogin
    msg <- getMessage
    layoutLogin $ do 
        [whamlet|
            <div class="msg-erro-login">
                $maybe mensagem <- msg 
                    <p> #{mensagem}
            <form action=@{LoginR} enctype=#{enctype} method=post>
                ^{widget}
                <button type="submit" value="login" class="mdl-button mdl-button-login mdl-js-button mdl-button--raised">Entrar
        |]

postLoginR :: Handler Html
postLoginR = do
    ((res,_),_) <- runFormPost formLogin
    case res of 
        FormSuccess ("root","park123") -> do 
            setSession "_ID" "admin"
            redirect EntradaR
        FormSuccess (email,senha) -> do 
            usuario <- autenticar email senha 
            case usuario of 
                Nothing -> do 
                    setMessage $ [shamlet| Usuario ou senha invalido |]
                    redirect LoginR 
                Just (Entity usuarioId usuario) -> do 
                    setSession "_ID" (usuarioNome usuario)
                    redirect EntradaR
        _ -> redirect EntradaR

postLogoutR :: Handler Html
postLogoutR = do 
    deleteSession "_ID"
    redirect LoginR

getIndexR :: Handler Html
getIndexR = do
    redirect LoginR