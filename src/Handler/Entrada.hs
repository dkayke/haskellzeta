
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Handler.Entrada where

import Import
import LayoutPark
import Data.Time.Clock
import Data.Time.LocalTime
import System.IO.Unsafe
import Database.Persist.Postgresql

formEntrada :: Form (Int, Text)
formEntrada = renderDivs $ (,)
    <$> areq intField "CNH" Nothing
    <*> areq textField "Placa do Veículo" Nothing

getEntradaR :: Handler Html -- entrando com formulario 
getEntradaR = do 
    (widget,enctype) <- generateFormPost formEntrada
    msg <- getMessage
    layoutPark $ do 
        [whamlet|
            <nav>
                <div class="breadcrumb">
                    <div class="col s12 mdl-color--grey-100">
                        <span>HaskPark /
                        <a>Entrada
                    
            <div class="msg-erro-login">
                $maybe mensagem <- msg 
                    <p> #{mensagem}
            <form action=@{EntradaR} enctype=#{enctype} method=post>
                ^{widget}
                <button type="submit" value="entrada" class="mdl-button mdl-button-login mdl-js-button mdl-button--raised">Entrar
        |]
        
postEntradaR :: Handler Html
postEntradaR = do 
    ((res,_),_) <- runFormPost formEntrada
    dthora <- liftIO getCurrentTime
    case res of 
        FormSuccess (cnh, placa) -> do 
            Just cnhcli <- runDB $ selectFirst [ClienteCnh ==. cnh][]
            Just placaveic <- runDB $ selectFirst [VeiculoPlaca ==. placa][]
            _ <- runDB $ insert $ Entrada (entityKey cnhcli) 
                                          (entityKey placaveic)
                                          (dthora)
            setMessage $ [shamlet| Entrada efetuada |]
            redirect EntradaR
        _ -> redirect EntradaR
    