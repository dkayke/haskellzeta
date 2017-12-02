
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

getEntradaR :: Handler Html -- entrando com formulario 
getEntradaR = do 
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
                <button type="submit" value="entrada" class="mdl-button mdl-button-login mdl-js-button mdl-button--raised">Entrar
        |]
        

postEntradaR :: Handler Html
postEntradaR = undefined