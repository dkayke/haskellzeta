{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Handler.CadCliente where

import Import
import LayoutPark
--import Database.Persist.Postgresql

formCliente :: Form Cliente 
formCliente = renderDivs $ Cliente
    <$> areq textField "Nome " Nothing
    <*> areq intField "CNH " Nothing
    <*> areq intField "CPF " Nothing
    <*> aopt dayField "Data de Nascimento " Nothing
    <*> aopt intField "Telefone 1 " Nothing
    <*> aopt intField "Telefone 2 " Nothing

getCadClienteR :: Handler Html
getCadClienteR = do 
    (widgetCli,enctype) <- generateFormPost formCliente
    msg <- getMessage
    layoutParkSec "CadClienteR" 
        [whamlet|
            <nav>
                <div class="breadcrumb">
                    <div class="col s12 mdl-color--grey-100">
                        <span>HaskPark /
                        <a href=@{CadastroR}>Cadastro <span>/
                        <a>Cliente
             $maybe mensagem <- msg 
                <h1> #{mensagem}
            <form action=@{CadClienteR} method=post>
                ^{widgetCli}
                    <button class="mdl-button mdl-js-button mdl-button--raised bt-acao">Cadastrar cliente
        |]

postCadClienteR :: Handler Html
postCadClienteR = undefined 