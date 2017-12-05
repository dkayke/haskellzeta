{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Handler.CadCliente where

import Import
import LayoutPark
import Data.Maybe


formCliente :: Form Cliente 
formCliente = renderDivs $ Cliente
    <$> areq textField "Nome " Nothing
    <*> areq textField "CNH " Nothing
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
                <h2> #{mensagem}
            <form action=@{CadClienteR} enctype=#{enctype} method=post>
                ^{widgetCli}
                    <button class="mdl-button mdl-js-button mdl-button--raised bt-acao">Cadastrar cliente
        |]

postCadClienteR :: Handler Html
postCadClienteR = do 
    ((res,_),_) <- runFormPost formCliente
    case res of 
        FormSuccess cliente -> do 
            cnhcli <- runDB $ selectFirst [ClienteCnh ==. clienteCnh cliente][]
            case cnhcli of
                Just resultadocnh -> do
                    setMessage $ [shamlet| CNH ja cadastrada por outro motorista |]
                    redirect CadClienteR
                _ -> do
                    _ <- runDB $ insert cliente
                    setMessage $ [shamlet| Cliente cadastrado com sucesso |]
                    redirect CadClienteR
        _ -> redirect CadClienteR