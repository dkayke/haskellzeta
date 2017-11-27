{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Handler.CadVeiculo where

import Import
import LayoutPark
import Database.Persist.Postgresql

formVeiculo :: Form (Veiculo, Int)
formVeiculo = renderDivs $ (,) <$> (Veiculo 
    <$> areq (selectField $ optionsPersistKey [] [] tipoveiculoNome) "Tipo Veiculo " Nothing
    <*> areq textField "Placa " Nothing
    <*> areq textField "UF - Cidade da Placa " Nothing
    <*> pure (toSqlKey 0)
    <*> aopt textField "Modelo do Veiculo" Nothing
    <*> aopt intField "Ano " Nothing
    <*> aopt textField "Cor " Nothing)
    <*> areq intField "CNH do Motorista" Nothing
    
getCadVeiculoR :: Handler Html
getCadVeiculoR = do 
    (widgetVeic,enctype) <- generateFormPost formVeiculo
    msg <- getMessage
    layoutParkSec "CadVeiculoR" 
        [whamlet|
            <nav>
                <div class="breadcrumb">
                    <div class="col s12 mdl-color--grey-100">
                        <span>HaskPark /
                        <a href=@{CadastroR}>Cadastro <span>/
                        <a>Veiculo
             $maybe mensagem <- msg 
                <h1> #{mensagem}
            <form action=@{CadVeiculoR} enctype=#{enctype} method=post>
                ^{widgetVeic}
                    <button class="mdl-button mdl-js-button mdl-button--raised bt-acao">Cadastrar veiculo
        |]

postCadVeiculoR :: Handler Html
postCadVeiculoR = undefined
    