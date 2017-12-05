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

formVeiculo :: Form (Veiculo, Text)
formVeiculo = renderDivs $ (,) <$> (Veiculo 
    <$> areq (selectField $ optionsPersistKey [] [] tipoveiculoNome) "Tipo Veículo " Nothing
    <*> areq textField "Placa " Nothing
    <*> areq textField "UF - Cidade da Placa " Nothing
    <*> pure (toSqlKey 0)
    <*> aopt textField "Modelo do Veiculo" Nothing
    <*> aopt intField "Ano " Nothing
    <*> aopt textField "Cor " Nothing)
    <*> areq textField "CNH do Motorista" Nothing
    
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
                <h2> #{mensagem}
            <form action=@{CadVeiculoR} enctype=#{enctype} method=post>
                ^{widgetVeic}
                    <button class="mdl-button mdl-js-button mdl-button--raised bt-acao">Cadastrar veiculo
        |]

postCadVeiculoR :: Handler Html
postCadVeiculoR = do 
    ((res,_),_) <- runFormPost formVeiculo
    case res of
        FormSuccess (veiculo, cnh) -> do 
            placaveic <- runDB $ selectFirst [VeiculoPlaca ==. veiculoPlaca veiculo][] 
            case placaveic of
                Just resultadoplaca -> do
                    setMessage $ [shamlet| Placa ja cadastrada |]
                    redirect CadVeiculoR
                _ -> do
                    cnhcli <- runDB $ selectFirst [ClienteCnh ==. cnh][]
                    case cnhcli of 
                        Just resultadocnh -> do
                            _ <- runDB $ insert $ Veiculo (veiculoTipoveic veiculo) 
                                                          (veiculoPlaca veiculo)
                                                          (veiculoUfplaca veiculo)
                                                          (entityKey resultadocnh)
                                                          (veiculoModelo veiculo)
                                                          (veiculoAno veiculo)
                                                          (veiculoCor veiculo)
                            setMessage $ [shamlet| Veículo cadastrado com sucesso |]
                            redirect CadVeiculoR
                        _ -> do
                            setMessage $ [shamlet| Motorista não cadastrado |]
                            redirect CadVeiculoR
        _ -> redirect CadVeiculoR