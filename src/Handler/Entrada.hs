
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Handler.Entrada where

import Import
import LayoutPark

formEntrada :: Form (TipoentradaId, ClienteId, VeiculoId)
formEntrada = renderDivs $ (, , )
    <$> areq (selectField $ optionsPersistKey [] [] tipoentradaNome) "Selecione o tipo de entrada:" Nothing
    <*> areq (selectField $ optionsPersistKey [] [] clienteCnh) "CNH" Nothing
    <*> areq (selectField $ optionsPersistKey [] [] veiculoPlaca) "Placa do Veículo" Nothing

getEntradaR :: Handler Html
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
        FormSuccess (entrada, cnh, placa) -> do
            Just entradatipo <- runDB $ selectFirst [TipoentradaId ==. entrada][]
            Just cnhcli <- runDB $ selectFirst [ClienteId ==. cnh][]
            placaveic <- runDB $ selectFirst [VeiculoId !=. placa][]
            case placaveic of
                Just resultadoplaca -> do
                    _ <- runDB $ insert $ Entrada (entityKey entradatipo) 
                                                  (entityKey cnhcli)
                                                  (entityKey resultadoplaca)
                                                  (dthora)
                    setMessage $ [shamlet| Entrada efetuada |]
                    redirect EntradaR
                _ -> do
                    setMessage $ [shamlet| Este veículo já deu entrada em nosso estacionamento |]
                    redirect EntradaR                
        _ -> redirect EntradaR
