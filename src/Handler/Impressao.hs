{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Handler.Impressao where

import Import
import LayoutPark
import Control.Exception.Base
import Data.Time.Format

tipoVeicTicket :: TipoveiculoId -> Widget
tipoVeicTicket tvid = do 
    tvei <- handlerToWidget $ runDB $ get404 tvid
    [whamlet|
        <p>Tipo:&nbsp;&nbsp;  #{tipoveiculoNome tvei}
    |]

veiTicket :: VeiculoId -> Widget
veiTicket vid = do 
    vei <- handlerToWidget $ runDB $ get404 vid
    [whamlet|
        <p class="title-ticket">VEICULO
        ^{tipoVeicTicket $ veiculoTipoveic vei}
        <p>Placa:&nbsp; #{veiculoPlaca vei}
    |]

cliTicket :: ClienteId -> Widget
cliTicket cid = do 
    cli <- handlerToWidget $ runDB $ get404 cid
    [whamlet|
        <p class="title-ticket">CLIENTE
        <p>Nome:&nbsp; #{clienteNome cli}
        <p>CNH:&nbsp;&nbsp; #{clienteCnh cli}
    |]

getImpressaoSaR :: SaidaId -> Handler Html
getImpressaoSaR sid = do 
    imp <- runDB $ selectFirst [SaidaId ==. sid][] :: Handler (Maybe (Entity Saida))
    layoutTicket "ImpressaoSaR"
        [whamlet|
            <p>
                <b>HASK PARK
            $maybe (Entity said ticket) <- imp 
                ^{cliTicket $ saidaClienteid ticket}
                ^{veiTicket $ saidaVeiculoid ticket}
                <p class="title-ticket">HORÁRIOS
                <p>Entrada:&nbsp; #{(formatTime defaultTimeLocale "%d/%m/%Y às %T" (saidaHrentrada ticket))}
                <p>Saída:&nbsp;&nbsp;&nbsp;   #{(formatTime defaultTimeLocale "%d/%m/%Y às %T" (saidaHrsaida ticket))}
                <p>Valor total: R$ #{saidaVltotal ticket}
        |]