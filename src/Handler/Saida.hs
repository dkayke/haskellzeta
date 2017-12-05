{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Handler.Saida where

import Import
import LayoutPark
import Data.Time.Clock
import Data.Maybe
import Database.Persist

cliById :: ClienteId -> Widget
cliById cid = do 
    cli <- handlerToWidget $ runDB $ get404 cid
    [whamlet|
        <td class="mdl-data-table__cell--non-numeric">
            #{clienteNome cli}
        <td class="mdl-data-table__cell--non-numeric">    
            #{clienteCnh cli}
    |]

veicById :: VeiculoId -> Widget
veicById vid = do 
    veic <- handlerToWidget $ runDB $ get404 vid
    [whamlet|
        <td class="mdl-data-table__cell--non-numeric">
            #{veiculoPlaca veic}
    |]

getSaidaLiR :: Handler Html
getSaidaLiR = do 
    msg <- getMessage
    locacoes <- runDB $ selectList [] [] :: Handler [Entity Entrada]
    layoutPark $ do 
        [whamlet|
            $maybe mensagem <- msg 
                <div id="snackbar"> #{mensagem}
                <script> myFunction();
                
            <nav>
                <div class="breadcrumb">
                    <div class="col s12 mdl-color--grey-100">
                        <span>HaskPark /
                        <a>Saída

            <label> Pesquisar por placa
            <input type="text" id="searchInput" onkeyup="searchInTable()">

            <table class="mdl-data-table mdl-js-data-table mdl-shadow--2dp" id="searchTable"> 
                <thead>
                    <tr>
                        <th class="mdl-data-table__cell--non-numeric"">Placa do veículo
                        <th class="mdl-data-table__cell--non-numeric">Motorista
                        <th class="mdl-data-table__cell--non-numeric">CNH
                        <th class="mdl-data-table__cell--non-numeric">Ação
                <tbody>
                    $forall (Entity locsid entrada) <- locacoes
                        <tr>
                            ^{veicById $ entradaVeiculoid entrada}
                            ^{cliById $ entradaClienteid entrada}
                            <td class="mdl-data-table__cell--non-numeric">
                                <form action=@{SaidaDeR locsid} method=post>
                                    <button class="mdl-button mdl-js-button mdl-button--raised bt-acao">Saída
        |]

utcToMinutes :: UTCTime -> UTCTime -> NominalDiffTime
utcToMinutes a b =  (diffUTCTime  b a) / 60

trunc :: Double -> Int -> Double
trunc x n = (fromIntegral (floor (x * t))) / t
    where t = 10^n

valor :: Double -> Double -> Double -> Double -> NominalDiffTime -> Text -> Double
valor vlMeia vlHora vlDemais vlDia minutos tpentrada
    | round (minutos) >  60  = trunc (realToFrac ((minutos / 60) * (realToFrac vlDemais :: NominalDiffTime) + (realToFrac vlHora :: NominalDiffTime)) :: Double) 2
    | round (minutos) <= 60 && minutos >= 31 = trunc vlHora 2 
    | round (minutos) <= 30 && minutos >  0  = trunc vlMeia 2
    | round (minutos) >= 1440 && tpentrada == "dia" = trunc (realToFrac ((realToFrac (ceiling ((minutos/60)/24)) :: NominalDiffTime) * (realToFrac vlDia :: NominalDiffTime) + (realToFrac vlHora :: NominalDiffTime)) :: Double)  2
    | otherwise = 0

postSaidaDeR :: EntradaId -> Handler Html 
postSaidaDeR locsid = do
    hrsaida  <- liftIO getCurrentTime
    Just negocio <- runDB $ selectFirst [][]
    entrada <- runDB $ selectFirst [EntradaId ==. locsid] []
    case entrada of 
        Nothing -> do 
            setMessage $ [shamlet| Usuario e/ou senha invalido. |]
            redirect LoginR 
        Just (Entity _ (Entrada tpentrada cliid veiid hrentr)) -> do
            Just tipoentradaa <- runDB $ selectFirst [TipoentradaId ==. tpentrada][]
            sid <- runDB $ insert $ Saida (tpentrada) 
                                            (cliid) 
                                            (veiid) 
                                            (hrentr) 
                                            (hrsaida) 
                                            (valor 
                                                (negocioVlmeiahora . entityVal $ negocio)
                                                (negocioVlhora . entityVal $ negocio)
                                                (negocioVldemais . entityVal $ negocio)
                                                (negocioVldiario . entityVal $ negocio)
                                                (utcToMinutes hrentr hrsaida)
                                                (tipoentradaNome . entityVal $ tipoentradaa)
                                            )
            runDB $ delete locsid
            redirect (ImpressaoR sid)