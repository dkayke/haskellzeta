{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Handler.Resultado where

import Import
import LayoutPark
import Data.Maybe

tipoveicById :: TipoveiculoId -> Widget
tipoveicById tvid = do 
    tp <- handlerToWidget $ runDB $ get404 tvid
    [whamlet| 
        #{tipoveiculoNome tp}
    |]

getResultadoR :: Text -> Handler Html
getResultadoR dado = do 
    msg <- getMessage
    dadocli <- runDB $ selectFirst ([ClienteCnh ==. dado] ||. [ClienteNome ==. dado])[] :: Handler (Maybe (Entity Cliente))
    dadovei <- runDB $ selectFirst [VeiculoPlaca ==. dado][] :: Handler (Maybe (Entity Veiculo))
    layoutTicket "ResultadoR"
        [whamlet|
            $maybe mensagem <- msg 
                <div id="snackbar"> #{mensagem}
                <script> myFunction();
                
            $maybe (Entity _ cdado) <- dadocli
                <p>
                    <b>Cliente
                    <p class="title-ticket">SOBRE
                    <p> Nome: #{clienteNome cdado}
                    <p> CNH:  #{clienteCnh cdado}
                    <p> Data de nascimento: #{(formatTime defaultTimeLocale " %Y-%m-%d " (clienteDtnascimento cdado))}
                    
                    <p class="title-ticket">CONTATO
                    <p> Telefone [1]: #{(clienteTelefone1 cdado)}
                    
                
            $maybe (Entity _ vdado) <- dadovei
                <p>
                    <b>Veículo
                    
                <p class="title-ticket">SOBRE
                <p> Tipo: ^{tipoveicById (veiculoTipoveic vdado)}
                <p> Placa: #{veiculoPlaca vdado}
                <p> UF Placa: #{veiculoUfplaca vdado}
                
                <p class="title-ticket">DETALHE
                <p> Modelo: #{(veiculoModelo vdado)}
                <p> Ano: #{(veiculoAno vdado)}
                <p> Cor: #{(veiculoCor vdado)}
        |]
