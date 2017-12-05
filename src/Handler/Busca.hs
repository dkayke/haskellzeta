{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Handler.Busca where

import Import
import LayoutPark

data Pesq = Pesq 
    {
        dadopesq :: Text
    }
    deriving Show

formBusca :: Form Pesq
formBusca = renderDivs $ Pesq
    <$> areq textField "CNH ou placa do veículo" Nothing

getBuscaR :: Handler Html
getBuscaR = do 
    (widget,enctype) <- generateFormPost formBusca
    msg <- getMessage
    layoutPark $ do 
        [whamlet|
            $maybe mensagem <- msg 
                    <div id="snackbar"> #{mensagem}
                    <script> myFunction();
                    
            <nav>
                <div class="breadcrumb">
                    <div class="col s12 mdl-color--grey-100">
                        <span>HaskPark /
                        <a>Busca
                
            <form action=@{BuscaR} enctype=#{enctype} method=post>
                ^{widget}
                <button type="submit" class="mdl-button mdl-button-login mdl-js-button mdl-button--raised">Pesquisar
        |]

postBuscaR ::  Handler Html
postBuscaR = do 
    ((res,_),_) <- runFormPost formBusca
    case res of
        FormSuccess dado -> do
            dadocli <- runDB $ selectFirst ([ClienteCnh ==. (dadopesq dado)] ||. [ClienteNome ==. (dadopesq dado)])[] :: Handler (Maybe (Entity Cliente))
            dadovei <- runDB $ selectFirst [VeiculoPlaca ==. (dadopesq dado)][] :: Handler (Maybe (Entity Veiculo))
            case dadocli of
                Nothing ->
                    case dadovei of
                    Nothing -> do
                        setMessage $ [shamlet| Pesquisa não retornou dados |]
                        redirect BuscaR
                    (Just _) -> do
                        redirect (ResultadoR (dadopesq dado))
                (Just _) -> do
                    redirect (ResultadoR (dadopesq dado))
        _ -> redirect BuscaR