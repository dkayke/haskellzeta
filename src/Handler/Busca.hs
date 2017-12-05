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

postBuscaR :: Handler Html
postBuscaR = undefined 
