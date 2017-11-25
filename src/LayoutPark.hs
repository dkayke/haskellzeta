{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module LayoutPark where

import Import
    
head' :: Widget
head' =  do
    setTitle "HaskPark"
    addStylesheet $ (StaticR css_material_min_css)
    addStylesheet $ (StaticR css_estilo_css)
    addScriptRemote "https://code.getmdl.io/1.3.0/material.min.js"
    toWidgetHead
        [hamlet|
            <meta charset="utf-8">
            <meta http-equiv="X-UA-Compatible" content="IE=edge">
            <meta name="description" content="A front-end template that helps you build fast, modern mobile web apps.">
            <meta name="viewport" content="width=device-width, initial-scale=1.0, minimum-scale=1.0">
            <meta name="mobile-web-app-capable" content="yes">
            <meta name="apple-mobile-web-app-capable" content="yes">
            <meta name="apple-mobile-web-app-status-bar-style" content="black">
            <meta name="apple-mobile-web-app-title" content="Material Design Lite">
            <meta name="msapplication-TileImage" content="images/touch/ms-touch-icon-144x144-precomposed.png">
            <meta name="msapplication-TileColor" content="#3372DF">
         |] 

body' ::  Widget -> Widget
body' section =
    [whamlet|
        <div class="mdl-layout mdl-js-layout mdl-layout--fixed-header mdl-layout--fixed-tabs">
            <header class="mdl-layout__header">
            
                <div class="title mdl-color--light-blue-900">
                    <span class="mdl-layout-title"><img src=@{StaticR img_logo_png}> HaskPark
                
                <div class="mdl-layout__tab-bar mdl-js-ripple-effect mdl-color--light-blue-800">
                    <a href=@{EntradaR}    class="mdl-layout__tab">Entrada
                    <a href=@{SaidaR}      class="mdl-layout__tab">Saída
                    <a href=@{CadastroR}   class="mdl-layout__tab">Cadastro
                    <a href=@{NegocioR}    class="mdl-layout__tab">Negócio
            <main class="mdl-layout__content page">
                <section class="mdl-layout__tab-panel is-active">
                    <div class="page-content">
                        ^{section}
    |]

layoutPark :: Widget -> Handler Html
layoutPark widget = defaultLayout $ do
    head' 
    body' widget