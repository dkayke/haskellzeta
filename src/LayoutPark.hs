{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module LayoutPark where

import Import

sndRote ::  String -> (Widget, String)
sndRote "CadClienteR"    = ([whamlet| @{CadastroR} |]  , "Cadastro de cliente")
sndRote "CadVeiculoR"    = ([whamlet| @{CadastroR} |]  , "Cadastro de veiculo")
sndRote "BuscaClienteR"  = ([whamlet| @{CadastroR} |]  , "Busca de cliente")
sndRote "BuscaVeiculoR"  = ([whamlet| @{CadastroR} |]  , "Busca de veiculo")
sndRote _ = ([whamlet| / |], "HaskPark")

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

bodySec' :: Widget -> String -> Widget
bodySec' section secroute =
    [whamlet|
        <div class="mdl-layout mdl-js-layout mdl-layout--fixed-header mdl-layout--fixed-tabs">
            <header class="mdl-layout__header">
                <div class="title mdl-color--light-blue-900">
                    <span class="mdl-layout-title">
                        <a href=^{fst (sndRote secroute)}>
                            <i class="material-icons">&#xE5C4;
                        <span class="mdl-layout-title">
                            <img src=@{StaticR img_logo_png}> HaskPark
                <div class="mdl-layout__tab-bar mdl-js-ripple-effect mdl-color--light-blue-800">
                    <span class="mdl-layout__tab is-active">#{snd (sndRote secroute)}
            <main class="mdl-layout__content page">
                <section class="mdl-layout__tab-panel is-active">
                    <div class="page-content" >
                        ^{section}
    |]
    
bodyLogin' :: Widget -> Widget
bodyLogin' section = 
    [whamlet|
        <div class="layout mdl-layout mdl-layout--fixed-header mdl-js-layout mdl-color--grey-100">
            <main class="main main-login mdl-layout__content">
                <div class="container container-login mdl-grid">
                    <div class="content content-login mdl-cell mdl-cell-login mdl-cell--12-col">
                        <div class="mdl-cell mdl-cell-login mdl-cell--12-col">
                            <span class="mdl-layout-title"><img src=@{StaticR img_logo_png}> HaskPark
                    
                    <div class="content content-login mdl-color--white mdl-shadow--4dp mdl-color-text--grey-800 mdl-cell mdl-cell-login mdl-cell--12-col">
                        <div class="mdl-cell mdl-cell-login mdl-cell--12-col">
                            <div class="mdl-tabs mdl-js-tabs mdl-js-ripple-effect">
                                
                                <div class="mdl-tabs__tab-bar">
                                    <span class="mdl-tabs__tab is-active">Login
                                    
                                <div class="mdl-tabs__panel is-active" id="login">
                                    ^{section}
                                    
                                <footer class="content content-login mdl-cell mdl-cell-login mdl-cell--12-col text-center">
                                    <div class="mdl-cell mdl-cell-login mdl-cell--12-col">HaskPark 2017 - Sobre nós 
    |]

layoutPark :: Widget -> Handler Html
layoutPark widget = defaultLayout $ do
    head' 
    body' widget 

layoutParkSec :: Widget -> String -> Handler Html
layoutParkSec widget backpage = defaultLayout $ do
    head' 
    bodySec' widget backpage

layoutLogin :: Widget -> Handler Html
layoutLogin widget = defaultLayout $ do
    toWidget $ [cassius|
        .layout
            background-size:cover;
            background-image: url( @{StaticR img_wallpaper_login_png} );
    |]
    head' 
    bodyLogin' widget